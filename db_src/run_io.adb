--
-- Created by ada_generator.py on 2012-02-12 15:05:10.219314
-- 
with Wsc_Db_Data;


with Ada.Containers.Vectors;
with Ada.Containers;

with Environment;

with DB_Commons; 
with DB_Commons.ODBC; 

with GNU.DB.SQLCLI;  
with GNU.DB.SQLCLI.Bind;
with GNU.DB.SQLCLI.Info;
with GNU.DB.SQLCLI.Environment_Attribute;
with GNU.DB.SQLCLI.Connection_Attribute;

with Ada.Exceptions;  
with Ada.Strings; 
with Ada.Strings.Wide_Fixed;
with Ada.Characters.Conversions;
with Ada.Strings.Unbounded; 
with Text_IO;
with Ada.Strings.Maps;
with Ada.Directories;
with Templates_Parser;

with Connection_Pool;

with Key_Value_Parameter_IO;
with Probit_Threshold_IO;
with Personal_Results_IO;
with State_IO;
with Disaggregated_Data_Table_IO;
with Uprate_Assumption_IO;
with Personal_Income_IO;
with Model.WSC.Global_Settings;

with Model.WSC.Run_Declarations;
with Model.WSC.Users;
with Text_Utils;
with Utils;
with GNATColl.Traces;

package body Run_IO is

   use Ada.Strings.Unbounded;
   use Ada.Exceptions;
   use Ada.Strings;
   use Text_Utils;
   use type Ada.Containers.Count_Type;  
   package dodbc renames DB_Commons.ODBC;
   package sql renames GNU.DB.SQLCLI;
   package sql_info renames GNU.DB.SQLCLI.Info;
   use sql;
   use Base_Types;
   
   --
   -- generic packages to handle each possible type of decimal, if any, go here
   --
   HIDE_THE_EVIDENCE : constant Boolean := False;   
   --
   -- Select all variables; substring to be competed with output from some criteria
   --
   SELECT_PART : constant String := "select " &
         "run_id, username, comparison_username, comparison_run_id, title, use_random_threshold, num_iterations, interest_rate_pct, real_terms, is_null_settings," &
         "working_root, users_directory, output_directory, dir_separator, session_id, dataset_name, default_run_dir_id, start_year, end_year, weighting_function," &
         "weighting_lower_bound, weighting_upper_bound, status, type_of_run, do_reweighting " &
         " from run " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into run (" &
         "run_id, username, comparison_username, comparison_run_id, title, use_random_threshold, num_iterations, interest_rate_pct, real_terms, is_null_settings," &
         "working_root, users_directory, output_directory, dir_separator, session_id, dataset_name, default_run_dir_id, start_year, end_year, weighting_function," &
         "weighting_lower_bound, weighting_upper_bound, status, type_of_run, do_reweighting " &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from run ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update run set  ";
   
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "RUN_IO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
   procedure Cleanup_Incomplete_Runs_For( username : Unbounded_String ) is
      userstr         : constant String := To_String( username ); 
      delete_no_params : constant String := 
         "delete from run where username='" & userstr & "' " & 
         " and run_id not in ( select distinct run_id  from key_value_parameter where username='" & userstr & "' )" ;
         
      delete_aborted   : constant String := 
         "delete from run where username='" & userstr & "' and run_id in " &
         "( select run_id from state where health in (1,2) and username='" & userstr & "') and " &
         " run_id < ( select max( run_id ) from run where username='" & userstr & "' )";

      ps              : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection      : dodbc.Database_Connection := Connection_Pool.Lease;
   begin
      Log( "executing " & delete_no_params );
      ps := dodbc.Initialise_Prepared_Statement( connection.connection, delete_no_params );       
      SQLExecute( ps );
      dodbc.Close_Prepared_Statement( ps );
      Log( "executing " & delete_aborted );
      ps := dodbc.Initialise_Prepared_Statement( connection.connection, delete_aborted );       
      SQLExecute( ps );
      dodbc.Close_Prepared_Statement( ps );
      -- FIXME add cleanup directory code here
      Connection_Pool.Return_Connection( connection );
   exception 
      when Error : others =>
         Raise_Exception( d.DB_Exception'Identity, 
            "Cleanup_Incomplete_Runs_Fo: exception thrown " & Exception_Information(Error) );
   end Cleanup_Incomplete_Runs_For;
   -- 
   -- Highest current runid, or 0 if none
   --
   function Highest_Run_Id( username : Unbounded_String ) return Integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      userstr : constant String := To_String( username );
      -- NOTE that we select from key_value_parameter to ensure we only select runs with parameters
      query : constant String := "select cast(max( run_id ) as integer ) as hrd from key_value_parameter where username='" & userstr & "'";
      ai : aliased Integer := 0;
      has_data : boolean := true; 
      output_len : aliased sql.SQLLEN := ai'Size/8;     
   begin      
      ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
      dodbc.I_Out_Binding.SQLBindCol( 
            StatementHandle  => ps, 
            ColumnNumber     => 1, 
            TargetValue      => ai'Access, 
            IndPtr           => output_len'Access );
      Log( dodbc.Describe_Column( ps, 1 ));
      SQLExecute( ps );
      loop
         dodbc.next_row( ps, has_data );
         Log( "after next row: Has data " & has_data'Img & "ai = " & ai'Img );
         if( not has_data ) then
            exit;
         end if;
         if( ai = Base_Types.MISSING_I_KEY ) then
            ai := 0;
         end if;        
      end loop;
      Log( "Highest_Run_Id; username |" & userstr & "| " & Integer'Image( ai ) & "query " & query );
      dodbc.Close_Prepared_Statement( ps );
      Connection_Pool.Return_Connection( connection );
      return Integer( ai );
   exception 
      when Error : others =>
         Raise_Exception( d.DB_Exception'Identity, 
            "next_free_user_id: exception thrown " & Exception_Information(Error) );
      return  Base_Types.MISSING_I_KEY;    
   end Highest_Run_Id;
   
   function Next_Free_Run_Id( username : Unbounded_String ) return Integer is
      hrd : Integer := Highest_Run_Id( username );
   begin
      if( hrd > 0 )then
         hrd := hrd + 1;
      end if;
      return  hrd;
   end Next_Free_Run_Id;
   
   -- function Get_Current_Run_For( username : Unbounded_String; status : Run_Status_Type; create_if_none : Boolean := False ) return Run is
      -- wsc_run : Run := NULL_RUN;
      -- -- hrd : Integer := Highest_Run_Id( username );
   -- begin
      -- Log( "Run_IO::Get_Current_Run_For; user is " & TS( username ) & " got hrd as " & hrd'Img );
      -- if( hrd = 0 ) and create_if_none then
         -- Log( "Get_Current_Run_For; AUTO Creating new run" );
         -- hrd := Highest_Run_Id( Model.WSC.Users.DEFAULT_USERNAME );
         -- wsc_run := Retrieve_By_PK( hrd, Model.WSC.Users.DEFAULT_USERNAME );
         -- wsc_run := Make_New_Copy( wsc_run, username ); 
      -- else
         -- case status is
            -- when edited | displayed | running_or_queued =>
               -- declare
                  -- c : d.Criteria;
                  -- l : Run_List.Vector;
               -- begin   
                  -- Add_Username( c, username );
                  -- Add_Status( c, status );
                  -- Add_Run_Id_To_Orderings( c, d.asc );
                  -- l := Retrieve( c );
                  -- if( l.Length > 0 )then
                     -- wsc_run := l.Element( Positive( l.Length ));
                  -- end if;
               -- end;
            -- when neither =>
               -- r := Retrieve_By_PK( hrd, username );
            -- end case;
      -- end if;
      -- Log( "got run id as " & wsc_run.run_id'Img );
      -- return r;
   -- end  Get_Current_Run_For;

   function Get_Current_Run_For( username : Unbounded_String; status : Run_Status_Type ) return Run is
      wsc_run : Run := NULL_RUN;
      c : d.Criteria;
      l : Run_List.Vector;
      -- hrd : Integer := Highest_Run_Id( username );
   begin
      Add_Username( c, username );
      Add_Status( c, status );
      Add_Run_Id_To_Orderings( c, d.asc );
      l := Retrieve( c );
      if( l.Length > 0 )then
         wsc_run := l.Element( Positive( l.Length ));
      end if;
      Log( "got run id as " & wsc_run.run_id'Img );
      return wsc_run;
   end  Get_Current_Run_For;

   function Get_New_Run_For( username : Unbounded_String; run_id : Natural := 0 ) return Run is
      r : Run;
      hrd : Integer;
   begin
      if( run_id = 0 )then
         hrd := Highest_Run_Id( username );
      else
         hrd := run_id;
      end if;
      Log( "Run_IO::Get_New_Run_For; user is " & TS( username ) & " got hrd as " & hrd'Img );
      if( hrd = 0 )then
         hrd := Highest_Run_Id( Model.WSC.Users.DEFAULT_USERNAME );
         r := Retrieve_By_PK( hrd, Model.WSC.Users.DEFAULT_USERNAME );         
      else
         r := Retrieve_By_PK( hrd, username );
         -- if( r.status = edited )then
         --    r.status := displayed;
         -- end if;
         -- Save( r );
      end if;
      r := Make_New_Copy( r, username );         
      return r;
   end  Get_New_Run_For;

   --
   -- returns true if the primary key parts of Run match the defaults in Wsc_Db_Data.Null_Run
   --
   --
   -- Does this Run equal the default Wsc_Db_Data.Null_Run ?
   --
   function Is_Null( r : Run ) return Boolean is
   begin
      return r = Null_Run;
   end Is_Null;
    use Keyed_Text_Buffer;
 
   procedure Bulk_Save_Parameters( r : Run; kvs : Keyed_Text_Buffer.Text_Buffer ) is   
   use Text_Utils;
   use Text_IO;
   use Text_Utils.String_Maps_Package;
      query         : Unbounded_String := TuS( "insert into key_value_parameter( username, run_id, key, val ) values ");
      size          : constant Natural := Natural( kvs.Length );
      n             : Natural := 1;
      c : Cursor    := kvs.First;
      f             : File_Type;
      ok            : Boolean;
      rstr          : constant String := Utils.Now_As_String;
      idstr         : constant String := "/" & r.run_id'Img( 2 .. r.run_id'Img'Length );
      sql_filename  : constant String := r.Qualified_Users_Directory & "/" & idstr & "/params/tmp_params_dump_" & rstr & ".sql"; 
      dump_filename : constant String := r.Qualified_Users_Directory & "/" & idstr & "/params/tmp_params_dump_" & rstr & ".dump"; 
   begin
      if( kvs.Length = 0 )then
         return;
      end if;
      loop            
         declare
            crit       : d.Criteria;
            k          : Unbounded_String := Key( c );  
            v          : Unbounded_String := Element( c ); 
         begin
            Add_Username( crit, r.username );
            Key_Value_Parameter_IO.Add_Run_Id( crit, r.run_id );
            Key_Value_Parameter_IO.Add_Key( crit, k );
            Key_Value_Parameter_IO.Add_Val( crit, v );
            query := query & "( "  & d.To_Crude_Array_Of_Values( crit ) & " )";
            if( n < size )then 
               query := query & ",";
            end if;
            Next( c );
         end;
         exit when n = size;
         n := n + 1;
      end loop;
      -- Log( "Run_IO::retrieve made this as query " & TS( query ));
      Create( f, Out_File, sql_filename );
      Put_Line( f, TS( query ));
      Close( f );
      ok := DB_Commons.Load_Postgres( sql_filename, dump_filename );
      if( ok and HIDE_THE_EVIDENCE )then
         Ada.Directories.Delete_File( sql_filename );
         Ada.Directories.Delete_File( dump_filename );         
      end if;
   end Bulk_Save_Parameters;


   --
   -- returns the single Run matching the primary key fields, or the Wsc_Db_Data.Null_Run record
   -- if no such record exists
   --
   function Retrieve_By_PK( Run_Id : integer; Username : Unbounded_String ) return Run is
      l : Run_List.Vector;
      r : Run;
      c : d.Criteria;
   begin      
      Add_Run_Id( c, Run_Id );
      Add_Username( c, Username );
      l := Retrieve( c );
      if( not Run_List.is_empty( l ) ) then
         r := Run_List.First_Element( l );
      else
         r := Null_Run;
      end if;
      return r;
   end Retrieve_By_PK;

   function Get_Canditate_Base_Runs_For( Username : Unbounded_String ) return Run_List.Vector is
      c : d.Criteria;
   begin      
      Add_Username( c, Model.WSC.Users.DEFAULT_USERNAME, d.eq );
      Add_Username( c, username, d.eq, d.join_or );
      Add_Username_To_Orderings( c, d.asc );
      Add_Run_Id_To_Orderings( c, d.desc );
      return Retrieve( c );
   end Get_Canditate_Base_Runs_For;
   -- 
   --
   -- Retrieves a list of Run matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Run_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;
   
   procedure Save_Arrays( r : Run ) is
      us      : Wsc_Db_Data.Uprate_Assumption;
      pt      : Wsc_Db_Data.Probit_Threshold;
   begin
      Log( "Run_IO::save_arrays; entered" );
      for i in Uprate_Targets loop
         us.percent_change := r.uprate_assumptions( i ).percent_change;
         us.use_obr := r.uprate_assumptions( i ).use_obr;
         us.element := r.uprate_assumptions( i ).element;
         us.username := r.username;
         us.run_id := r.run_id;
         us.target := i;
         Log( "Run_IO::save_arrays saving " & us.target'Img );
         Uprate_Assumption_IO.Save( us, True );
      end loop;
      for i in Probit_Threshold_Type loop
         pt.threshold := r.probit_thresholds( i );
         pt.username := r.username;
         pt.run_id := r.run_id;
         pt.element := i;
         Probit_Threshold_IO.Save( pt, True );
      end loop;
   end Save_Arrays;
   
   procedure Fill_In_Arrays( r : in out Run ) is
      probs   : Wsc_Db_Data.Probit_Threshold_List.Vector := Retrieve_Associated_Probit_Thresholds( r );
      n_probs : Natural := Natural( probs.Length );
      pt      : Wsc_Db_Data.Probit_Threshold;
      uprs    : Wsc_Db_Data.Uprate_Assumption_List.Vector := Retrieve_Associated_Uprate_Assumptions( r );
      n_uprs  : Natural := Natural( uprs.Length );
      us      : Wsc_Db_Data.Uprate_Assumption;
   begin
      r.probit_thresholds := ( others => 0.0 );
      r.uprate_assumptions := ( others => NULL_UPRATE_ASSUMPTION );
      for i in 1 .. n_probs loop
         pt := probs.Element( i );
         r.probit_thresholds( pt.element ) := pt.threshold;
      end loop;      
      for i in 1 .. n_uprs loop
         us := uprs.Element( i );
         r.uprate_assumptions( us.target ).percent_change := us.percent_change;
         r.uprate_assumptions( us.target ).use_obr := us.use_obr;
         r.uprate_assumptions( us.target ).element := us.element;
      end loop;      
   end Fill_In_Arrays;

   procedure Delete_Results_For( wsc_run : Run ) is
   use Templates_Parser;
      translations : Translate_Set;
      connection   : dodbc.Database_Connection := Connection_Pool.Lease;
      s            : Unbounded_String;
   begin
      insert( translations, Assoc( "USERNAME", wsc_run.username ));
      insert( translations, Assoc( "RUN-ID", wsc_run.run_id ));
      s := Templates_Parser.Parse( Model.WSC.Global_Settings.template_components_path & "delete_run_results.sql", translations );
      dodbc.Execute_Script( connection, TS( s ));
      Connection_Pool.Return_Connection( connection );
   end Delete_Results_For;
   
   function Make_New_Copy( run_to_copy : Run; new_username : Unbounded_String := Null_Unbounded_String ) return Run is
   use Templates_Parser;
      new_run      : Run := run_to_copy;
      translations : Translate_Set;
      connection   : dodbc.Database_Connection := Connection_Pool.Lease;
      s            : Unbounded_String;
   begin
      new_run.status := edited;
      Log( "Make_New_Copy entered; copying from " & TS( run_to_copy.username ) & " rid " & run_to_copy.run_id'Img & " to " & TS( new_username ));
      if( new_username /= Null_Unbounded_String )then
         new_run.username := new_username;
      end if;
      new_run.run_id := Next_Free_Run_Id( new_run.username );
      if( new_run.run_id <= 0 )then
         new_run.run_id := 1;
      end if;
      Save( run_to_save => new_run, overwrite=> False, save_associated_arrays=>False );
      Insert( translations, Assoc( "TO-USERNAME", new_run.username ));
      Insert( translations, Assoc( "TO-RUN-ID", new_run.run_id ));
      Insert( translations, Assoc( "FROM-USERNAME", run_to_copy.username ));
      Insert( translations, Assoc( "FROM-RUN-ID", run_to_copy.run_id ));
      s := Templates_Parser.Parse( Model.WSC.Global_Settings.template_components_path & "make_new_run_copy.sql", translations );
      dodbc.Execute_Script( connection, TS( s ));
      Connection_Pool.Return_Connection( connection );
      Fill_In_Arrays( new_run );
      return new_run; 
   end Make_New_Copy;
      
   function Has_Run_In_State( username : Unbounded_String; state : Run_Status_Type ) return Boolean is
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      ps         : SQLHDBC := SQL_NULL_HANDLE;
      has_data   : Boolean;
      n : Natural := 0;
      query : constant String := "select run_id from run where username ='" & TS( username ) & "' and status= " 
         & RUn_Status_Type'Pos( state )'Img;
   begin
      begin
         Log( "Has_Run_In_State; made query as " & query );
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            n := n + 1;
         end loop;   
      exception 
         when No_Data => null; 
         when Error : others =>
            Raise_Exception( d.DB_Exception'Identity, 
               "retrieve: exception " & Exception_Information(Error) );
      end; -- exception block
      begin
         dodbc.Close_Prepared_Statement( ps );
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            Raise_Exception( d.DB_Exception'Identity, 
               "retrieve: exception " & Exception_Information(Error) );
      end; -- exception block
      return n > 0;
   end Has_Run_In_State;
   

   --
   -- Retrieves a list of Run retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Run_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Run_List.Vector;
      ps : SQLHDBC := SQL_NULL_HANDLE;
      has_data : Boolean := false;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := SELECT_PART & " " & sqlstr;
      --
      -- aliased local versions of fields 
      --
      Run_Id: aliased integer;
      Username: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Comparison_Username: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Comparison_Run_Id: aliased integer;
      Title: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Use_Random_Threshold: aliased Integer;
      Num_Iterations: aliased integer;
      Interest_Rate_Pct: aliased Real;
      Real_Terms: aliased Integer;
      Is_Null_Settings: aliased Integer;
      Working_Root: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" &
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Users_Directory: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" &
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Output_Directory: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" &
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Dir_Separator: aliased String := 
            "@";
      Session_Id: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Dataset_Name: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Default_Run_Dir_Id: aliased integer;
      Start_Year: aliased integer;
      End_Year: aliased integer;
      Weighting_Function: aliased Integer;
      Weighting_Lower_Bound: aliased Real;
      Weighting_Upper_Bound: aliased Real;
      Status: aliased Integer;
      Type_Of_Run: aliased Integer;
      do_reweighting : aliased Integer;
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'access;
      dataset_name_access : String_Access := dataset_name'access;
      Comparison_Username_access : String_Access := Comparison_Username'access;
      Title_access : String_Access := Title'access;
      Interest_Rate_Pct_access : Real_Access := Interest_Rate_Pct'access;
      Working_Root_access : String_Access := Working_Root'access;
      Users_Directory_access : String_Access := Users_Directory'access;
      Output_Directory_access : String_Access := Output_Directory'access;
      Dir_Separator_access : String_Access := Dir_Separator'access;
      Session_Id_access : String_Access := Session_Id'access;
      Weighting_Lower_Bound_access : Real_Access := Weighting_Lower_Bound'access;
      Weighting_Upper_Bound_access : Real_Access := Weighting_Upper_Bound'access;
      --
      -- length holders for each retrieved variable
      --
      Run_Id_len : aliased SQLLEN:= Run_Id'Size/8;
      Username_len : aliased SQLLEN:= Username'Size/8;
      Comparison_Username_len : aliased SQLLEN:= Comparison_Username'Size/8;
      Comparison_Run_Id_len : aliased SQLLEN:= Comparison_Run_Id'Size/8;
      Title_len : aliased SQLLEN:= Title'Size/8;
      Use_Random_Threshold_len : aliased SQLLEN:= Use_Random_Threshold'Size/8;
      Num_Iterations_len : aliased SQLLEN:= Num_Iterations'Size/8;
      Interest_Rate_Pct_len : aliased SQLLEN:= Interest_Rate_Pct'Size/8;
      Real_Terms_len : aliased SQLLEN:= Real_Terms'Size/8;
      Is_Null_Settings_len : aliased SQLLEN:= Is_Null_Settings'Size/8;
      Working_Root_len : aliased SQLLEN:= Working_Root'Size/8;
      Users_Directory_len : aliased SQLLEN:= Users_Directory'Size/8;
      Output_Directory_len : aliased SQLLEN:= Output_Directory'Size/8;
      Dir_Separator_len : aliased SQLLEN:= Dir_Separator'Size/8;
      Session_Id_len : aliased SQLLEN:= Session_Id'Size/8;
      Dataset_Name_len : aliased SQLLEN:= Dataset_Name'Size/8;
      Default_Run_Dir_Id_len : aliased SQLLEN:= Default_Run_Dir_Id'Size/8;
      Start_Year_len : aliased SQLLEN:= Start_Year'Size/8;
      End_Year_len : aliased SQLLEN:= End_Year'Size/8;
      Weighting_Function_len : aliased SQLLEN:= Weighting_Function'Size/8;
      Weighting_Lower_Bound_len : aliased SQLLEN:= Weighting_Lower_Bound'Size/8;
      Weighting_Upper_Bound_len : aliased SQLLEN:= Weighting_Upper_Bound'Size/8;
      Status_len : aliased SQLLEN:= Status'Size/8;
      Type_Of_Run_len : aliased SQLLEN:= Type_Of_Run'Size/8;
      do_reweighting_len : aliased SQLLEN:= Do_Reweighting'Size/8;
      
      r : Run;
   begin
      Log( "Run_IO::retrieve made this as query " & query );
      begin -- exception block
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 1,
            TargetValue      => Run_Id'access,
            IndPtr           => Run_Id_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 2,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Username_access.all'address ),
            BufferLength     => Username_len,
            StrLen_Or_IndPtr => Username_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 3,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Comparison_Username_access.all'address ),
            BufferLength     => Comparison_Username_len,
            StrLen_Or_IndPtr => Comparison_Username_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetValue      => Comparison_Run_Id'access,
            IndPtr           => Comparison_Run_Id_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 5,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Title_access.all'address ),
            BufferLength     => Title_len,
            StrLen_Or_IndPtr => Title_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 6,
            TargetValue      => Use_Random_Threshold'access,
            IndPtr           => Use_Random_Threshold_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 7,
            TargetValue      => Num_Iterations'access,
            IndPtr           => Num_Iterations_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 8,
            TargetValuePtr   => To_SQLPOINTER( Interest_Rate_Pct_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Interest_Rate_Pct_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 9,
            TargetValue      => Real_Terms'access,
            IndPtr           => Real_Terms_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 10,
            TargetValue      => Is_Null_Settings'access,
            IndPtr           => Is_Null_Settings_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 11,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Working_Root_access.all'address ),
            BufferLength     => Working_Root_len,
            StrLen_Or_IndPtr => Working_Root_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 12,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Users_Directory_access.all'address ),
            BufferLength     => Users_Directory_len,
            StrLen_Or_IndPtr => Users_Directory_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 13,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Output_Directory_access.all'address ),
            BufferLength     => Output_Directory_len,
            StrLen_Or_IndPtr => Output_Directory_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 14,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Dir_Separator_access.all'address ),
            BufferLength     => Dir_Separator_len,
            StrLen_Or_IndPtr => Dir_Separator_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 15,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Session_Id_access.all'address ),
            BufferLength     => Session_Id_len,
            StrLen_Or_IndPtr => Session_Id_len'access );
            
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 16,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Dataset_name_access.all'address ),
            BufferLength     => Dataset_Name_len,
            StrLen_Or_IndPtr => Dataset_Name_len'access );
            
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 17,
            TargetValue      => Default_Run_Dir_Id'access,
            IndPtr           => Default_Run_Dir_Id_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 18,
            TargetValue      => Start_Year'access,
            IndPtr           => Start_Year_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 19,
            TargetValue      => End_Year'access,
            IndPtr           => End_Year_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 20,
            TargetValue      => Weighting_Function'access,
            IndPtr           => Weighting_Function_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 21,
            TargetValuePtr   => To_SQLPOINTER( Weighting_Lower_Bound_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Weighting_Lower_Bound_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 22,
            TargetValuePtr   => To_SQLPOINTER( Weighting_Upper_Bound_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Weighting_Upper_Bound_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 23,
            TargetValue      => status'access,
            IndPtr           => status_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 24,
            TargetValue      => Type_Of_Run'access,
            IndPtr           => Type_Of_Run_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 25,
            TargetValue      => Do_Reweighting'access,
            IndPtr           => Do_Reweighting_len'access );

         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Log( "Username_len=" & Username_len'Img & " username " & username & " actual length " & username'Length'Img );
            r.Run_Id := Run_Id;
            r.Username := Slice_To_Unbounded( Username, 1, Natural( Username_len ) );
            r.Comparison_Username := Slice_To_Unbounded( Comparison_Username, 1, Natural( Comparison_Username_len ) );
            r.Comparison_Run_Id := Comparison_Run_Id;
            r.Title := Slice_To_Unbounded( Title, 1, Natural( Title_len ) );
            r.Use_Random_Threshold := Boolean'Val( Use_Random_Threshold );
            r.Num_Iterations := Num_Iterations;
            r.Interest_Rate_Pct:= Real( Interest_Rate_Pct_access.all );
            r.Real_Terms := Boolean'Val( Real_Terms );
            r.Is_Null_Settings := Boolean'Val( Is_Null_Settings );
            r.Working_Root := Slice_To_Unbounded( Working_Root, 1, Natural( Working_Root_len ) );
            r.Users_Directory := Slice_To_Unbounded( Users_Directory, 1, Natural( Users_Directory_len ) );
            r.Output_Directory := Slice_To_Unbounded( Output_Directory, 1, Natural( Output_Directory_len ) );
            r.Dir_Separator := Slice_To_Unbounded( Dir_Separator, 1, Natural( Dir_Separator_len ) );
            r.Session_Id := Slice_To_Unbounded( Session_Id, 1, Natural( Session_Id_len ) );
            r.Dataset_Name := Slice_To_Unbounded( Dataset_Name, 1, Natural( dataset_name_len ));
            r.Default_Run_Dir_Id := Default_Run_Dir_Id;
            r.Start_Year := Start_Year;
            r.End_Year := End_Year;
            r.Weighting_Function := Distance_Function_Type'Val( Weighting_Function );
            r.Weighting_Lower_Bound:= Real( Weighting_Lower_Bound_access.all );
            r.Weighting_Upper_Bound:= Real( Weighting_Upper_Bound_access.all );
            r.status := Run_Status_Type'Val( status );
            r.Type_Of_Run := Run_Type'Val( type_of_run );
            r.do_reweighting := Boolean'Val( do_reweighting );
            Fill_In_Arrays( r );
            Run_List.append( l, r );        
         end loop;
         Log( "Run_IO::retrieve: Query Run OK" );
      exception 
         when No_Data => Null; 
         when Error : others =>
            Raise_Exception( d.DB_Exception'Identity, 
               "retrieve: exception " & Exception_Information(Error) );
      end; -- exception block
      begin
         dodbc.Close_Prepared_Statement( ps );
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            Raise_Exception( d.DB_Exception'Identity, 
               "retrieve: exception " & Exception_Information(Error) );
      end; -- exception block
      return l;
   end Retrieve;

   
   --
   -- Update the given record 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Update( run_to_update : Run; update_associated_arrays : Boolean := True ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Comparison_Username( values_c, run_to_update.Comparison_Username );
      Add_Comparison_Run_Id( values_c, run_to_update.Comparison_Run_Id );
      Add_Title( values_c, run_to_update.Title );
      Add_Use_Random_Threshold( values_c, run_to_update.Use_Random_Threshold );
      Add_Num_Iterations( values_c, run_to_update.Num_Iterations );
      Add_Interest_Rate_Pct( values_c, run_to_update.Interest_Rate_Pct );
      Add_Real_Terms( values_c, run_to_update.Real_Terms );
      Add_Is_Null_Settings( values_c, run_to_update.Is_Null_Settings );
      Add_Working_Root( values_c, run_to_update.Working_Root );
      Add_Users_Directory( values_c, run_to_update.Users_Directory );
      Add_Output_Directory( values_c, run_to_update.Output_Directory );
      Add_Dir_Separator( values_c, run_to_update.Dir_Separator );
      Add_Session_Id( values_c, run_to_update.Session_Id );
      Add_Dataset_Name( values_c, run_to_update.Dataset_Name );
      Add_Default_Run_Dir_Id( values_c, run_to_update.Default_Run_Dir_Id );
      Add_Start_Year( values_c, run_to_update.Start_Year );
      Add_End_Year( values_c, run_to_update.End_Year );
      Add_Weighting_Function( values_c, run_to_update.Weighting_Function );
      Add_Weighting_Lower_Bound( values_c, run_to_update.Weighting_Lower_Bound );
      Add_Weighting_Upper_Bound( values_c, run_to_update.Weighting_Upper_Bound );
      Add_Status( values_c, run_to_update.Status );
      Add_Type_Of_Run( values_c, run_to_update.Type_Of_Run );
      Add_Do_Reweighting( values_c, run_to_update.Do_Reweighting );
      --
      -- primary key fields
      --
      Add_Run_Id( pk_c, run_to_update.Run_Id );
      Add_Username( pk_c, run_to_update.Username );
      query := query & d.To_String( values_c, "," ) & d.to_string( pk_c );
      Log( "Run_IO::update; executing query" & To_String(query) );
      if update_associated_arrays then
         Save_Arrays( run_to_update );
      end if;
      begin -- exception block      
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         Log( "Run_IO::update; execute query OK" );
      exception 
         when No_Data => Null; -- ignore if no updates made
         when Error : others =>
            Log( "run_io.adb ERROR:update: failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "update: exception thrown " & Exception_Information(Error) );
      end; -- exception block
      dodbc.Close_Prepared_Statement( ps );
      Connection_Pool.Return_Connection( connection );
   end Update;
   
   procedure Set_Edited_Run_To_Highest( username : Unbounded_String ) is
   use Templates_Parser;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      s : Unbounded_String;
      translations   : Translate_Set;
   begin
      Insert( translations, Assoc( "USERNAME", username ));
      s := Templates_Parser.Parse( Model.WSC.Global_Settings.template_components_path & "set_default_edited_run_id.sql", translations );
      dodbc.Execute_Script( connection, TS( s ));
      Connection_Pool.Return_Connection( connection );
   end Set_Edited_Run_To_Highest;

   
   procedure Set_Highest_Run_As_Active_If_None_Active( Username : Unbounded_String ) is
   use Templates_Parser;
     connection    : dodbc.Database_Connection := Connection_Pool.Lease;
      s             : Unbounded_String;
      translations  : Translate_Set;
   begin
      Insert( translations, Assoc( "USERNAME", username ));
      s := Templates_Parser.Parse( Model.WSC.Global_Settings.template_components_path & "set_highest_run_as_active_if_none_active.sql", translations );
      dodbc.Execute_Script( connection, TS( s ));
      Connection_Pool.Return_Connection( connection );
   end Set_Highest_Run_As_Active_If_None_Active;
   

   
   function Get_Num_Runs( username : Unbounded_String ) return Natural is
   begin
      return 0;
   end Get_Num_Runs;

   
   procedure Clear_Status_Flag_For( username : Unbounded_String; status : Run_Status_Type ) is
      ps         : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query      : Unbounded_String := TuS( UPDATE_PART );
      values_c   : d.Criteria;
      select_c   : d.Criteria;
   begin
      Add_Status( values_c, neither );
      Add_Status( select_c, status );
      if( username /= NULL_UNBOUNDED_STRING )then
         Add_Username ( select_c, username );
      end if;
      query := query & d.To_String( values_c, "," ) & d.To_String( select_c );
      Log( "query " & TS( query ));
      begin -- exception block      
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         Log( "Run_IO::update; execute query OK" );
      exception 
         when No_Data => Null; -- ignore if no updates made
         when Error : others =>
            Log( "run_io.adb ERROR:update: failed with message " & Exception_Information( Error ));
            Raise_Exception( d.DB_Exception'Identity, 
               "update: exception thrown " & Exception_Information(Error) );
      end; -- exception block
      dodbc.Close_Prepared_Statement( ps );
      Connection_Pool.Return_Connection( connection );
   end Clear_Status_Flag_For;

   --
   -- Save the compelete given record 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( run_to_save : Run; overwrite : Boolean := True; save_associated_arrays : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      Run_Tmp : Run;
   begin
      if( overwrite ) then
         Run_Tmp := retrieve_By_PK( run_to_save.Run_Id, run_to_save.Username );
         if( not is_Null( Run_Tmp )) then
            Update( run_to_save, save_associated_arrays );
            return;
         end if;
      end if;
      Add_Run_Id( c, run_to_save.Run_Id );
      Add_Username( c, run_to_save.Username );
      Add_Comparison_Username( c, run_to_save.Comparison_Username );
      Add_Comparison_Run_Id( c, run_to_save.Comparison_Run_Id );
      Add_Title( c, run_to_save.Title );
      Add_Use_Random_Threshold( c, run_to_save.Use_Random_Threshold );
      Add_Num_Iterations( c, run_to_save.Num_Iterations );
      Add_Interest_Rate_Pct( c, run_to_save.Interest_Rate_Pct );
      Add_Real_Terms( c, run_to_save.Real_Terms );
      Add_Is_Null_Settings( c, run_to_save.Is_Null_Settings );
      Add_Working_Root( c, run_to_save.Working_Root );
      Add_Users_Directory( c, run_to_save.Users_Directory );
      Add_Output_Directory( c, run_to_save.Output_Directory );
      Add_Dir_Separator( c, run_to_save.Dir_Separator );
      Add_Session_Id( c, run_to_save.Session_Id );
      Add_Dataset_Name( c, run_to_save.Dataset_Name );
      Add_Default_Run_Dir_Id( c, run_to_save.Default_Run_Dir_Id );
      Add_Start_Year( c, run_to_save.Start_Year );
      Add_End_Year( c, run_to_save.End_Year );
      Add_Weighting_Function( c, run_to_save.Weighting_Function );
      Add_Weighting_Lower_Bound( c, run_to_save.Weighting_Lower_Bound );
      Add_Weighting_Upper_Bound( c, run_to_save.Weighting_Upper_Bound );
      Add_Status( c, run_to_save.Status );
      Add_Type_Of_Run( c, run_to_save.Type_Of_Run );
      Add_Do_Reweighting( c, run_to_save.Do_Reweighting );
      
      query := query & "( "  & d.To_Crude_Array_Of_Values( c ) & " )";
      Log( "Run_IO::save; executing query" & To_String( query ));
      begin
         connection := Connection_Pool.Lease;
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         if( save_associated_arrays )then
            Save_Arrays( run_to_save );
         end if;
         Log( "Run_IO::save; execute query OK" );
      exception 
         when Error : others =>
            Log( "run_io.adb ERROR:save; execute query failed with message " & Exception_Information( Error ));
            Raise_Exception( d.DB_Exception'Identity, 
               "save: exception thrown " & Exception_Information( Error ));
      end;
      begin
         dodbc.Close_Prepared_Statement( ps );
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            Log( "run_io.adb ERROR:save/close " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "save/close: exception " & Exception_Information(Error) );
      end;
   end Save;


   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Run
   --

   procedure Delete( r : in out Run ) is
         c : d.Criteria;
   begin  
      Add_Run_Id( c, r.Run_Id );
      Add_Username( c, r.Username );
      delete( c );
      r := Null_Run;
      Log( "Run_IO::delete record; execute query OK" );
   end Delete;


   --
   -- delete the records indentified by the criteria
   --
   procedure Delete( c : d.Criteria ) is
   begin      
      delete( d.to_string( c ) );
      Log( "Run_IO::delete criteria; execute query OK" );
   end Delete;
   
   procedure Delete( where_Clause : String ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := DELETE_PART & To_Unbounded_String(" ");
   begin
      query := query & where_Clause;
      begin -- try catch block for execute
         Log( "Run_IO::delete; executing query" & To_String(query) );
         connection := Connection_Pool.Lease;
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         Log( "Run_IO::delete; execute query OK" );
      exception 
         when Error : No_Data => Null; -- silently ignore no data exception, which is hardly exceptional
         when Error : others =>
            Log( "run_io.adb ERROR:delete; execute query failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "delete: exception thrown " & Exception_Information(Error) );
      end;
      begin -- try catch block for close
         dodbc.Close_Prepared_Statement( ps );
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            Log( "run_io.adb ERROR:delete; execute query failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "delete: exception thrown " & Exception_Information(Error) );
      end;
   end Delete;

   --
   -- functions to retrieve records from tables with foreign keys
   -- referencing the table modelled by this package
   --
   function Retrieve_Associated_Key_Value_Parameters( r : Run ) return Wsc_Db_Data.Key_Value_Parameter_List.Vector is
      c : d.Criteria;
   begin
      Key_Value_Parameter_IO.Add_Username( c, r.Username );
      Key_Value_Parameter_IO.Add_Run_Id( c, r.Run_Id );
      return Key_Value_Parameter_IO.retrieve( c );
   end Retrieve_Associated_Key_Value_Parameters;

   function Map_Associated_Params_To_Text_Buffer( r : Run ) return Keyed_Text_Buffer.Text_Buffer is
      use WSC_DB_Data;
      b : Keyed_Text_Buffer.Text_Buffer;
      v : Wsc_Db_Data.Key_Value_Parameter_List.Vector := Retrieve_Associated_Key_Value_Parameters( r );
      k : Key_Value_Parameter;
      n : Natural := Natural( v.Length );
   begin
      for i in 1 .. n loop
         k := v.Element( i );
         b.Insert( k.key, k.val );
      end loop;
      return b;
   end Map_Associated_Params_To_Text_Buffer;
   
   procedure Store_Params_From_Text_Buffer( r : Run; buff : Keyed_Text_Buffer.Text_Buffer ) is
      use WSC_DB_Data;
      use Keyed_Text_Buffer;
      use String_Maps_Package;
      kvp : Key_Value_Parameter;
      
      procedure Save_One( c : Cursor ) is
         k : Unbounded_String := Key( c );
         v : Unbounded_String := Element( c );
      begin         
         kvp.key := k;
         kvp.val := v;
         Key_Value_Parameter_IO.Save( kvp );
      end Save_One;

   begin
      kvp.username := r.username;
      kvp.run_id := r.run_id;
      Iterate( buff, Save_One'Access );
   end Store_Params_From_Text_Buffer;

   --
   -- functions to retrieve records from tables with foreign keys
   -- referencing the table modelled by this package
   --
   function Retrieve_Associated_Probit_Thresholds( r : Run ) return Wsc_Db_Data.Probit_Threshold_List.Vector is
      c : d.Criteria;
   begin
      Probit_Threshold_IO.Add_Username( c, r.Username );
      Probit_Threshold_IO.Add_Run_Id( c, r.Run_Id );
      return Probit_Threshold_IO.retrieve( c );
   end Retrieve_Associated_Probit_Thresholds;


   function Retrieve_Associated_Personal_Results( r : Run ) return Wsc_Db_Data.Personal_Results_List.Vector is
      c : d.Criteria;
   begin
      Personal_Results_IO.Add_Username( c, r.Username );
      Personal_Results_IO.Add_Run_Id( c, r.Run_Id );
      return Personal_Results_IO.retrieve( c );
   end Retrieve_Associated_Personal_Results;


   function Retrieve_Child_State( r : Run ) return Wsc_Db_Data.State is
   begin
      return State_IO.retrieve_By_PK( r.Username, r.Run_Id );
   end Retrieve_Child_State;
   
   procedure Delete_State_For_Run( r : Run ) is
      c : d.Criteria;
   begin
      State_IO.Add_Username( c, r.username );
      State_IO.Add_Run_Id( c, r.run_id );
      State_IO.Delete( c );
   end Delete_State_For_Run;
   
   
   function Retrieve_State_For_Run( r : Run ) return Wsc_Db_Data.State is
      s : Wsc_Db_Data.State;
   begin
      s := Retrieve_Child_State( r );
      if( s.run_id /= r.run_id )then
         s := Model.Run_Settings.BLANK_STATE_TYPE;
         s.run_id := r.run_id;
         s.username := r.username;
         State_IO.Save( s );        
      end if;
      return s;
   end Retrieve_State_For_Run;


   function Retrieve_Associated_Disaggregated_Data_Tables( r : Run ) return Wsc_Db_Data.Disaggregated_Data_Table_List.Vector is
      c : d.Criteria;
   begin
      Disaggregated_Data_Table_IO.Add_Run_Id( c, r.Run_Id );
      Disaggregated_Data_Table_IO.Add_Username( c, r.Username );
      return Disaggregated_Data_Table_IO.retrieve( c );
   end Retrieve_Associated_Disaggregated_Data_Tables;


   function Retrieve_Associated_Uprate_Assumptions( r : Run ) return Wsc_Db_Data.Uprate_Assumption_List.Vector is
      c : d.Criteria;
   begin
      Uprate_Assumption_IO.Add_Username( c, r.Username );
      Uprate_Assumption_IO.Add_Run_Id( c, r.Run_Id );
      return Uprate_Assumption_IO.retrieve( c );
   end Retrieve_Associated_Uprate_Assumptions;


   function Retrieve_Associated_Personal_Incomes( r : Run ) return Wsc_Db_Data.Personal_Income_List.Vector is
      c : d.Criteria;
   begin
      Personal_Income_IO.Add_Username( c, r.Username );
      Personal_Income_IO.Add_Run_Id( c, r.Run_Id );
      return Personal_Income_IO.retrieve( c );
   end Retrieve_Associated_Personal_Incomes;



   --
   -- functions to add something to a criteria
   --
   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "run_id", op, join, Run_Id );
   begin
      d.add_to_criteria( c, elem );
   end Add_Run_Id;


   procedure Add_Username( c : in out d.Criteria; Username : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "username", op, join, To_String( Username ), 32 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Username;


   procedure Add_Username( c : in out d.Criteria; Username : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "username", op, join, Username, 32 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Username;


   procedure Add_Comparison_Username( c : in out d.Criteria; Comparison_Username : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "comparison_username", op, join, To_String( Comparison_Username ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Comparison_Username;


   procedure Add_Comparison_Username( c : in out d.Criteria; Comparison_Username : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "comparison_username", op, join, Comparison_Username, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Comparison_Username;


   procedure Add_Comparison_Run_Id( c : in out d.Criteria; Comparison_Run_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "comparison_run_id", op, join, Comparison_Run_Id );
   begin
      d.add_to_criteria( c, elem );
   end Add_Comparison_Run_Id;


   procedure Add_Title( c : in out d.Criteria; Title : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "title", op, join, To_String( Title ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Title;


   procedure Add_Title( c : in out d.Criteria; Title : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "title", op, join, Title, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Title;


   procedure Add_Use_Random_Threshold( c : in out d.Criteria; Use_Random_Threshold : boolean; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "use_random_threshold", op, join, Integer( boolean'Pos( Use_Random_Threshold )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Use_Random_Threshold;


   procedure Add_Num_Iterations( c : in out d.Criteria; Num_Iterations : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "num_iterations", op, join, Num_Iterations );
   begin
      d.add_to_criteria( c, elem );
   end Add_Num_Iterations;


   procedure Add_Interest_Rate_Pct( c : in out d.Criteria; Interest_Rate_Pct : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "interest_rate_pct", op, join, Interest_Rate_Pct );
   begin
      d.add_to_criteria( c, elem );
   end Add_Interest_Rate_Pct;


   procedure Add_Real_Terms( c : in out d.Criteria; Real_Terms : boolean; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "real_terms", op, join, Integer( boolean'Pos( Real_Terms )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Real_Terms;


   procedure Add_Is_Null_Settings( c : in out d.Criteria; Is_Null_Settings : boolean; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "is_null_settings", op, join, Integer( boolean'Pos( Is_Null_Settings )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Is_Null_Settings;


   procedure Add_Working_Root( c : in out d.Criteria; Working_Root : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "working_root", op, join, To_String( Working_Root ), 120 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Working_Root;


   procedure Add_Working_Root( c : in out d.Criteria; Working_Root : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "working_root", op, join, Working_Root, 120 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Working_Root;


   procedure Add_Users_Directory( c : in out d.Criteria; Users_Directory : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "users_directory", op, join, To_String( Users_Directory ), 120 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Users_Directory;


   procedure Add_Users_Directory( c : in out d.Criteria; Users_Directory : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "users_directory", op, join, Users_Directory, 120 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Users_Directory;


   procedure Add_Output_Directory( c : in out d.Criteria; Output_Directory : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "output_directory", op, join, To_String( Output_Directory ), 120 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Output_Directory;


   procedure Add_Output_Directory( c : in out d.Criteria; Output_Directory : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "output_directory", op, join, Output_Directory, 120 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Output_Directory;


   procedure Add_Dir_Separator( c : in out d.Criteria; Dir_Separator : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dir_separator", op, join, To_String( Dir_Separator ), 1 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dir_Separator;


   procedure Add_Dir_Separator( c : in out d.Criteria; Dir_Separator : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dir_separator", op, join, Dir_Separator, 1 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dir_Separator;


   procedure Add_Session_Id( c : in out d.Criteria; Session_Id : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "session_id", op, join, To_String( Session_Id ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Session_Id;


   procedure Add_Session_Id( c : in out d.Criteria; Session_Id : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "session_id", op, join, Session_Id, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Session_Id;


   procedure Add_Dataset_Name( c : in out d.Criteria; Dataset_Name : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dataset_name", op, join, To_String( Dataset_Name ));
   begin
      d.add_to_criteria( c, elem );
   end Add_Dataset_Name;

   procedure Add_Dataset_Name( c : in out d.Criteria; Dataset_Name : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dataset_name", op, join, Dataset_Name );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dataset_Name;


   procedure Add_Default_Run_Dir_Id( c : in out d.Criteria; Default_Run_Dir_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "default_run_dir_id", op, join, Default_Run_Dir_Id );
   begin
      d.add_to_criteria( c, elem );
   end Add_Default_Run_Dir_Id;


   procedure Add_Start_Year( c : in out d.Criteria; Start_Year : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "start_year", op, join, Start_Year );
   begin
      d.add_to_criteria( c, elem );
   end Add_Start_Year;


   procedure Add_End_Year( c : in out d.Criteria; End_Year : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "end_year", op, join, End_Year );
   begin
      d.add_to_criteria( c, elem );
   end Add_End_Year;


   procedure Add_Weighting_Function( c : in out d.Criteria; Weighting_Function : Distance_Function_Type; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "weighting_function", op, join, Integer( Distance_Function_Type'Pos( Weighting_Function )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Weighting_Function;


   procedure Add_Weighting_Lower_Bound( c : in out d.Criteria; Weighting_Lower_Bound : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "weighting_lower_bound", op, join, Weighting_Lower_Bound );
   begin
      d.add_to_criteria( c, elem );
   end Add_Weighting_Lower_Bound;


   procedure Add_Weighting_Upper_Bound( c : in out d.Criteria; Weighting_Upper_Bound : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "weighting_upper_bound", op, join, Weighting_Upper_Bound );
   begin
      d.add_to_criteria( c, elem );
   end Add_Weighting_Upper_Bound;

   procedure Add_Status( c : in out d.Criteria; status : Run_Status_Type; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "status", op, join, Integer( Run_Status_Type'Pos( Status )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Status;

   procedure Add_Type_Of_Run( c : in out d.Criteria; Type_Of_Run : Run_Type; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "type_of_run", op, join, Integer( Run_Type'Pos( Type_Of_Run )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Type_Of_Run;

   procedure Add_Do_Reweighting( c : in out d.Criteria; Do_Reweighting : Boolean; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "Do_Reweighting", op, join, Integer( Boolean'Pos( Do_Reweighting )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Do_Reweighting;
   
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "run_id", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Run_Id_To_Orderings;


   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "username", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Username_To_Orderings;


   procedure Add_Comparison_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "comparison_username", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Comparison_Username_To_Orderings;


   procedure Add_Comparison_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "comparison_run_id", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Comparison_Run_Id_To_Orderings;


   procedure Add_Title_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "title", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Title_To_Orderings;


   procedure Add_Use_Random_Threshold_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "use_random_threshold", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Use_Random_Threshold_To_Orderings;


   procedure Add_Num_Iterations_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "num_iterations", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Num_Iterations_To_Orderings;


   procedure Add_Interest_Rate_Pct_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "interest_rate_pct", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Interest_Rate_Pct_To_Orderings;


   procedure Add_Real_Terms_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "real_terms", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Real_Terms_To_Orderings;


   procedure Add_Is_Null_Settings_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "is_null_settings", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Is_Null_Settings_To_Orderings;


   procedure Add_Working_Root_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "working_root", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Working_Root_To_Orderings;


   procedure Add_Users_Directory_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "users_directory", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Users_Directory_To_Orderings;


   procedure Add_Output_Directory_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "output_directory", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Output_Directory_To_Orderings;


   procedure Add_Dir_Separator_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dir_separator", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dir_Separator_To_Orderings;


   procedure Add_Session_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "session_id", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Session_Id_To_Orderings;


   procedure Add_Dataset_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dataset_name", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dataset_Name_To_Orderings;


   procedure Add_Default_Run_Dir_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "default_run_dir_id", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Default_Run_Dir_Id_To_Orderings;


   procedure Add_Start_Year_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "start_year", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Start_Year_To_Orderings;


   procedure Add_End_Year_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "end_year", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_End_Year_To_Orderings;


   procedure Add_Weighting_Function_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "weighting_function", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Weighting_Function_To_Orderings;


   procedure Add_Weighting_Lower_Bound_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "weighting_lower_bound", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Weighting_Lower_Bound_To_Orderings;


   procedure Add_Weighting_Upper_Bound_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "weighting_upper_bound", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Weighting_Upper_Bound_To_Orderings;

   procedure Add_Type_Of_Run_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "type_of_run", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Type_Of_Run_To_Orderings;

   procedure Add_Do_Reweighting_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "Do_Reweighting", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Do_Reweighting_To_Orderings;


   -- 
   -- procedure Bulk_Store_Associated_Key_Value_Parameters( r : Run; buff : Keyed_Text_Buffer.Text_Buffer ) is
      -- use Ada.Text_IO;
      -- f : File_Type;
   -- begin
      -- 
      -- Close( f );
   -- end Bulk_Store_Associated_Key_Value_Parameters;
   -- 
end Run_IO;
