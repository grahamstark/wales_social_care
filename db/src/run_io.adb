--
-- Created by ada_generator.py on 2012-07-24 19:03:55.690351
-- 
with Wsc_Db_Data;


with Ada.Containers.Vectors;

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
with Connection_Pool;

with Uap_Threshold_IO;
with Key_Value_Parameter_IO;
with Probit_Threshold_IO;
with Personal_Results_IO;
with State_IO;
with Disaggregated_Data_Table_IO;
with Uprate_Assumption_IO;
with Personal_Income_IO;

with DB_Logger;

package body Run_IO is

   use Ada.Strings.Unbounded;
   use Ada.Exceptions;
   use Ada.Strings;

   package dodbc renames DB_Commons.ODBC;
   package sql renames GNU.DB.SQLCLI;
   package sql_info renames GNU.DB.SQLCLI.Info;
   use SQL;
   use Base_Types;
   --
   -- generic packages to handle each possible type of decimal, if any, go here
   --
   
   --
   -- Select all variables; substring to be competed with output from some criteria
   --
   SELECT_PART : constant String := "select " &
         "run_id, username, comparison_username, comparison_run_id, title, use_random_threshold, num_iterations, interest_rate_pct, real_terms, is_null_settings," &
         "working_root, users_directory, output_directory, dir_separator, session_id, dataset_name, default_run_dir_id, start_year, end_year, weighting_function," &
         "weighting_lower_bound, weighting_upper_bound, status, type_of_run " &
         " from run " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into run (" &
         "run_id, username, comparison_username, comparison_run_id, title, use_random_threshold, num_iterations, interest_rate_pct, real_terms, is_null_settings," &
         "working_root, users_directory, output_directory, dir_separator, session_id, dataset_name, default_run_dir_id, start_year, end_year, weighting_function," &
         "weighting_lower_bound, weighting_upper_bound, status, type_of_run " &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from run ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update run set  ";
   
   
   -- 
   -- Next highest avaiable value of Run_Id - useful for saving  
   --
   function Next_Free_Run_Id return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( run_id ) from run";
      ai : aliased integer;
      has_data : boolean := true; 
      output_len : aliased sql.SQLINTEGER;     
   begin
      ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
      dodbc.I_Out_Binding.SQLBindCol( 
            StatementHandle  => ps, 
            ColumnNumber     => 1, 
            TargetValue      => ai'access, 
            IndPtr           => output_len'Access );
      SQLExecute( ps );
      loop
         dodbc.next_row( ps, has_data );
         if( not has_data ) then
            exit;
         end if;
         if( ai = Base_Types.MISSING_I_KEY ) then
            ai := 1;
         else
            ai := ai + 1;
         end if;        
      end loop;
      dodbc.Close_Prepared_Statement( ps );
      Connection_Pool.Return_Connection( connection );
      return ai;
   exception 
      when Error : others =>
         Raise_Exception( d.DB_Exception'Identity, 
            "next_free_user_id: exception thrown " & Exception_Information(Error) );
      return  Base_Types.MISSING_I_KEY;    
   end Next_Free_Run_Id;



   --
   -- returns true if the primary key parts of Run match the defaults in Wsc_Db_Data.Null_Run
   --
   --
   -- Does this Run equal the default Wsc_Db_Data.Null_Run ?
   --
   function Is_Null( Run : Wsc_Db_Data.Run ) return Boolean is
   use Wsc_Db_Data;
   begin
      return Run = Wsc_Db_Data.Null_Run;
   end Is_Null;


   
   --
   -- returns the single Run matching the primary key fields, or the Wsc_Db_Data.Null_Run record
   -- if no such record exists
   --
   function Retrieve_By_PK( Run_Id : integer; Username : Unbounded_String ) return Wsc_Db_Data.Run is
      l : Wsc_Db_Data.Run_List.Vector;
      Run : Wsc_Db_Data.Run;
      c : d.Criteria;
   begin      
      Add_Run_Id( c, Run_Id );
      Add_Username( c, Username );
      l := Retrieve( c );
      if( not Wsc_Db_Data.Run_List.is_empty( l ) ) then
         Run := Wsc_Db_Data.Run_List.First_Element( l );
      else
         Run := Wsc_Db_Data.Null_Run;
      end if;
      return Run;
   end Retrieve_By_PK;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Run matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Run_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Run retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Run_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.Run_List.Vector;
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
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'Access;
      Comparison_Username_access : String_Access := Comparison_Username'Access;
      Title_access : String_Access := Title'Access;
      Interest_Rate_Pct_access : Real_Access := Interest_Rate_Pct'Access;
      Working_Root_access : String_Access := Working_Root'Access;
      Users_Directory_access : String_Access := Users_Directory'Access;
      Output_Directory_access : String_Access := Output_Directory'Access;
      Dir_Separator_access : String_Access := Dir_Separator'Access;
      Session_Id_access : String_Access := Session_Id'Access;
      Dataset_Name_access : String_Access := Dataset_Name'Access;
      Weighting_Lower_Bound_access : Real_Access := Weighting_Lower_Bound'Access;
      Weighting_Upper_Bound_access : Real_Access := Weighting_Upper_Bound'Access;
      --
      -- length holders for each retrieved variable
      --
      Run_Id_len : aliased SQLINTEGER := Run_Id'Size/8;
      Username_len : aliased SQLINTEGER := Username'Size/8;
      Comparison_Username_len : aliased SQLINTEGER := Comparison_Username'Size/8;
      Comparison_Run_Id_len : aliased SQLINTEGER := Comparison_Run_Id'Size/8;
      Title_len : aliased SQLINTEGER := Title'Size/8;
      Use_Random_Threshold_len : aliased SQLINTEGER := Use_Random_Threshold'Size/8;
      Num_Iterations_len : aliased SQLINTEGER := Num_Iterations'Size/8;
      Interest_Rate_Pct_len : aliased SQLINTEGER := Interest_Rate_Pct'Size/8;
      Real_Terms_len : aliased SQLINTEGER := Real_Terms'Size/8;
      Is_Null_Settings_len : aliased SQLINTEGER := Is_Null_Settings'Size/8;
      Working_Root_len : aliased SQLINTEGER := Working_Root'Size/8;
      Users_Directory_len : aliased SQLINTEGER := Users_Directory'Size/8;
      Output_Directory_len : aliased SQLINTEGER := Output_Directory'Size/8;
      Dir_Separator_len : aliased SQLINTEGER := Dir_Separator'Size/8;
      Session_Id_len : aliased SQLINTEGER := Session_Id'Size/8;
      Dataset_Name_len : aliased SQLINTEGER := Dataset_Name'Size/8;
      Default_Run_Dir_Id_len : aliased SQLINTEGER := Default_Run_Dir_Id'Size/8;
      Start_Year_len : aliased SQLINTEGER := Start_Year'Size/8;
      End_Year_len : aliased SQLINTEGER := End_Year'Size/8;
      Weighting_Function_len : aliased SQLINTEGER := Weighting_Function'Size/8;
      Weighting_Lower_Bound_len : aliased SQLINTEGER := Weighting_Lower_Bound'Size/8;
      Weighting_Upper_Bound_len : aliased SQLINTEGER := Weighting_Upper_Bound'Size/8;
      Status_len : aliased SQLINTEGER := Status'Size/8;
      Type_Of_Run_len : aliased SQLINTEGER := Type_Of_Run'Size/8;
      Run : Wsc_Db_Data.Run;
   begin
      DB_Logger.info( "retrieve made this as query " & query );
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
            TargetValuePtr   => To_SQLPOINTER( Username_access.all'Address ),
            BufferLength     => Username_len,
            StrLen_Or_IndPtr => Username_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 3,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Comparison_Username_access.all'Address ),
            BufferLength     => Comparison_Username_len,
            StrLen_Or_IndPtr => Comparison_Username_len'Access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetValue      => Comparison_Run_Id'access,
            IndPtr           => Comparison_Run_Id_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 5,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Title_access.all'Address ),
            BufferLength     => Title_len,
            StrLen_Or_IndPtr => Title_len'Access );
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
            TargetValuePtr   => To_SQLPOINTER( Interest_Rate_Pct_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Interest_Rate_Pct_len'Access );
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
            TargetValuePtr   => To_SQLPOINTER( Working_Root_access.all'Address ),
            BufferLength     => Working_Root_len,
            StrLen_Or_IndPtr => Working_Root_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 12,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Users_Directory_access.all'Address ),
            BufferLength     => Users_Directory_len,
            StrLen_Or_IndPtr => Users_Directory_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 13,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Output_Directory_access.all'Address ),
            BufferLength     => Output_Directory_len,
            StrLen_Or_IndPtr => Output_Directory_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 14,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Dir_Separator_access.all'Address ),
            BufferLength     => Dir_Separator_len,
            StrLen_Or_IndPtr => Dir_Separator_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 15,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Session_Id_access.all'Address ),
            BufferLength     => Session_Id_len,
            StrLen_Or_IndPtr => Session_Id_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 16,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Dataset_Name_access.all'Address ),
            BufferLength     => Dataset_Name_len,
            StrLen_Or_IndPtr => Dataset_Name_len'Access );
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
            TargetValuePtr   => To_SQLPOINTER( Weighting_Lower_Bound_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Weighting_Lower_Bound_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 22,
            TargetValuePtr   => To_SQLPOINTER( Weighting_Upper_Bound_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Weighting_Upper_Bound_len'Access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 23,
            TargetValue      => Status'access,
            IndPtr           => Status_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 24,
            TargetValue      => Type_Of_Run'access,
            IndPtr           => Type_Of_Run_len'access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Run.Run_Id := Run_Id;
            Run.Username := Slice_To_Unbounded( Username, 1, Natural( Username_len ) );
            Run.Comparison_Username := Slice_To_Unbounded( Comparison_Username, 1, Natural( Comparison_Username_len ) );
            Run.Comparison_Run_Id := Comparison_Run_Id;
            Run.Title := Slice_To_Unbounded( Title, 1, Natural( Title_len ) );
            Run.Use_Random_Threshold := Boolean'Val( Use_Random_Threshold );
            Run.Num_Iterations := Num_Iterations;
            Run.Interest_Rate_Pct:= Real( Interest_Rate_Pct_access.all );
            Run.Real_Terms := Boolean'Val( Real_Terms );
            Run.Is_Null_Settings := Boolean'Val( Is_Null_Settings );
            Run.Working_Root := Slice_To_Unbounded( Working_Root, 1, Natural( Working_Root_len ) );
            Run.Users_Directory := Slice_To_Unbounded( Users_Directory, 1, Natural( Users_Directory_len ) );
            Run.Output_Directory := Slice_To_Unbounded( Output_Directory, 1, Natural( Output_Directory_len ) );
            Run.Dir_Separator := Slice_To_Unbounded( Dir_Separator, 1, Natural( Dir_Separator_len ) );
            Run.Session_Id := Slice_To_Unbounded( Session_Id, 1, Natural( Session_Id_len ) );
            Run.Dataset_Name := Slice_To_Unbounded( Dataset_Name, 1, Natural( Dataset_Name_len ) );
            Run.Default_Run_Dir_Id := Default_Run_Dir_Id;
            Run.Start_Year := Start_Year;
            Run.End_Year := End_Year;
            Run.Weighting_Function := run_weighting_function_Enum'Val( Weighting_Function );
            Run.Weighting_Lower_Bound:= Real( Weighting_Lower_Bound_access.all );
            Run.Weighting_Upper_Bound:= Real( Weighting_Upper_Bound_access.all );
            Run.Status := run_status_Enum'Val( Status );
            Run.Type_Of_Run := run_type_of_run_Enum'Val( Type_Of_Run );
            Wsc_Db_Data.Run_List.append( l, Run );        
         end loop;
         DB_Logger.info( "retrieve: Query Run OK" );
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
   procedure Update( Run : Wsc_Db_Data.Run ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Comparison_Username( values_c, Run.Comparison_Username );
      Add_Comparison_Run_Id( values_c, Run.Comparison_Run_Id );
      Add_Title( values_c, Run.Title );
      Add_Use_Random_Threshold( values_c, Run.Use_Random_Threshold );
      Add_Num_Iterations( values_c, Run.Num_Iterations );
      Add_Interest_Rate_Pct( values_c, Run.Interest_Rate_Pct );
      Add_Real_Terms( values_c, Run.Real_Terms );
      Add_Is_Null_Settings( values_c, Run.Is_Null_Settings );
      Add_Working_Root( values_c, Run.Working_Root );
      Add_Users_Directory( values_c, Run.Users_Directory );
      Add_Output_Directory( values_c, Run.Output_Directory );
      Add_Dir_Separator( values_c, Run.Dir_Separator );
      Add_Session_Id( values_c, Run.Session_Id );
      Add_Dataset_Name( values_c, Run.Dataset_Name );
      Add_Default_Run_Dir_Id( values_c, Run.Default_Run_Dir_Id );
      Add_Start_Year( values_c, Run.Start_Year );
      Add_End_Year( values_c, Run.End_Year );
      Add_Weighting_Function( values_c, Run.Weighting_Function );
      Add_Weighting_Lower_Bound( values_c, Run.Weighting_Lower_Bound );
      Add_Weighting_Upper_Bound( values_c, Run.Weighting_Upper_Bound );
      Add_Status( values_c, Run.Status );
      Add_Type_Of_Run( values_c, Run.Type_Of_Run );
      --
      -- primary key fields
      --
      Add_Run_Id( pk_c, Run.Run_Id );
      Add_Username( pk_c, Run.Username );
      query := query & d.To_String( values_c, "," ) & d.to_string( pk_c );
      DB_Logger.info( "update; executing query" & To_String(query) );
      begin -- exception block      
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         DB_Logger.info( "update; execute query OK" );
      exception 
         when No_Data => Null; -- ignore if no updates made
         when Error : others =>
            DB_Logger.error( "update: failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "update: exception thrown " & Exception_Information(Error) );
      end; -- exception block
      dodbc.Close_Prepared_Statement( ps );
      Connection_Pool.Return_Connection( connection );
   end Update;


   --
   -- Save the compelete given record 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Run : Wsc_Db_Data.Run; overwrite : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      Run_Tmp : Wsc_Db_Data.Run;
   begin
      if( overwrite ) then
         Run_Tmp := retrieve_By_PK( Run.Run_Id, Run.Username );
         if( not is_Null( Run_Tmp )) then
            Update( Run );
            return;
         end if;
      end if;
      Add_Run_Id( c, Run.Run_Id );
      Add_Username( c, Run.Username );
      Add_Comparison_Username( c, Run.Comparison_Username );
      Add_Comparison_Run_Id( c, Run.Comparison_Run_Id );
      Add_Title( c, Run.Title );
      Add_Use_Random_Threshold( c, Run.Use_Random_Threshold );
      Add_Num_Iterations( c, Run.Num_Iterations );
      Add_Interest_Rate_Pct( c, Run.Interest_Rate_Pct );
      Add_Real_Terms( c, Run.Real_Terms );
      Add_Is_Null_Settings( c, Run.Is_Null_Settings );
      Add_Working_Root( c, Run.Working_Root );
      Add_Users_Directory( c, Run.Users_Directory );
      Add_Output_Directory( c, Run.Output_Directory );
      Add_Dir_Separator( c, Run.Dir_Separator );
      Add_Session_Id( c, Run.Session_Id );
      Add_Dataset_Name( c, Run.Dataset_Name );
      Add_Default_Run_Dir_Id( c, Run.Default_Run_Dir_Id );
      Add_Start_Year( c, Run.Start_Year );
      Add_End_Year( c, Run.End_Year );
      Add_Weighting_Function( c, Run.Weighting_Function );
      Add_Weighting_Lower_Bound( c, Run.Weighting_Lower_Bound );
      Add_Weighting_Upper_Bound( c, Run.Weighting_Upper_Bound );
      Add_Status( c, Run.Status );
      Add_Type_Of_Run( c, Run.Type_Of_Run );
      query := query & "( "  & d.To_Crude_Array_Of_Values( c ) & " )";
      DB_Logger.info( "save; executing query" & To_String(query) );
      begin
         connection := Connection_Pool.Lease;
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         DB_Logger.info( "save; execute query OK" );
         
      exception 
         when Error : others =>
            DB_Logger.error( "save; execute query failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "save: exception thrown " & Exception_Information(Error) );
      end;
      begin
         dodbc.Close_Prepared_Statement( ps );
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            DB_Logger.error( "save/close " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "save/close: exception " & Exception_Information(Error) );
      end;
      
   end Save;


   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Run
   --

   procedure Delete( Run : in out Wsc_Db_Data.Run ) is
         c : d.Criteria;
   begin  
      Add_Run_Id( c, Run.Run_Id );
      Add_Username( c, Run.Username );
      delete( c );
      Run := Wsc_Db_Data.Null_Run;
      DB_Logger.info( "delete record; execute query OK" );
   end Delete;


   --
   -- delete the records indentified by the criteria
   --
   procedure Delete( c : d.Criteria ) is
   begin      
      delete( d.to_string( c ) );
      DB_Logger.info( "delete criteria; execute query OK" );
   end Delete;
   
   procedure Delete( where_Clause : String ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := DELETE_PART & To_Unbounded_String(" ");
   begin
      query := query & where_Clause;
      begin -- try catch block for execute
         DB_Logger.info( "delete; executing query" & To_String(query) );
         connection := Connection_Pool.Lease;
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         DB_Logger.info( "delete; execute query OK" );
      exception 
         when Error : No_Data => Null; -- silently ignore no data exception, which is hardly exceptional
         when Error : others =>
            DB_Logger.error( "delete; execute query failed with message " & Exception_Information( Error ));
            Raise_Exception( d.DB_Exception'Identity, 
               "delete: exception thrown " & Exception_Information( Error ));
      end;
      begin -- try catch block for close
         dodbc.Close_Prepared_Statement( ps );
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            DB_Logger.error( "delete; execute query failed with message " & Exception_Information( Error ));
            Raise_Exception( d.DB_Exception'Identity, 
               "delete: exception thrown " & Exception_Information( Error ));
      end;
   end Delete;


   --
   -- functions to retrieve records from tables with foreign keys
   -- referencing the table modelled by this package
   --
   function Retrieve_Associated_Uap_Thresholds( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Uap_Threshold_List.Vector is
      c : d.Criteria;
   begin
      Uap_Threshold_IO.Add_Username( c, Run.Username );
      Uap_Threshold_IO.Add_Run_Id( c, Run.Run_Id );
      return Uap_Threshold_IO.retrieve( c );
   end Retrieve_Associated_Uap_Thresholds;


   function Retrieve_Associated_Key_Value_Parameters( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Key_Value_Parameter_List.Vector is
      c : d.Criteria;
   begin
      Key_Value_Parameter_IO.Add_Username( c, Run.Username );
      Key_Value_Parameter_IO.Add_Run_Id( c, Run.Run_Id );
      return Key_Value_Parameter_IO.retrieve( c );
   end Retrieve_Associated_Key_Value_Parameters;


   function Retrieve_Associated_Probit_Thresholds( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Probit_Threshold_List.Vector is
      c : d.Criteria;
   begin
      Probit_Threshold_IO.Add_Username( c, Run.Username );
      Probit_Threshold_IO.Add_Run_Id( c, Run.Run_Id );
      return Probit_Threshold_IO.retrieve( c );
   end Retrieve_Associated_Probit_Thresholds;


   function Retrieve_Associated_Personal_Results( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Personal_Results_List.Vector is
      c : d.Criteria;
   begin
      Personal_Results_IO.Add_Username( c, Run.Username );
      Personal_Results_IO.Add_Run_Id( c, Run.Run_Id );
      return Personal_Results_IO.retrieve( c );
   end Retrieve_Associated_Personal_Results;


   function Retrieve_Child_State( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.State is
   begin
      return State_IO.retrieve_By_PK( Run.Username, Run.Run_Id );
   end Retrieve_Child_State;


   function Retrieve_Associated_Disaggregated_Data_Tables( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Disaggregated_Data_Table_List.Vector is
      c : d.Criteria;
   begin
      Disaggregated_Data_Table_IO.Add_Run_Id( c, Run.Run_Id );
      Disaggregated_Data_Table_IO.Add_Username( c, Run.Username );
      return Disaggregated_Data_Table_IO.retrieve( c );
   end Retrieve_Associated_Disaggregated_Data_Tables;


   function Retrieve_Associated_Uprate_Assumptions( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Uprate_Assumption_List.Vector is
      c : d.Criteria;
   begin
      Uprate_Assumption_IO.Add_Username( c, Run.Username );
      Uprate_Assumption_IO.Add_Run_Id( c, Run.Run_Id );
      return Uprate_Assumption_IO.retrieve( c );
   end Retrieve_Associated_Uprate_Assumptions;


   function Retrieve_Associated_Personal_Incomes( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Personal_Income_List.Vector is
      c : d.Criteria;
   begin
      Personal_Income_IO.Add_Username( c, Run.Username );
      Personal_Income_IO.Add_Run_Id( c, Run.Run_Id );
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
   elem : d.Criterion := d.make_Criterion_Element( "dataset_name", op, join, To_String( Dataset_Name ), 32 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dataset_Name;


   procedure Add_Dataset_Name( c : in out d.Criteria; Dataset_Name : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dataset_name", op, join, Dataset_Name, 32 );
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


   procedure Add_Weighting_Function( c : in out d.Criteria; Weighting_Function : run_weighting_function_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "weighting_function", op, join, Integer( run_weighting_function_Enum'Pos( Weighting_Function )) );
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


   procedure Add_Status( c : in out d.Criteria; Status : run_status_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "status", op, join, Integer( run_status_Enum'Pos( Status )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Status;


   procedure Add_Type_Of_Run( c : in out d.Criteria; Type_Of_Run : run_type_of_run_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "type_of_run", op, join, Integer( run_type_of_run_Enum'Pos( Type_Of_Run )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Type_Of_Run;


   
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


   procedure Add_Status_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "status", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Status_To_Orderings;


   procedure Add_Type_Of_Run_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "type_of_run", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Type_Of_Run_To_Orderings;


   
end Run_IO;
