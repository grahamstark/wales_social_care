--
-- Created by ada_generator.py on 2012-07-24 19:03:55.762772
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


with DB_Logger;

package body Personal_Income_IO is

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
         "username, run_id, pid, sysno, iteration, wave, income_type, hid, buno, adno," &
         "value " &
         " from personal_income " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into personal_income (" &
         "username, run_id, pid, sysno, iteration, wave, income_type, hid, buno, adno," &
         "value " &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from personal_income ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update personal_income set  ";
   
   
   -- 
   -- Next highest avaiable value of Run_Id - useful for saving  
   --
   function Next_Free_Run_Id return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( run_id ) from personal_income";
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
   -- Next highest avaiable value of Pid - useful for saving  
   --
   function Next_Free_Pid return Big_Integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( pid ) from personal_income";
      ai : aliased Big_Integer;
      has_data : boolean := true; 
      output_len : aliased sql.SQLINTEGER;     
   begin
      ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
      dodbc.L_Out_Binding.SQLBindCol( 
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
   end Next_Free_Pid;


   -- 
   -- Next highest avaiable value of Sysno - useful for saving  
   --
   function Next_Free_Sysno return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( sysno ) from personal_income";
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
   end Next_Free_Sysno;


   -- 
   -- Next highest avaiable value of Iteration - useful for saving  
   --
   function Next_Free_Iteration return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( iteration ) from personal_income";
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
   end Next_Free_Iteration;



   --
   -- returns true if the primary key parts of Personal_Income match the defaults in Wsc_Db_Data.Null_Personal_Income
   --
   --
   -- Does this Personal_Income equal the default Wsc_Db_Data.Null_Personal_Income ?
   --
   function Is_Null( Personal_Income : Wsc_Db_Data.Personal_Income ) return Boolean is
   use Wsc_Db_Data;
   begin
      return Personal_Income = Wsc_Db_Data.Null_Personal_Income;
   end Is_Null;


   
   --
   -- returns the single Personal_Income matching the primary key fields, or the Wsc_Db_Data.Null_Personal_Income record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String; Run_Id : integer; Pid : Big_Integer; Sysno : integer; Iteration : integer; Wave : Unbounded_String; Income_Type : personal_income_income_type_Enum ) return Wsc_Db_Data.Personal_Income is
      l : Wsc_Db_Data.Personal_Income_List.Vector;
      Personal_Income : Wsc_Db_Data.Personal_Income;
      c : d.Criteria;
   begin      
      Add_Username( c, Username );
      Add_Run_Id( c, Run_Id );
      Add_Pid( c, Pid );
      Add_Sysno( c, Sysno );
      Add_Iteration( c, Iteration );
      Add_Wave( c, Wave );
      Add_Income_Type( c, Income_Type );
      l := Retrieve( c );
      if( not Wsc_Db_Data.Personal_Income_List.is_empty( l ) ) then
         Personal_Income := Wsc_Db_Data.Personal_Income_List.First_Element( l );
      else
         Personal_Income := Wsc_Db_Data.Null_Personal_Income;
      end if;
      return Personal_Income;
   end Retrieve_By_PK;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Personal_Income matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Personal_Income_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Personal_Income retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Personal_Income_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.Personal_Income_List.Vector;
      ps : SQLHDBC := SQL_NULL_HANDLE;
      has_data : Boolean := false;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := SELECT_PART & " " & sqlstr;
      --
      -- aliased local versions of fields 
      --
      Username: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Run_Id: aliased integer;
      Pid: aliased Big_Integer;
      Sysno: aliased integer;
      Iteration: aliased integer;
      Wave: aliased String := 
            "@@";
      Income_Type: aliased Integer;
      Hid: aliased Big_Integer;
      Buno: aliased integer;
      Adno: aliased integer;
      Value: aliased Real;
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'Access;
      Wave_access : String_Access := Wave'Access;
      Value_access : Real_Access := Value'Access;
      --
      -- length holders for each retrieved variable
      --
      Username_len : aliased SQLINTEGER := Username'Size/8;
      Run_Id_len : aliased SQLINTEGER := Run_Id'Size/8;
      Pid_len : aliased SQLINTEGER := Pid'Size/8;
      Sysno_len : aliased SQLINTEGER := Sysno'Size/8;
      Iteration_len : aliased SQLINTEGER := Iteration'Size/8;
      Wave_len : aliased SQLINTEGER := Wave'Size/8;
      Income_Type_len : aliased SQLINTEGER := Income_Type'Size/8;
      Hid_len : aliased SQLINTEGER := Hid'Size/8;
      Buno_len : aliased SQLINTEGER := Buno'Size/8;
      Adno_len : aliased SQLINTEGER := Adno'Size/8;
      Value_len : aliased SQLINTEGER := Value'Size/8;
      Personal_Income : Wsc_Db_Data.Personal_Income;
   begin
      DB_Logger.info( "retrieve made this as query " & query );
      begin -- exception block
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 1,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Username_access.all'Address ),
            BufferLength     => Username_len,
            StrLen_Or_IndPtr => Username_len'Access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 2,
            TargetValue      => Run_Id'access,
            IndPtr           => Run_Id_len'access );
         dodbc.L_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 3,
            TargetValue      => Pid'access,
            IndPtr           => Pid_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetValue      => Sysno'access,
            IndPtr           => Sysno_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 5,
            TargetValue      => Iteration'access,
            IndPtr           => Iteration_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 6,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Wave_access.all'Address ),
            BufferLength     => Wave_len,
            StrLen_Or_IndPtr => Wave_len'Access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 7,
            TargetValue      => Income_Type'access,
            IndPtr           => Income_Type_len'access );
         dodbc.L_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 8,
            TargetValue      => Hid'access,
            IndPtr           => Hid_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 9,
            TargetValue      => Buno'access,
            IndPtr           => Buno_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 10,
            TargetValue      => Adno'access,
            IndPtr           => Adno_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 11,
            TargetValuePtr   => To_SQLPOINTER( Value_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Value_len'Access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Personal_Income.Username := Slice_To_Unbounded( Username, 1, Natural( Username_len ) );
            Personal_Income.Run_Id := Run_Id;
            Personal_Income.Pid := Pid;
            Personal_Income.Sysno := Sysno;
            Personal_Income.Iteration := Iteration;
            Personal_Income.Wave := Slice_To_Unbounded( Wave, 1, Natural( Wave_len ) );
            Personal_Income.Income_Type := personal_income_income_type_Enum'Val( Income_Type );
            Personal_Income.Hid := Hid;
            Personal_Income.Buno := Buno;
            Personal_Income.Adno := Adno;
            Personal_Income.Value:= Real( Value_access.all );
            Wsc_Db_Data.Personal_Income_List.append( l, Personal_Income );        
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
   procedure Update( Personal_Income : Wsc_Db_Data.Personal_Income ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Hid( values_c, Personal_Income.Hid );
      Add_Buno( values_c, Personal_Income.Buno );
      Add_Adno( values_c, Personal_Income.Adno );
      Add_Value( values_c, Personal_Income.Value );
      --
      -- primary key fields
      --
      Add_Username( pk_c, Personal_Income.Username );
      Add_Run_Id( pk_c, Personal_Income.Run_Id );
      Add_Pid( pk_c, Personal_Income.Pid );
      Add_Sysno( pk_c, Personal_Income.Sysno );
      Add_Iteration( pk_c, Personal_Income.Iteration );
      Add_Wave( pk_c, Personal_Income.Wave );
      Add_Income_Type( pk_c, Personal_Income.Income_Type );
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
   procedure Save( Personal_Income : Wsc_Db_Data.Personal_Income; overwrite : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      Personal_Income_Tmp : Wsc_Db_Data.Personal_Income;
   begin
      if( overwrite ) then
         Personal_Income_Tmp := retrieve_By_PK( Personal_Income.Username, Personal_Income.Run_Id, Personal_Income.Pid, Personal_Income.Sysno, Personal_Income.Iteration, Personal_Income.Wave, Personal_Income.Income_Type );
         if( not is_Null( Personal_Income_Tmp )) then
            Update( Personal_Income );
            return;
         end if;
      end if;
      Add_Username( c, Personal_Income.Username );
      Add_Run_Id( c, Personal_Income.Run_Id );
      Add_Pid( c, Personal_Income.Pid );
      Add_Sysno( c, Personal_Income.Sysno );
      Add_Iteration( c, Personal_Income.Iteration );
      Add_Wave( c, Personal_Income.Wave );
      Add_Income_Type( c, Personal_Income.Income_Type );
      Add_Hid( c, Personal_Income.Hid );
      Add_Buno( c, Personal_Income.Buno );
      Add_Adno( c, Personal_Income.Adno );
      Add_Value( c, Personal_Income.Value );
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
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Personal_Income
   --

   procedure Delete( Personal_Income : in out Wsc_Db_Data.Personal_Income ) is
         c : d.Criteria;
   begin  
      Add_Username( c, Personal_Income.Username );
      Add_Run_Id( c, Personal_Income.Run_Id );
      Add_Pid( c, Personal_Income.Pid );
      Add_Sysno( c, Personal_Income.Sysno );
      Add_Iteration( c, Personal_Income.Iteration );
      Add_Wave( c, Personal_Income.Wave );
      Add_Income_Type( c, Personal_Income.Income_Type );
      delete( c );
      Personal_Income := Wsc_Db_Data.Null_Personal_Income;
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

   --
   -- functions to add something to a criteria
   --
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


   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "run_id", op, join, Run_Id );
   begin
      d.add_to_criteria( c, elem );
   end Add_Run_Id;


   procedure Add_Pid( c : in out d.Criteria; Pid : Big_Integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "pid", op, join, Pid );
   begin
      d.add_to_criteria( c, elem );
   end Add_Pid;


   procedure Add_Sysno( c : in out d.Criteria; Sysno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "sysno", op, join, Sysno );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sysno;


   procedure Add_Iteration( c : in out d.Criteria; Iteration : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "iteration", op, join, Iteration );
   begin
      d.add_to_criteria( c, elem );
   end Add_Iteration;


   procedure Add_Wave( c : in out d.Criteria; Wave : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "wave", op, join, To_String( Wave ), 2 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Wave;


   procedure Add_Wave( c : in out d.Criteria; Wave : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "wave", op, join, Wave, 2 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Wave;


   procedure Add_Income_Type( c : in out d.Criteria; Income_Type : personal_income_income_type_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "income_type", op, join, Integer( personal_income_income_type_Enum'Pos( Income_Type )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Income_Type;


   procedure Add_Hid( c : in out d.Criteria; Hid : Big_Integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "hid", op, join, Hid );
   begin
      d.add_to_criteria( c, elem );
   end Add_Hid;


   procedure Add_Buno( c : in out d.Criteria; Buno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "buno", op, join, Buno );
   begin
      d.add_to_criteria( c, elem );
   end Add_Buno;


   procedure Add_Adno( c : in out d.Criteria; Adno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "adno", op, join, Adno );
   begin
      d.add_to_criteria( c, elem );
   end Add_Adno;


   procedure Add_Value( c : in out d.Criteria; Value : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "value", op, join, Value );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value;


   
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "username", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Username_To_Orderings;


   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "run_id", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Run_Id_To_Orderings;


   procedure Add_Pid_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "pid", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Pid_To_Orderings;


   procedure Add_Sysno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "sysno", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sysno_To_Orderings;


   procedure Add_Iteration_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "iteration", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Iteration_To_Orderings;


   procedure Add_Wave_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "wave", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Wave_To_Orderings;


   procedure Add_Income_Type_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "income_type", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Income_Type_To_Orderings;


   procedure Add_Hid_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "hid", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Hid_To_Orderings;


   procedure Add_Buno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "buno", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Buno_To_Orderings;


   procedure Add_Adno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "adno", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Adno_To_Orderings;


   procedure Add_Value_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "value", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value_To_Orderings;


   
end Personal_Income_IO;
