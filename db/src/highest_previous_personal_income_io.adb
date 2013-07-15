--
-- Created by ada_generator.py on 2011-11-29 16:58:56.632011
-- 
with Wsc_Db_Data;


with Ada.Containers.Vectors;

with environment;

with db_commons; 
with db_commons.odbc; 

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
with text_io;
with Ada.Strings.Maps;


with DB_Logger;

package body Highest_Previous_Personal_Income_IO is

   use Ada.Strings.Unbounded;
   use Ada.Exceptions;
   use Ada.Strings;

   package dodbc renames db_commons.odbc;
   package sql renames GNU.DB.SQLCLI;
   package sql_info renames GNU.DB.SQLCLI.Info;
   use sql;
   use base_types;
   --
   -- generic packages to handle each possible type of decimal, if any, go here
   --
   
   --
   -- Select all variables; substring to be competed with output from some criteria
   --
   SELECT_PART : constant String := " select " & 
        "case when sum( value ) is null then 0.0 else sum( value ) end as sum_value , " &
        "case when max( value ) is null then 0.0 else max( value ) end as max_value " &
        " from personal_incomes "; 
   
   function get_connection return dodbc.Database_Connection is
      use dodbc;
      con : dodbc.Database_Connection := dodbc.Null_Database_Connection;
   begin
      con := dodbc.connect( 
            environment.Get_Server_Name, 
            environment.Get_Username, 
            environment.Get_Password );
      return con;
      exception 
      when Error : others =>  
         DB_Logger.error( "exception " & Exception_Information(Error) ); 
         Raise_Exception( d.DB_Exception'Identity, 
            "getConnection: exception thrown " & Exception_Information(Error) );
   end get_connection;
   

   --
   -- returns true if the primary key parts of Highest_Previous_Personal_Income match the defaults in Wsc_Db_Data.Null_Highest_Previous_Personal_Income
   --
   --
   -- Does this Highest_Previous_Personal_Income equal the default Wsc_Db_Data.Null_Highest_Previous_Personal_Income ?
   --
   function Is_Null( Highest_Previous_Personal_Income : Wsc_Db_Data.Highest_Previous_Personal_Income ) return Boolean is
   use Wsc_Db_Data;
   begin
      return Highest_Previous_Personal_Income = Wsc_Db_Data.Null_Highest_Previous_Personal_Income;
   end Is_Null;


   
   --
   -- returns the single Highest_Previous_Personal_Income matching the primary key fields, or the Wsc_Db_Data.Null_Highest_Previous_Personal_Income record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String; Run_Id : Unbounded_String; Pid : Unbounded_String; Wave : Unbounded_String; Income_Name : Unbounded_String ) return Wsc_Db_Data.Highest_Previous_Personal_Income is
      l : Wsc_Db_Data.Highest_Previous_Personal_Income_List.Vector;
      Highest_Previous_Personal_Income : Wsc_Db_Data.Highest_Previous_Personal_Income;
      c : d.Criteria;
   begin      
      Add_Username( c, Username );
      Add_Run_Id( c, Run_Id );
      Add_Pid( c, Pid );
      Add_Wave( c, Wave );
      Add_Income_Name( c, Income_Name );
      l := retrieve( c );
      if( not Wsc_Db_Data.Highest_Previous_Personal_Income_List.is_empty( l ) ) then
         Highest_Previous_Personal_Income := Wsc_Db_Data.Highest_Previous_Personal_Income_List.First_Element( l );
      else
         Highest_Previous_Personal_Income := Wsc_Db_Data.Null_Highest_Previous_Personal_Income;
      end if;
      return Highest_Previous_Personal_Income;
   end Retrieve_By_PK;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Highest_Previous_Personal_Income matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Highest_Previous_Personal_Income_List.Vector is
   begin      
      return Retrieve( d.To_String( c ) );
   end Retrieve;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Highest_Previous_Personal_Income retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Highest_Previous_Personal_Income_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.Highest_Previous_Personal_Income_List.Vector;
      ps : SQLHDBC := SQL_NULL_HANDLE;
      has_data : Boolean := false;
      connection : dodbc.Database_Connection := get_connection;
      query : constant String := SELECT_PART & " " & sqlstr;
      --
      -- aliased local versions of fields 
      --
      Username: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Run_Id: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Pid: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Wave: aliased String := 
            "@@";
      Hid: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Buno: aliased integer;
      Adno: aliased integer;
      Income_Name: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Value: aliased Real;
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'access;
      Run_Id_access : String_Access := Run_Id'access;
      Pid_access : String_Access := Pid'access;
      Wave_access : String_Access := Wave'access;
      Hid_access : String_Access := Hid'access;
      Income_Name_access : String_Access := Income_Name'access;
      Value_access : Real_Access := Value'access;
      --
      -- length holders for each retrieved variable
      --
      Username_len : aliased SQLINTEGER := Username'Size;
      Run_Id_len : aliased SQLINTEGER := Run_Id'Size;
      Pid_len : aliased SQLINTEGER := Pid'Size;
      Wave_len : aliased SQLINTEGER := Wave'Size;
      Hid_len : aliased SQLINTEGER := Hid'Size;
      Buno_len : aliased SQLINTEGER := Buno'Size;
      Adno_len : aliased SQLINTEGER := Adno'Size;
      Income_Name_len : aliased SQLINTEGER := Income_Name'Size;
      Value_len : aliased SQLINTEGER := Value'Size;
      Highest_Previous_Personal_Income : Wsc_Db_Data.Highest_Previous_Personal_Income;
   begin
      DB_Logger.info( "retrieve made this as query " & query );
      begin -- exception block
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 1,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Username_access.all'address ),
            BufferLength     => Username_len,
            StrLen_Or_IndPtr => Username_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 2,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Run_Id_access.all'address ),
            BufferLength     => Run_Id_len,
            StrLen_Or_IndPtr => Run_Id_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 3,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Pid_access.all'address ),
            BufferLength     => Pid_len,
            StrLen_Or_IndPtr => Pid_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Wave_access.all'address ),
            BufferLength     => Wave_len,
            StrLen_Or_IndPtr => Wave_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 5,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Hid_access.all'address ),
            BufferLength     => Hid_len,
            StrLen_Or_IndPtr => Hid_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 6,
            TargetValue      => Buno'access,
            IndPtr           => Buno_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 7,
            TargetValue      => Adno'access,
            IndPtr           => Adno_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 8,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Income_Name_access.all'address ),
            BufferLength     => Income_Name_len,
            StrLen_Or_IndPtr => Income_Name_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 9,
            TargetValuePtr   => To_SQLPOINTER( Value_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Value_len'access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Highest_Previous_Personal_Income.Username := Slice_To_Unbounded( Username, 1, Natural( Username_len ) );
            Highest_Previous_Personal_Income.Run_Id := Slice_To_Unbounded( Run_Id, 1, Natural( Run_Id_len ) );
            Highest_Previous_Personal_Income.Pid := Slice_To_Unbounded( Pid, 1, Natural( Pid_len ) );
            Highest_Previous_Personal_Income.Wave := Slice_To_Unbounded( Wave, 1, Natural( Wave_len ) );
            Highest_Previous_Personal_Income.Hid := Slice_To_Unbounded( Hid, 1, Natural( Hid_len ) );
            Highest_Previous_Personal_Income.Buno := Buno;
            Highest_Previous_Personal_Income.Adno := Adno;
            Highest_Previous_Personal_Income.Income_Name := Slice_To_Unbounded( Income_Name, 1, Natural( Income_Name_len ) );
            Highest_Previous_Personal_Income.Value:= Real( Value_access.all );
            Wsc_Db_Data.Highest_Previous_Personal_Income_List.append( l, Highest_Previous_Personal_Income );        
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
         dodbc.Disconnect( connection );
      exception 
         when Error : others =>
            Raise_Exception( d.DB_Exception'Identity, 
               "retrieve: exception " & Exception_Information(Error) );
      end; -- exception block
      return l;
   end Retrieve;

   
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Highest_Previous_Personal_Income
   --

   procedure Delete( Highest_Previous_Personal_Income : in out Wsc_Db_Data.Highest_Previous_Personal_Income ) is
         c : d.Criteria;
   begin  
      Add_Username( c, Highest_Previous_Personal_Income.Username );
      Add_Run_Id( c, Highest_Previous_Personal_Income.Run_Id );
      Add_Pid( c, Highest_Previous_Personal_Income.Pid );
      Add_Wave( c, Highest_Previous_Personal_Income.Wave );
      Add_Income_Name( c, Highest_Previous_Personal_Income.Income_Name );
      delete( c );
      Highest_Previous_Personal_Income := Wsc_Db_Data.Null_Highest_Previous_Personal_Income;
      DB_Logger.info( "delete record; execute query OK" );
   end Delete;


   --
   -- delete the records indentified by the criteria
   --
   procedure Delete( c : d.Criteria ) is
   begin      
      delete( d.To_String( c ) );
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
         connection := get_connection;
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         DB_Logger.info( "delete; execute query OK" );
      exception 
         when Error : No_Data => Null; -- silently ignore no data exception, which is hardly exceptional
         when Error : others =>
            DB_Logger.error( "delete; execute query failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "delete: exception thrown " & Exception_Information(Error) );
      end;
      begin -- try catch block for close
         dodbc.Close_Prepared_Statement( ps );
         dodbc.Disconnect( connection );
      exception 
         when Error : others =>
            DB_Logger.error( "delete; execute query failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "delete: exception thrown " & Exception_Information(Error) );
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


   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "run_id", op, join, To_String( Run_Id ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Run_Id;


   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "run_id", op, join, Run_Id, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Run_Id;


   procedure Add_Pid( c : in out d.Criteria; Pid : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "pid", op, join, To_String( Pid ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Pid;


   procedure Add_Pid( c : in out d.Criteria; Pid : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "pid", op, join, Pid, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Pid;


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


   procedure Add_Hid( c : in out d.Criteria; Hid : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "hid", op, join, To_String( Hid ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Hid;


   procedure Add_Hid( c : in out d.Criteria; Hid : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "hid", op, join, Hid, 60 );
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


   procedure Add_Income_Name( c : in out d.Criteria; Income_Name : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "income_name", op, join, To_String( Income_Name ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Income_Name;


   procedure Add_Income_Name( c : in out d.Criteria; Income_Name : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "income_name", op, join, Income_Name, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Income_Name;


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


   procedure Add_Wave_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "wave", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Wave_To_Orderings;


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


   procedure Add_Income_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "income_name", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Income_Name_To_Orderings;


   procedure Add_Value_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "value", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value_To_Orderings;


   
end Highest_Previous_Personal_Income_IO;
