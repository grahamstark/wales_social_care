--
-- Created by ada_generator.py on 2012-02-22 13:19:40.006030
-- 
with Wsc_Db_Data;


with Ada.Containers.Vectors;

with Environment;

with DB_Commons; 
-- M1

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
with Utils;

with GNATColl.Traces;

package body State_IO is

   use Ada.Strings.Unbounded;
   use Ada.Exceptions;
   use Ada.Strings;

   package dodbc renames DB_Commons.ODBC;
   package sql renames GNU.DB.SQLCLI;
   package sql_info renames GNU.DB.SQLCLI.Info;
   use sql;
   use Base_Types;
   --
   -- generic packages to handle each possible type of decimal, if any, go here
   --
   
   --
   -- Select all variables; substring to be competed with output from some criteria
   --
   SELECT_PART : constant String := "select " &
         "username, run_id, household, other_counter, other_counter2, other_counter3, other_counter4, year, phase, health, error_code, read_error, session_id" &
         " from state " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into state (" &
         "username, run_id, household, other_counter, other_counter2, other_counter3, other_counter4, year, phase, health, error_code, read_error, session_id" &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from state ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update state set  ";
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "STATE_IO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;
   
   -- 
   -- Next highest avaiable value of Run_Id - useful for saving  
   --
   function Next_Free_Run_Id return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( run_id ) from state";
      ai : aliased integer;
      has_data : boolean := true; 
      output_len : aliased sql.SQLLEN;     
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
   -- returns true if the primary key parts of State match the defaults in Wsc_Db_Data.Null_State
   --
   --
   -- Does this State equal the default Wsc_Db_Data.Null_State ?
   --
   function Is_Null( State : Wsc_Db_Data.State ) return Boolean is
   use Wsc_Db_Data;
   begin
      return State = Wsc_Db_Data.Null_State;
   end Is_Null;


   
   --
   -- returns the single State matching the primary key fields, or the Wsc_Db_Data.Null_State record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String; Run_Id : integer ) return Wsc_Db_Data.State is
      l : Wsc_Db_Data.State_List.Vector;
      State : Wsc_Db_Data.State;
      c : d.Criteria;
   begin      
      Add_Username( c, Username );
      Add_Run_Id( c, Run_Id );
      l := Retrieve( c );
      if( not Wsc_Db_Data.State_List.is_empty( l ) ) then
         State := Wsc_Db_Data.State_List.First_Element( l );
      else
         State := Wsc_Db_Data.Null_State;
      end if;
      return State;
   end Retrieve_By_PK;

   
   --
   -- Retrieves a list of Wsc_Db_Data.State matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.State_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   
   --
   -- Retrieves a list of Wsc_Db_Data.State retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.State_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.State_List.Vector;
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
      Household: aliased integer;
      Other_Counter: aliased integer;
      Other_Counter2: aliased integer;
      Other_Counter3: aliased integer;
      Other_Counter4: aliased integer;
      Year: aliased integer;
      Phase: aliased Integer;
      Health: aliased Integer;
      Error_Code: aliased integer;
      Read_Error: aliased Integer;
      Session_Id: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" &
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'access;
      Session_Id_access : String_Access := Session_Id'access;
      --
      -- length holders for each retrieved variable
      --
      Username_len : aliased SQLLEN := Username'Size/8;
      Run_Id_len : aliased SQLLEN := Run_Id'Size/8;
      Household_len : aliased SQLLEN := Household'Size/8;
      Other_Counter_len : aliased SQLLEN := Other_Counter'Size/8;
      Other_Counter2_len : aliased SQLLEN := Other_Counter2'Size/8;
      Other_Counter3_len : aliased SQLLEN := Other_Counter3'Size/8;
      Other_Counter4_len : aliased SQLLEN := Other_Counter4'Size/8;
      Year_len : aliased SQLLEN := Year'Size/8;
      Phase_len : aliased SQLLEN := Phase'Size/8;
      Health_len : aliased SQLLEN := Health'Size/8;
      Error_Code_len : aliased SQLLEN := Error_Code'Size/8;
      Read_Error_len : aliased SQLLEN := Read_Error'Size/8;
      Session_Id_len : aliased SQLLEN := Session_Id'Size/8;
      State : Wsc_Db_Data.State := BLANK_STATE_TYPE;
   begin
      Log( "retrieve made this as query " & query );
      begin -- exception block
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 1,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Username_access.all'address ),
            BufferLength     => Username_len,
            StrLen_Or_IndPtr => Username_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 2,
            TargetValue      => Run_Id'access,
            IndPtr           => Run_Id_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 3,
            TargetValue      => Household'access,
            IndPtr           => Household_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetValue      => Other_Counter'access,
            IndPtr           => Other_Counter_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 5,
            TargetValue      => Other_Counter2'access,
            IndPtr           => Other_Counter2_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 6,
            TargetValue      => Other_Counter3'access,
            IndPtr           => Other_Counter3_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 7,
            TargetValue      => Other_Counter4'access,
            IndPtr           => Other_Counter4_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 8,
            TargetValue      => Year'access,
            IndPtr           => Year_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 9,
            TargetValue      => Phase'access,
            IndPtr           => Phase_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 10,
            TargetValue      => Health'access,
            IndPtr           => Health_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 11,
            TargetValue      => Error_Code'access,
            IndPtr           => Error_Code_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 12,
            TargetValue      => Read_Error'access,
            IndPtr           => Read_Error_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 13,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Session_Id_access.all'address ),
            BufferLength     => Session_Id_len,
            StrLen_Or_IndPtr => Session_Id_len'access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            State.Username := Slice_To_Unbounded( Username, 1, Natural( Username_len ) );
            State.Run_Id := Run_Id;
            State.Household := Household;
            State.Other_Counter := Other_Counter;
            State.Other_Counter2 := Other_Counter2;
            State.Other_Counter3 := Other_Counter3;
            State.Other_Counter4 := Other_Counter4;
            State.Year := Year;
            State.Phase := Phase_Type'Val( Phase );
            State.Health := Health_Type'Val( Health );
            State.Error_Code := Error_Code;
            State.Read_Error := Boolean'Val( Read_Error );
            State.Session_Id := Slice_To_Unbounded( Session_Id, 1, Natural( Session_Id_len ) );
            Wsc_Db_Data.State_List.append( l, State );        
         end loop;
         Log( "retrieve: Query Run OK" );
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
   procedure Update( State : Wsc_Db_Data.State ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Household( values_c, State.Household );
      Add_Other_Counter( values_c, State.Other_Counter );
      Add_Other_Counter2( values_c, State.Other_Counter2 );
      Add_Other_Counter3( values_c, State.Other_Counter3 );
      Add_Other_Counter4( values_c, State.Other_Counter4 );
      Add_Year( values_c, State.Year );
      Add_Phase( values_c, State.Phase );
      Add_Health( values_c, State.Health );
      Add_Error_Code( values_c, State.Error_Code );
      Add_Read_Error( values_c, State.Read_Error );
      Add_Session_Id( values_c, State.Session_Id );
      --
      -- primary key fields
      --
      Add_Username( pk_c, State.Username );
      Add_Run_Id( pk_c, State.Run_Id );
      query := query & d.To_String( values_c, "," ) & d.to_string( pk_c );
      Log( "update; executing query" & To_String(query) );
      begin -- exception block      
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         Log( "update; execute query OK" );
      exception 
         when No_Data => Null; -- ignore if no updates made
         when Error : others =>
            GNATColl.Traces.Trace( log_trace, error, "update: failed with message " );
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
   procedure Save( State : Wsc_Db_Data.State; overwrite : Boolean := True; connection : Database_Connection := Null_Database_Connection ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      State_Tmp : Wsc_Db_Data.State;
      -- M1
      local_connection : Database_Connection;
      locally_allocated_connection : Boolean;
   begin
      Log( "State.Save; " & To_String( state ));
      Log( Utils.Get_Stack_Trace( "State_IO.Save" ));
      if( overwrite ) then
         State_Tmp := retrieve_By_PK( State.Username, State.Run_Id );
         if( not is_Null( State_Tmp )) then
            Update( State );
            return;
         end if;
      end if;
      Add_Username( c, State.Username );
      Add_Run_Id( c, State.Run_Id );
      Add_Household( c, State.Household );
      Add_Other_Counter( c, State.Other_Counter );
      Add_Other_Counter2( c, State.Other_Counter2 );
      Add_Other_Counter3( c, State.Other_Counter3 );
      Add_Other_Counter4( c, State.Other_Counter4 );
      Add_Year( c, State.Year );
      Add_Phase( c, State.Phase );
      Add_Health( c, State.Health );
      Add_Error_Code( c, State.Error_Code );
      Add_Read_Error( c, State.Read_Error );
      Add_Session_Id( c, State.Session_Id );
      query := query & "( "  & d.To_Crude_Array_Of_Values( c ) & " )";
      Log( "save; executing query" & To_String(query) );
      begin
         -- M1
         if( connection = Null_Database_Connection )then
            local_connection := Connection_Pool.Lease;
            locally_allocated_connection := True;
         else
            local_connection := connection;
            locally_allocated_connection := False;         
         end if;
         ps := dodbc.Initialise_Prepared_Statement( local_connection.connection, query );       
         SQLExecute( ps );
         Log( "save; execute query OK" );         
      exception 
         when Error : others =>
            GNATColl.Traces.Trace( log_trace, error, "save; execute query failed with message " );
            Raise_Exception( d.DB_Exception'Identity, 
               "save: exception thrown " & Exception_Information(Error) );
      end;
      begin
         dodbc.Close_Prepared_Statement( ps );
         -- M1
         if( locally_allocated_connection )then
            Connection_Pool.Return_Connection( local_connection );
         end if;
      exception 
         when Error : others =>
            GNATColl.Traces.Trace( log_trace, error, "save/close " );
            Raise_Exception( d.DB_Exception'Identity, 
               "save/close: exception " & Exception_Information( Error) );
      end;
      
   end Save;

   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_State
   --

   procedure Delete( State : in out Wsc_Db_Data.State ) is
         c : d.Criteria;
   begin  
      Add_Username( c, State.Username );
      Add_Run_Id( c, State.Run_Id );
      delete( c );
      State := Wsc_Db_Data.Null_State;
      Log( "delete record; execute query OK" );
   end Delete;

   procedure Cleanup is
      c :  d.Criteria;
   begin
      Add_Phase( c, complete, d.lt );
      Delete( c );
   end Cleanup;
   


   --
   -- delete the records indentified by the criteria
   --
   procedure Delete( c : d.Criteria ) is
   begin      
      delete( d.to_string( c ) );
      Log( "delete criteria; execute query OK" );
   end Delete;
   
   procedure Delete( where_Clause : String ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := DELETE_PART & To_Unbounded_String(" ");
   begin
      query := query & where_Clause;
      begin -- try catch block for execute
         Log( "delete; executing query" & To_String(query) );
         connection := Connection_Pool.Lease;
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         Log( "delete; execute query OK" );
      exception 
         when Error : No_Data => Null; -- silently ignore no data exception, which is hardly exceptional
         when Error : others =>
            GNATColl.Traces.Trace( log_trace, error, "delete; execute query failed with message " );
            Raise_Exception( d.DB_Exception'Identity, 
               "delete: exception thrown " & Exception_Information(Error) );
      end;
      begin -- try catch block for close
         dodbc.Close_Prepared_Statement( ps );
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            GNATColl.Traces.Trace( log_trace, error, "delete; execute query failed with message " );
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


   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "run_id", op, join, Run_Id );
   begin
      d.add_to_criteria( c, elem );
   end Add_Run_Id;


   procedure Add_Household( c : in out d.Criteria; Household : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "household", op, join, Household );
   begin
      d.add_to_criteria( c, elem );
   end Add_Household;


   procedure Add_Other_Counter( c : in out d.Criteria; Other_Counter : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "other_counter", op, join, Other_Counter );
   begin
      d.add_to_criteria( c, elem );
   end Add_Other_Counter;

   procedure Add_Other_Counter2( c : in out d.Criteria; Other_Counter2 : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "other_counter2", op, join, Other_Counter2 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Other_Counter2;

   procedure Add_Other_Counter3( c : in out d.Criteria; Other_Counter3 : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "other_counter3", op, join, Other_Counter3 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Other_Counter3;

   procedure Add_Other_Counter4( c : in out d.Criteria; Other_Counter4 : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "other_counter4", op, join, Other_Counter4 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Other_Counter4;

   procedure Add_Year( c : in out d.Criteria; Year : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "year", op, join, Year );
   begin
      d.add_to_criteria( c, elem );
   end Add_Year;


   procedure Add_Phase( c : in out d.Criteria; Phase : Phase_Type; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "phase", op, join, Integer( Phase_Type'Pos( Phase )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Phase;


   procedure Add_Health( c : in out d.Criteria; Health : Health_Type; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "health", op, join, Integer( Health_Type'Pos( Health )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Health;


   procedure Add_Error_Code( c : in out d.Criteria; Error_Code : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "error_code", op, join, Error_Code );
   begin
      d.add_to_criteria( c, elem );
   end Add_Error_Code;


   procedure Add_Read_Error( c : in out d.Criteria; Read_Error : boolean; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "read_error", op, join, Integer( boolean'Pos( Read_Error )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Read_Error;


   procedure Add_Session_Id( c : in out d.Criteria; Session_Id : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "session_id", op, join, To_String( Session_Id ), 120 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Session_Id;


   procedure Add_Session_Id( c : in out d.Criteria; Session_Id : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "session_id", op, join, Session_Id, 120 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Session_Id;


   
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


   procedure Add_Household_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "household", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Household_To_Orderings;


   procedure Add_Other_Counter_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "other_counter", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Other_Counter_To_Orderings;

   procedure Add_Other_Counter2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "other_counter2", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Other_Counter2_To_Orderings;

   procedure Add_Other_Counter3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "other_counter3", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Other_Counter3_To_Orderings;

   procedure Add_Other_Counter4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "other_counter4", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Other_Counter4_To_Orderings;

   procedure Add_Year_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "year", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Year_To_Orderings;


   procedure Add_Phase_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "phase", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Phase_To_Orderings;


   procedure Add_Health_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "health", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Health_To_Orderings;


   procedure Add_Error_Code_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "error_code", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Error_Code_To_Orderings;


   procedure Add_Read_Error_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "read_error", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Read_Error_To_Orderings;


   procedure Add_Session_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "session_id", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Session_Id_To_Orderings;


   
end State_IO;
