--
-- Created by ada_generator.py on 2012-02-09 16:45:16.000085
-- 
with Wsc_Db_Data;


with Ada.Containers.Vectors;
with Ada.Text_IO;

with Environment;

with DB_Commons; 
with DB_Commons.ODBC; 

with GNU.DB.SQLCLI;  
with GNU.DB.SQLCLI.Bind;
with GNU.DB.SQLCLI.Info;
with GNU.DB.SQLCLI.Environment_Attribute;
with GNU.DB.SQLCLI.Connection_Attribute;

with GNATColl.Traces;

with Ada.Exceptions;  
with Ada.Strings; 
with Ada.Strings.Wide_Fixed;
with Ada.Characters.Conversions;
with Ada.Strings.Unbounded; 
with Text_IO;
with Ada.Strings.Maps;
with Connection_Pool;

package body Uprate_Assumption_IO is

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
         "username, run_id, percent_change, use_obr, target, element " &
         " from uprate_assumption " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into uprate_assumption (" &
         "username, run_id, percent_change, use_obr, target, element " &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from uprate_assumption ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update uprate_assumption set  ";
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "UPRATE_ASSUMPTION_IO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   -- 
   -- Next highest avaiable value of Run_Id - useful for saving  
   --
   function Next_Free_Run_Id return Integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( run_id ) from uprate_assumption";
      ai : aliased Integer;
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
   -- returns true if the primary key parts of Uprate_Assumption match the defaults in Wsc_Db_Data.Null_Uprate_Assumption
   --
   --
   -- Does this Uprate_Assumption equal the default Wsc_Db_Data.Null_Uprate_Assumption ?
   --
   function Is_Null( Uprate_Assumption : Wsc_Db_Data.Uprate_Assumption ) return Boolean is
   use Wsc_Db_Data;
   begin
      return Uprate_Assumption = Wsc_Db_Data.Null_Uprate_Assumption;
   end Is_Null;


   
   --
   -- returns the single Uprate_Assumption matching the primary key fields, or the Wsc_Db_Data.Null_Uprate_Assumption record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String; Run_Id : integer; Target : Uprate_Targets ) return Wsc_Db_Data.Uprate_Assumption is
      l : Wsc_Db_Data.Uprate_Assumption_List.Vector;
      Uprate_Assumption : Wsc_Db_Data.Uprate_Assumption;
      c : d.Criteria;
   begin      
      Add_Username( c, Username );
      Add_Run_Id( c, Run_Id );
      Add_Target( c, Target );
      l := Retrieve( c );
      if( not Wsc_Db_Data.Uprate_Assumption_List.is_empty( l ) ) then
         Uprate_Assumption := Wsc_Db_Data.Uprate_Assumption_List.First_Element( l );
      else
         Uprate_Assumption := Wsc_Db_Data.Null_Uprate_Assumption;
      end if;
      return Uprate_Assumption;
   end Retrieve_By_PK;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Uprate_Assumption matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Uprate_Assumption_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Uprate_Assumption retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Uprate_Assumption_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.Uprate_Assumption_List.Vector;
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
      Percent_Change: aliased Real;
      Use_Obr: aliased Integer;
      Target: aliased Integer;
      Element: aliased Integer;
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'access;
      Percent_Change_access : Real_Access := Percent_Change'access;
      --
      -- length holders for each retrieved variable
      --
      Username_len : aliased SQLLEN := Username'Size/8;
      Run_Id_len : aliased SQLLEN := Run_Id'Size/8;
      Percent_Change_len : aliased SQLLEN := Percent_Change'Size/8;
      Use_Obr_len : aliased SQLLEN := Use_Obr'Size/8;
      Target_len : aliased SQLLEN := Target'Size/8;
      Element_len : aliased SQLLEN := Element'Size/8;
      Uprate_Assumption : Wsc_Db_Data.Uprate_Assumption;
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
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 3,
            TargetValuePtr   => To_SQLPOINTER( Percent_Change_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Percent_Change_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetValue      => Use_Obr'access,
            IndPtr           => Use_Obr_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 5,
            TargetValue      => Target'access,
            IndPtr           => Target_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 6,
            TargetValue      => Element'access,
            IndPtr           => Element_len'access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Uprate_Assumption.Username := Slice_To_Unbounded( Username, 1, Natural( Username_len ) );
            Uprate_Assumption.Run_Id := Run_Id;
            Uprate_Assumption.Percent_Change:= Real( Percent_Change_access.all );
            Uprate_Assumption.Use_Obr := Boolean'Val( Use_Obr );
            Uprate_Assumption.Target := Uprate_Targets'Val( Target );
            Uprate_Assumption.Element := Forecast_Element'Val( Element );
            Wsc_Db_Data.Uprate_Assumption_List.append( l, Uprate_Assumption );        
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
   procedure Update( Uprate_Assumption : Wsc_Db_Data.Uprate_Assumption ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Percent_Change( values_c, Uprate_Assumption.Percent_Change );
      Add_Use_Obr( values_c, Uprate_Assumption.Use_Obr );
      Add_Element( values_c, Uprate_Assumption.Element );
      --
      -- primary key fields
      --
      Add_Username( pk_c, Uprate_Assumption.Username );
      Add_Run_Id( pk_c, Uprate_Assumption.Run_Id );
      Add_Target( pk_c, Uprate_Assumption.Target );
      query := query & d.To_String( values_c, "," ) & d.to_string( pk_c );
      Log( "update; executing query" & To_String(query) );
      begin -- exception block      
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         Log( "update; execute query OK" );
      exception 
         when No_Data => Null; -- ignore if no updates made
         when Error : others =>
            Log( "update: failed with message " & Exception_Information(Error)  );
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
   procedure Save( Uprate_Assumption : Wsc_Db_Data.Uprate_Assumption; overwrite : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      Uprate_Assumption_Tmp : Wsc_Db_Data.Uprate_Assumption;
   begin
      if( overwrite ) then
         Uprate_Assumption_Tmp := retrieve_By_PK( Uprate_Assumption.Username, Uprate_Assumption.Run_Id, Uprate_Assumption.Target );
         if( not is_Null( Uprate_Assumption_Tmp )) then
            Update( Uprate_Assumption );
            return;
         end if;
      end if;
      Add_Username( c, Uprate_Assumption.Username );
      Add_Run_Id( c, Uprate_Assumption.Run_Id );
      Add_Percent_Change( c, Uprate_Assumption.Percent_Change );
      Add_Use_Obr( c, Uprate_Assumption.Use_Obr );
      Add_Target( c, Uprate_Assumption.Target );
      Add_Element( c, Uprate_Assumption.Element );
      query := query & "( "  & d.To_Crude_Array_Of_Values( c ) & " )";
      Log( "save; executing query" & To_String(query) );
      begin
         connection := Connection_Pool.Lease;
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         Log( "save; execute query OK" );
      exception 
         when Error : others =>
            Log( "save; execute query failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "save: exception thrown " & Exception_Information(Error) );
      end;
      begin
         dodbc.Close_Prepared_Statement( ps );
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            Log( "save/close " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "save/close: exception " & Exception_Information(Error) );
      end;
      
   end Save;


   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Uprate_Assumption
   --

   procedure Delete( Uprate_Assumption : in out Wsc_Db_Data.Uprate_Assumption ) is
         c : d.Criteria;
   begin  
      Add_Username( c, Uprate_Assumption.Username );
      Add_Run_Id( c, Uprate_Assumption.Run_Id );
      Add_Target( c, Uprate_Assumption.Target );
      delete( c );
      Uprate_Assumption := Wsc_Db_Data.Null_Uprate_Assumption;
      Log( "delete record; execute query OK" );
   end Delete;


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
            Log( "delete; execute query failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "delete: exception thrown " & Exception_Information(Error) );
      end;
      begin -- try catch block for close
         dodbc.Close_Prepared_Statement( ps );
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            Log( "delete; execute query failed with message " & Exception_Information(Error)  );
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


   procedure Add_Percent_Change( c : in out d.Criteria; Percent_Change : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "percent_change", op, join, Percent_Change );
   begin
      d.add_to_criteria( c, elem );
   end Add_Percent_Change;


   procedure Add_Use_Obr( c : in out d.Criteria; Use_Obr : boolean; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "use_obr", op, join, Integer( Boolean'Pos( Use_Obr )));
   begin
      d.add_to_criteria( c, elem );
   end Add_Use_Obr;


   procedure Add_Target( c : in out d.Criteria; Target : Uprate_Targets; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "target", op, join, Integer( Uprate_Targets'Pos( Target )));
   begin
      d.add_to_criteria( c, elem );
   end Add_Target;


   procedure Add_Element( c : in out d.Criteria; Element : Forecast_Element; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "element", op, join, Integer( Forecast_Element'Pos( Element )));
   begin
      d.add_to_criteria( c, elem );
   end Add_Element;


   
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


   procedure Add_Percent_Change_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "percent_change", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Percent_Change_To_Orderings;


   procedure Add_Use_Obr_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "use_obr", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Use_Obr_To_Orderings;


   procedure Add_Target_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "target", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Target_To_Orderings;


   procedure Add_Element_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "element", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Element_To_Orderings;


   
end Uprate_Assumption_IO;
