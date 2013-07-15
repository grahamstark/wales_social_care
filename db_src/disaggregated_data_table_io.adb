--
-- Created by ada_generator.py on 2012-02-12 11:09:11.287004
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

with Disaggregated_Data_Table_Cell_IO;
with Text_Utils;

with GNATColl.Traces;

package body Disaggregated_Data_Table_IO is

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
         "run_id, username, model_table_name, iteration " &
         " from disaggregated_data_table " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into disaggregated_data_table (" &
         "run_id, username, model_table_name, iteration " &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from disaggregated_data_table ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update disaggregated_data_table set  ";
   
      
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "DISAGGREGATED_DATA_TABLE_IO" );
   
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
      query : constant String := "select max( run_id ) from disaggregated_data_table";
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
   -- Next highest avaiable value of Iteration - useful for saving  
   --
   function Next_Free_Iteration return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( iteration ) from disaggregated_data_table";
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
   end Next_Free_Iteration;



   --
   -- returns true if the primary key parts of Disaggregated_Data_Table match the defaults in Wsc_Db_Data.Null_Disaggregated_Data_Table
   --
   --
   -- Does this Disaggregated_Data_Table equal the default Wsc_Db_Data.Null_Disaggregated_Data_Table ?
   --
   function Is_Null( Disaggregated_Data_Table : Wsc_Db_Data.Disaggregated_Data_Table ) return Boolean is
   use Wsc_Db_Data;
   begin
      return Disaggregated_Data_Table = Wsc_Db_Data.Null_Disaggregated_Data_Table;
   end Is_Null;


   
   --
   -- returns the single Disaggregated_Data_Table matching the primary key fields, or the Wsc_Db_Data.Null_Disaggregated_Data_Table record
   -- if no such record exists
   --
   function Retrieve_By_PK( Run_Id : integer; Username : Unbounded_String; Model_Table_Name : Unbounded_String; Iteration : integer ) return Wsc_Db_Data.Disaggregated_Data_Table is
      l : Wsc_Db_Data.Disaggregated_Data_Table_List.Vector;
      Disaggregated_Data_Table : Wsc_Db_Data.Disaggregated_Data_Table;
      c : d.Criteria;
   begin      
      Add_Run_Id( c, Run_Id );
      Add_Username( c, Username );
      Add_Model_Table_Name( c, Model_Table_Name );
      Add_Iteration( c, Iteration );
      l := Retrieve( c );
      if( not Wsc_Db_Data.Disaggregated_Data_Table_List.is_empty( l ) ) then
         Disaggregated_Data_Table := Wsc_Db_Data.Disaggregated_Data_Table_List.First_Element( l );
      else
         Disaggregated_Data_Table := Wsc_Db_Data.Null_Disaggregated_Data_Table;
      end if;
      return Disaggregated_Data_Table;
   end Retrieve_By_PK;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Disaggregated_Data_Table_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Disaggregated_Data_Table_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.Disaggregated_Data_Table_List.Vector;
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
      Model_Table_Name: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Iteration: aliased integer;
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'access;
      Model_Table_Name_access : String_Access := Model_Table_Name'access;
      --
      -- length holders for each retrieved variable
      --
      Run_Id_len : aliased SQLLEN := Run_Id'Size/8;
      Username_len : aliased SQLLEN := Username'Size/8;
      Model_Table_Name_len : aliased SQLLEN := Model_Table_Name'Size/8;
      Iteration_len : aliased SQLLEN := Iteration'Size/8;
      Disaggregated_Data_Table : Wsc_Db_Data.Disaggregated_Data_Table;
   begin
      Log( "retrieve made this as query " & query );
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
            TargetValuePtr   => To_SQLPOINTER( Model_Table_Name_access.all'address ),
            BufferLength     => Model_Table_Name_len,
            StrLen_Or_IndPtr => Model_Table_Name_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetValue      => Iteration'access,
            IndPtr           => Iteration_len'access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Disaggregated_Data_Table.Run_Id := Run_Id;
            Disaggregated_Data_Table.Username := Slice_To_Unbounded( Username, 1, Natural( Username_len ) );
            Disaggregated_Data_Table.Model_Table_Name := Slice_To_Unbounded( Model_Table_Name, 1, Natural( Model_Table_Name_len ) );
            Disaggregated_Data_Table.Iteration := Iteration;
            Wsc_Db_Data.Disaggregated_Data_Table_List.append( l, Disaggregated_Data_Table );        
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
   procedure Update( Disaggregated_Data_Table : Wsc_Db_Data.Disaggregated_Data_Table ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      --
      -- primary key fields
      --
      Add_Run_Id( pk_c, Disaggregated_Data_Table.Run_Id );
      Add_Username( pk_c, Disaggregated_Data_Table.Username );
      Add_Model_Table_Name( pk_c, Disaggregated_Data_Table.Model_Table_Name );
      Add_Iteration( pk_c, Disaggregated_Data_Table.Iteration );
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
   procedure Save( Disaggregated_Data_Table : Wsc_Db_Data.Disaggregated_Data_Table; overwrite : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      Disaggregated_Data_Table_Tmp : Wsc_Db_Data.Disaggregated_Data_Table;
   begin
      if( overwrite ) then
         Disaggregated_Data_Table_Tmp := retrieve_By_PK( Disaggregated_Data_Table.Run_Id, Disaggregated_Data_Table.Username, Disaggregated_Data_Table.Model_Table_Name, Disaggregated_Data_Table.Iteration );
         if( not is_Null( Disaggregated_Data_Table_Tmp )) then
            -- no fields could be updated since all in PK Update( Disaggregated_Data_Table );
            return;
         end if;
      end if;
      Add_Run_Id( c, Disaggregated_Data_Table.Run_Id );
      Add_Username( c, Disaggregated_Data_Table.Username );
      Add_Model_Table_Name( c, Disaggregated_Data_Table.Model_Table_Name );
      Add_Iteration( c, Disaggregated_Data_Table.Iteration );
      query := query & "( "  & d.To_Crude_Array_Of_Values( c ) & " )";
      Log( "save; executing query" & To_String(query) );
      begin
         connection := Connection_Pool.Lease;
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
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
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            GNATColl.Traces.Trace( log_trace, error, "save/close " );
            Raise_Exception( d.DB_Exception'Identity, 
               "save/close: exception " & Exception_Information(Error) );
      end;
      
   end Save;


   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Disaggregated_Data_Table
   --

   procedure Delete( Disaggregated_Data_Table : in out Wsc_Db_Data.Disaggregated_Data_Table ) is
         c : d.Criteria;
   begin  
      Add_Run_Id( c, Disaggregated_Data_Table.Run_Id );
      Add_Username( c, Disaggregated_Data_Table.Username );
      Add_Model_Table_Name( c, Disaggregated_Data_Table.Model_Table_Name );
      Add_Iteration( c, Disaggregated_Data_Table.Iteration );
      delete( c );
      Disaggregated_Data_Table := Wsc_Db_Data.Null_Disaggregated_Data_Table;
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
   function Retrieve_Associated_Disaggregated_Data_Table_Cells( Disaggregated_Data_Table : Wsc_Db_Data.Disaggregated_Data_Table ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.Vector is
      c : d.Criteria;
   begin
      Disaggregated_Data_Table_Cell_IO.Add_Run_Id( c, Disaggregated_Data_Table.Run_Id );
      Disaggregated_Data_Table_Cell_IO.Add_Username( c, Disaggregated_Data_Table.Username );
      Disaggregated_Data_Table_Cell_IO.Add_Iteration( c, Disaggregated_Data_Table.Iteration );
      Disaggregated_Data_Table_Cell_IO.Add_Model_Table_Name( c, Disaggregated_Data_Table.Model_Table_Name );
      return Disaggregated_Data_Table_Cell_IO.retrieve( c );
   end Retrieve_Associated_Disaggregated_Data_Table_Cells;

   --
   -- functions to retrieve records from tables with foreign keys
   -- referencing the table modelled by this package
   --
   function Retrieve_Associated_Disaggregated_Data_Table_Cells( Disaggregated_Data_Table : Wsc_Db_Data.Disaggregated_Data_Table;  sysno : Positive; wave : Waves ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.Vector is
      use Text_Utils;
      c : d.Criteria;
      wave_str : Unbounded_String := TuS( wave'Img );
   begin
      Disaggregated_Data_Table_Cell_IO.Add_Run_Id( c, Disaggregated_Data_Table.Run_Id );
      Disaggregated_Data_Table_Cell_IO.Add_Username( c, Disaggregated_Data_Table.Username );
      Disaggregated_Data_Table_Cell_IO.Add_Iteration( c, Disaggregated_Data_Table.Iteration );
      Disaggregated_Data_Table_Cell_IO.Add_Model_Table_Name( c, Disaggregated_Data_Table.Model_Table_Name );
      Disaggregated_Data_Table_Cell_IO.Add_System_Number( c, sysno );
      Disaggregated_Data_Table_Cell_IO.Add_Wave( c, wave_str );
      return Disaggregated_Data_Table_Cell_IO.retrieve( c );
   end Retrieve_Associated_Disaggregated_Data_Table_Cells;


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


   procedure Add_Model_Table_Name( c : in out d.Criteria; Model_Table_Name : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "model_table_name", op, join, To_String( Model_Table_Name ), 40 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Model_Table_Name;


   procedure Add_Model_Table_Name( c : in out d.Criteria; Model_Table_Name : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "model_table_name", op, join, Model_Table_Name, 40 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Model_Table_Name;


   procedure Add_Iteration( c : in out d.Criteria; Iteration : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "iteration", op, join, Iteration );
   begin
      d.add_to_criteria( c, elem );
   end Add_Iteration;


   
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


   procedure Add_Model_Table_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "model_table_name", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Model_Table_Name_To_Orderings;


   procedure Add_Iteration_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "iteration", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Iteration_To_Orderings;


   
end Disaggregated_Data_Table_IO;
