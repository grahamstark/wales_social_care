--
-- Created by ada_generator.py on 2012-05-03 16:03:51.797484
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

with Run_IO;
with GNATColl.Traces;



package body Dataset_IO is

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
         "name, creator, title, run_id " &
         " from dataset " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into dataset (" &
         "name, creator, title, run_id " &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from dataset ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update dataset set  ";
   
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "DATASET_IO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   --
   -- returns true if the primary key parts of Dataset match the defaults in Wsc_Db_Data.Null_Dataset
   --
   --
   -- Does this Dataset equal the default Wsc_Db_Data.Null_Dataset ?
   --
   function Is_Null( ds : Dataset ) return Boolean is
   use Wsc_Db_Data;
   begin
      return ds = Null_Dataset;
   end Is_Null;


   
   --
   -- returns the single Dataset matching the primary key fields, or the Wsc_Db_Data.Null_Dataset record
   -- if no such record exists
   --
   function Retrieve_By_PK( Name : Unbounded_String ) return Dataset is
      l : Dataset_List.Vector;
      ds : Dataset;
      c : d.Criteria;
   begin      
      Add_Name( c, Name );
      l := Retrieve( c );
      if( not Dataset_List.is_empty( l ) ) then
         ds := Dataset_List.First_Element( l );
      else
         ds := Null_Dataset;
      end if;
      return ds;
   end Retrieve_By_PK;

   
   --
   -- Retrieves a list of Dataset matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Dataset_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   
   --
   -- Retrieves a list of Dataset retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Dataset_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Dataset_List.Vector;
      ps : SQLHDBC := SQL_NULL_HANDLE;
      has_data : Boolean := false;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := SELECT_PART & " " & sqlstr;
      --
      -- aliased local versions of fields 
      --
      Name: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Creator: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Title: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Run_Id: aliased integer;
      --
      -- access variables for any variables retrieved via access types
      --
      Name_access : String_Access := Name'access;
      Creator_access : String_Access := Creator'access;
      Title_access : String_Access := Title'access;
      --
      -- length holders for each retrieved variable
      --
      Name_len : aliased SQLLEN := Name'Size/8;
      Creator_len : aliased SQLLEN := Creator'Size/8;
      Title_len : aliased SQLLEN := Title'Size/8;
      Run_Id_len : aliased SQLLEN := Run_Id'Size/8;
      ds : Dataset;
   begin
      Log( "retrieve made this as query " & query );
      begin -- exception block
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 1,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Name_access.all'address ),
            BufferLength     => Name_len,
            StrLen_Or_IndPtr => Name_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 2,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Creator_access.all'address ),
            BufferLength     => Creator_len,
            StrLen_Or_IndPtr => Creator_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 3,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Title_access.all'address ),
            BufferLength     => Title_len,
            StrLen_Or_IndPtr => Title_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetValue      => Run_Id'access,
            IndPtr           => Run_Id_len'access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            ds.Name := Slice_To_Unbounded( Name, 1, Natural( Name_len ) );
            ds.Creator := Slice_To_Unbounded( Creator, 1, Natural( Creator_len ) );
            ds.Title := Slice_To_Unbounded( Title, 1, Natural( Title_len ) );
            ds.Run_Id := Run_Id;
            Dataset_List.append( l, ds );        
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
   procedure Update( ds : Dataset ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Creator( values_c, ds.Creator );
      Add_Title( values_c, ds.Title );
      Add_Run_Id( values_c, ds.Run_Id );
      --
      -- primary key fields
      --
      Add_Name( pk_c, ds.Name );
      query := query & d.To_String( values_c, "," ) & d.to_string( pk_c );
      Log( "update; executing query" & To_String( query ));
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
   procedure Save( ds : Dataset; overwrite : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      Dataset_Tmp : Dataset;
   begin
      if( overwrite ) then
         Dataset_Tmp := retrieve_By_PK( ds.Name );
         if( not is_Null( Dataset_Tmp )) then
            Update( ds );
            return;
         end if;
      end if;
      Add_Name( c, ds.Name );
      Add_Creator( c, ds.Creator );
      Add_Title( c, ds.Title );
      Add_Run_Id( c, ds.Run_Id );
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
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Dataset
   --

   procedure Delete( ds : in out Dataset ) is
      c : d.Criteria;
   begin  
      Add_Name( c, ds.Name );
      delete( c );
      ds := Null_Dataset;
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
   function Retrieve_Associated_Runs( ds : Dataset ) return Run_List.Vector is
      c : d.Criteria;
   begin
      Run_IO.Add_Dataset_Name( c, ds.Name );
      return Run_IO.retrieve( c );
   end Retrieve_Associated_Runs;



   --
   -- functions to add something to a criteria
   --
   procedure Add_Name( c : in out d.Criteria; Name : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "name", op, join, To_String( Name ), 32 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Name;


   procedure Add_Name( c : in out d.Criteria; Name : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "name", op, join, Name, 32 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Name;


   procedure Add_Creator( c : in out d.Criteria; Creator : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "creator", op, join, To_String( Creator ), 32 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Creator;


   procedure Add_Creator( c : in out d.Criteria; Creator : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "creator", op, join, Creator, 32 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Creator;


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


   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "run_id", op, join, Run_Id );
   begin
      d.add_to_criteria( c, elem );
   end Add_Run_Id;


   
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "name", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Name_To_Orderings;


   procedure Add_Creator_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "creator", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Creator_To_Orderings;


   procedure Add_Title_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "title", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Title_To_Orderings;


   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "run_id", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Run_Id_To_Orderings;


   
end Dataset_IO;
