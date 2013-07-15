--
-- Created by ada_generator.py on 2012-02-12 11:09:11.281300
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

with GNATColl.Traces;

with Disaggregated_Data_Table_Cell_IO;



package body Disaggregated_Data_Table_Cell_Description_IO is

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
         "model_table_name, cell_pos, cell_label, cell_type, file_pos " &
         " from disaggregated_data_table_cell_description " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into disaggregated_data_table_cell_description (" &
         "model_table_name, cell_pos, cell_label, cell_type, file_pos " &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from disaggregated_data_table_cell_description ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update disaggregated_data_table_cell_description set  ";
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "DISAGGREGATED_DATA_TABLE_CELL_DESCRIPTION_IO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   -- 
   -- Next highest avaiable value of Cell_Pos - useful for saving  
   --
   function Next_Free_Cell_Pos return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( cell_pos ) from disaggregated_data_table_cell_description";
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
   end Next_Free_Cell_Pos;



   --
   -- returns true if the primary key parts of Disaggregated_Data_Table_Cell_Description match the defaults in Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell_Description
   --
   --
   -- Does this Disaggregated_Data_Table_Cell_Description equal the default Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell_Description ?
   --
   function Is_Null( Disaggregated_Data_Table_Cell_Description : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description ) return Boolean is
   use Wsc_Db_Data;
   begin
      return Disaggregated_Data_Table_Cell_Description = Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell_Description;
   end Is_Null;


   
   --
   -- returns the single Disaggregated_Data_Table_Cell_Description matching the primary key fields, or the Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell_Description record
   -- if no such record exists
   --
   function Retrieve_By_PK( Model_Table_Name : Unbounded_String; Cell_Pos : integer ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description is
      l : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description_List.Vector;
      Disaggregated_Data_Table_Cell_Description : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description;
      c : d.Criteria;
   begin      
      Add_Model_Table_Name( c, Model_Table_Name );
      Add_Cell_Pos( c, Cell_Pos );
      l := Retrieve( c );
      if( not Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description_List.is_empty( l ) ) then
         Disaggregated_Data_Table_Cell_Description := Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description_List.First_Element( l );
      else
         Disaggregated_Data_Table_Cell_Description := Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell_Description;
      end if;
      return Disaggregated_Data_Table_Cell_Description;
   end Retrieve_By_PK;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description_List.Vector;
      ps : SQLHDBC := SQL_NULL_HANDLE;
      has_data : Boolean := false;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := SELECT_PART & " " & sqlstr;
      --
      -- aliased local versions of fields 
      --
      Model_Table_Name: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Cell_Pos: aliased integer;
      Cell_Label: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" &
            "@@@@@@@@";
      Cell_Type: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@";
      File_Pos: aliased integer;
      --
      -- access variables for any variables retrieved via access types
      --
      Model_Table_Name_access : String_Access := Model_Table_Name'access;
      Cell_Label_access : String_Access := Cell_Label'access;
      Cell_Type_access : String_Access := Cell_Type'access;
      --
      -- length holders for each retrieved variable
      --
      Model_Table_Name_len : aliased SQLLEN := Model_Table_Name'Size/8;
      Cell_Pos_len : aliased SQLLEN := Cell_Pos'Size/8;
      Cell_Label_len : aliased SQLLEN := Cell_Label'Size/8;
      Cell_Type_len : aliased SQLLEN := Cell_Type'Size/8;
      File_Pos_len : aliased SQLLEN := File_Pos'Size/8;
      Disaggregated_Data_Table_Cell_Description : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description;
   begin
      Log( "retrieve made this as query " & query );
      begin -- exception block
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 1,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Model_Table_Name_access.all'address ),
            BufferLength     => Model_Table_Name_len,
            StrLen_Or_IndPtr => Model_Table_Name_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 2,
            TargetValue      => Cell_Pos'access,
            IndPtr           => Cell_Pos_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 3,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Cell_Label_access.all'address ),
            BufferLength     => Cell_Label_len,
            StrLen_Or_IndPtr => Cell_Label_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Cell_Type_access.all'address ),
            BufferLength     => Cell_Type_len,
            StrLen_Or_IndPtr => Cell_Type_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 5,
            TargetValue      => File_Pos'access,
            IndPtr           => File_Pos_len'access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Disaggregated_Data_Table_Cell_Description.Model_Table_Name := Slice_To_Unbounded( Model_Table_Name, 1, Natural( Model_Table_Name_len ) );
            Disaggregated_Data_Table_Cell_Description.Cell_Pos := Cell_Pos;
            Disaggregated_Data_Table_Cell_Description.Cell_Label := Slice_To_Unbounded( Cell_Label, 1, Natural( Cell_Label_len ) );
            Disaggregated_Data_Table_Cell_Description.Cell_Type := Slice_To_Unbounded( Cell_Type, 1, Natural( Cell_Type_len ) );
            Disaggregated_Data_Table_Cell_Description.File_Pos := File_Pos;
            Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description_List.append( l, Disaggregated_Data_Table_Cell_Description );        
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
   procedure Update( Disaggregated_Data_Table_Cell_Description : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Cell_Label( values_c, Disaggregated_Data_Table_Cell_Description.Cell_Label );
      Add_Cell_Type( values_c, Disaggregated_Data_Table_Cell_Description.Cell_Type );
      Add_File_Pos( values_c, Disaggregated_Data_Table_Cell_Description.File_Pos );
      --
      -- primary key fields
      --
      Add_Model_Table_Name( pk_c, Disaggregated_Data_Table_Cell_Description.Model_Table_Name );
      Add_Cell_Pos( pk_c, Disaggregated_Data_Table_Cell_Description.Cell_Pos );
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
   procedure Save( Disaggregated_Data_Table_Cell_Description : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description; overwrite : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      Disaggregated_Data_Table_Cell_Description_Tmp : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description;
   begin
      if( overwrite ) then
         Disaggregated_Data_Table_Cell_Description_Tmp := retrieve_By_PK( Disaggregated_Data_Table_Cell_Description.Model_Table_Name, Disaggregated_Data_Table_Cell_Description.Cell_Pos );
         if( not is_Null( Disaggregated_Data_Table_Cell_Description_Tmp )) then
            Update( Disaggregated_Data_Table_Cell_Description );
            return;
         end if;
      end if;
      Add_Model_Table_Name( c, Disaggregated_Data_Table_Cell_Description.Model_Table_Name );
      Add_Cell_Pos( c, Disaggregated_Data_Table_Cell_Description.Cell_Pos );
      Add_Cell_Label( c, Disaggregated_Data_Table_Cell_Description.Cell_Label );
      Add_Cell_Type( c, Disaggregated_Data_Table_Cell_Description.Cell_Type );
      Add_File_Pos( c, Disaggregated_Data_Table_Cell_Description.File_Pos );
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
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell_Description
   --

   procedure Delete( Disaggregated_Data_Table_Cell_Description : in out Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description ) is
         c : d.Criteria;
   begin  
      Add_Model_Table_Name( c, Disaggregated_Data_Table_Cell_Description.Model_Table_Name );
      Add_Cell_Pos( c, Disaggregated_Data_Table_Cell_Description.Cell_Pos );
      delete( c );
      Disaggregated_Data_Table_Cell_Description := Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell_Description;
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
   function Retrieve_Associated_Disaggregated_Data_Table_Cells( Disaggregated_Data_Table_Cell_Description : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.Vector is
      c : d.Criteria;
   begin
      Disaggregated_Data_Table_Cell_IO.Add_Model_Table_Name( c, Disaggregated_Data_Table_Cell_Description.Model_Table_Name );
      Disaggregated_Data_Table_Cell_IO.Add_Col_Num( c, Disaggregated_Data_Table_Cell_Description.Cell_Pos );
      return Disaggregated_Data_Table_Cell_IO.retrieve( c );
   end Retrieve_Associated_Disaggregated_Data_Table_Cells;



   --
   -- functions to add something to a criteria
   --
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


   procedure Add_Cell_Pos( c : in out d.Criteria; Cell_Pos : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "cell_pos", op, join, Cell_Pos );
   begin
      d.add_to_criteria( c, elem );
   end Add_Cell_Pos;


   procedure Add_Cell_Label( c : in out d.Criteria; Cell_Label : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "cell_label", op, join, To_String( Cell_Label ), 80 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Cell_Label;


   procedure Add_Cell_Label( c : in out d.Criteria; Cell_Label : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "cell_label", op, join, Cell_Label, 80 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Cell_Label;


   procedure Add_Cell_Type( c : in out d.Criteria; Cell_Type : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "cell_type", op, join, To_String( Cell_Type ), 20 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Cell_Type;


   procedure Add_Cell_Type( c : in out d.Criteria; Cell_Type : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "cell_type", op, join, Cell_Type, 20 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Cell_Type;


   procedure Add_File_Pos( c : in out d.Criteria; File_Pos : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "file_pos", op, join, File_Pos );
   begin
      d.add_to_criteria( c, elem );
   end Add_File_Pos;


   
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Model_Table_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "model_table_name", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Model_Table_Name_To_Orderings;


   procedure Add_Cell_Pos_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "cell_pos", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Cell_Pos_To_Orderings;


   procedure Add_Cell_Label_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "cell_label", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Cell_Label_To_Orderings;


   procedure Add_Cell_Type_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "cell_type", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Cell_Type_To_Orderings;


   procedure Add_File_Pos_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "file_pos", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_File_Pos_To_Orderings;


   
end Disaggregated_Data_Table_Cell_Description_IO;
