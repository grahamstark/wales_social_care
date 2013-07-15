--
-- Created by ada_generator.py on 2012-07-24 19:03:55.768529
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

with Disaggregated_Data_Table_Cell_Description_IO;
with Disaggregated_Data_Table_IO;

with DB_Logger;

package body Disaggregated_Data_Table_Description_IO is

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
         "model_table_name, filename, has_totals, table_type, table_subtype " &
         " from disaggregated_data_table_description " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into disaggregated_data_table_description (" &
         "model_table_name, filename, has_totals, table_type, table_subtype " &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from disaggregated_data_table_description ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update disaggregated_data_table_description set  ";
   
   

   --
   -- returns true if the primary key parts of Disaggregated_Data_Table_Description match the defaults in Wsc_Db_Data.Null_Disaggregated_Data_Table_Description
   --
   --
   -- Does this Disaggregated_Data_Table_Description equal the default Wsc_Db_Data.Null_Disaggregated_Data_Table_Description ?
   --
   function Is_Null( Disaggregated_Data_Table_Description : Wsc_Db_Data.Disaggregated_Data_Table_Description ) return Boolean is
   use Wsc_Db_Data;
   begin
      return Disaggregated_Data_Table_Description = Wsc_Db_Data.Null_Disaggregated_Data_Table_Description;
   end Is_Null;


   
   --
   -- returns the single Disaggregated_Data_Table_Description matching the primary key fields, or the Wsc_Db_Data.Null_Disaggregated_Data_Table_Description record
   -- if no such record exists
   --
   function Retrieve_By_PK( Model_Table_Name : Unbounded_String ) return Wsc_Db_Data.Disaggregated_Data_Table_Description is
      l : Wsc_Db_Data.Disaggregated_Data_Table_Description_List.Vector;
      Disaggregated_Data_Table_Description : Wsc_Db_Data.Disaggregated_Data_Table_Description;
      c : d.Criteria;
   begin      
      Add_Model_Table_Name( c, Model_Table_Name );
      l := Retrieve( c );
      if( not Wsc_Db_Data.Disaggregated_Data_Table_Description_List.is_empty( l ) ) then
         Disaggregated_Data_Table_Description := Wsc_Db_Data.Disaggregated_Data_Table_Description_List.First_Element( l );
      else
         Disaggregated_Data_Table_Description := Wsc_Db_Data.Null_Disaggregated_Data_Table_Description;
      end if;
      return Disaggregated_Data_Table_Description;
   end Retrieve_By_PK;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table_Description matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Disaggregated_Data_Table_Description_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table_Description retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Disaggregated_Data_Table_Description_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.Disaggregated_Data_Table_Description_List.Vector;
      ps : SQLHDBC := SQL_NULL_HANDLE;
      has_data : Boolean := false;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := SELECT_PART & " " & sqlstr;
      --
      -- aliased local versions of fields 
      --
      Model_Table_Name: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Filename: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" &
            "@@@@@@@@";
      Has_Totals: aliased integer;
      Table_Type: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Table_Subtype: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      --
      -- access variables for any variables retrieved via access types
      --
      Model_Table_Name_access : String_Access := Model_Table_Name'Access;
      Filename_access : String_Access := Filename'Access;
      Table_Type_access : String_Access := Table_Type'Access;
      Table_Subtype_access : String_Access := Table_Subtype'Access;
      --
      -- length holders for each retrieved variable
      --
      Model_Table_Name_len : aliased SQLINTEGER := Model_Table_Name'Size/8;
      Filename_len : aliased SQLINTEGER := Filename'Size/8;
      Has_Totals_len : aliased SQLINTEGER := Has_Totals'Size/8;
      Table_Type_len : aliased SQLINTEGER := Table_Type'Size/8;
      Table_Subtype_len : aliased SQLINTEGER := Table_Subtype'Size/8;
      Disaggregated_Data_Table_Description : Wsc_Db_Data.Disaggregated_Data_Table_Description;
   begin
      DB_Logger.info( "retrieve made this as query " & query );
      begin -- exception block
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 1,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Model_Table_Name_access.all'Address ),
            BufferLength     => Model_Table_Name_len,
            StrLen_Or_IndPtr => Model_Table_Name_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 2,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Filename_access.all'Address ),
            BufferLength     => Filename_len,
            StrLen_Or_IndPtr => Filename_len'Access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 3,
            TargetValue      => Has_Totals'access,
            IndPtr           => Has_Totals_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Table_Type_access.all'Address ),
            BufferLength     => Table_Type_len,
            StrLen_Or_IndPtr => Table_Type_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 5,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Table_Subtype_access.all'Address ),
            BufferLength     => Table_Subtype_len,
            StrLen_Or_IndPtr => Table_Subtype_len'Access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Disaggregated_Data_Table_Description.Model_Table_Name := Slice_To_Unbounded( Model_Table_Name, 1, Natural( Model_Table_Name_len ) );
            Disaggregated_Data_Table_Description.Filename := Slice_To_Unbounded( Filename, 1, Natural( Filename_len ) );
            Disaggregated_Data_Table_Description.Has_Totals := Has_Totals;
            Disaggregated_Data_Table_Description.Table_Type := Slice_To_Unbounded( Table_Type, 1, Natural( Table_Type_len ) );
            Disaggregated_Data_Table_Description.Table_Subtype := Slice_To_Unbounded( Table_Subtype, 1, Natural( Table_Subtype_len ) );
            Wsc_Db_Data.Disaggregated_Data_Table_Description_List.append( l, Disaggregated_Data_Table_Description );        
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
   procedure Update( Disaggregated_Data_Table_Description : Wsc_Db_Data.Disaggregated_Data_Table_Description ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Filename( values_c, Disaggregated_Data_Table_Description.Filename );
      Add_Has_Totals( values_c, Disaggregated_Data_Table_Description.Has_Totals );
      Add_Table_Type( values_c, Disaggregated_Data_Table_Description.Table_Type );
      Add_Table_Subtype( values_c, Disaggregated_Data_Table_Description.Table_Subtype );
      --
      -- primary key fields
      --
      Add_Model_Table_Name( pk_c, Disaggregated_Data_Table_Description.Model_Table_Name );
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
   procedure Save( Disaggregated_Data_Table_Description : Wsc_Db_Data.Disaggregated_Data_Table_Description; overwrite : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      Disaggregated_Data_Table_Description_Tmp : Wsc_Db_Data.Disaggregated_Data_Table_Description;
   begin
      if( overwrite ) then
         Disaggregated_Data_Table_Description_Tmp := retrieve_By_PK( Disaggregated_Data_Table_Description.Model_Table_Name );
         if( not is_Null( Disaggregated_Data_Table_Description_Tmp )) then
            Update( Disaggregated_Data_Table_Description );
            return;
         end if;
      end if;
      Add_Model_Table_Name( c, Disaggregated_Data_Table_Description.Model_Table_Name );
      Add_Filename( c, Disaggregated_Data_Table_Description.Filename );
      Add_Has_Totals( c, Disaggregated_Data_Table_Description.Has_Totals );
      Add_Table_Type( c, Disaggregated_Data_Table_Description.Table_Type );
      Add_Table_Subtype( c, Disaggregated_Data_Table_Description.Table_Subtype );
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
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Disaggregated_Data_Table_Description
   --

   procedure Delete( Disaggregated_Data_Table_Description : in out Wsc_Db_Data.Disaggregated_Data_Table_Description ) is
         c : d.Criteria;
   begin  
      Add_Model_Table_Name( c, Disaggregated_Data_Table_Description.Model_Table_Name );
      delete( c );
      Disaggregated_Data_Table_Description := Wsc_Db_Data.Null_Disaggregated_Data_Table_Description;
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
   function Retrieve_Associated_Disaggregated_Data_Table_Cell_Descriptions( Disaggregated_Data_Table_Description : Wsc_Db_Data.Disaggregated_Data_Table_Description ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description_List.Vector is
      c : d.Criteria;
   begin
      Disaggregated_Data_Table_Cell_Description_IO.Add_Model_Table_Name( c, Disaggregated_Data_Table_Description.Model_Table_Name );
      return Disaggregated_Data_Table_Cell_Description_IO.retrieve( c );
   end Retrieve_Associated_Disaggregated_Data_Table_Cell_Descriptions;


   function Retrieve_Associated_Disaggregated_Data_Tables( Disaggregated_Data_Table_Description : Wsc_Db_Data.Disaggregated_Data_Table_Description ) return Wsc_Db_Data.Disaggregated_Data_Table_List.Vector is
      c : d.Criteria;
   begin
      Disaggregated_Data_Table_IO.Add_Model_Table_Name( c, Disaggregated_Data_Table_Description.Model_Table_Name );
      return Disaggregated_Data_Table_IO.retrieve( c );
   end Retrieve_Associated_Disaggregated_Data_Tables;



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


   procedure Add_Filename( c : in out d.Criteria; Filename : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "filename", op, join, To_String( Filename ), 80 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Filename;


   procedure Add_Filename( c : in out d.Criteria; Filename : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "filename", op, join, Filename, 80 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Filename;


   procedure Add_Has_Totals( c : in out d.Criteria; Has_Totals : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "has_totals", op, join, Has_Totals );
   begin
      d.add_to_criteria( c, elem );
   end Add_Has_Totals;


   procedure Add_Table_Type( c : in out d.Criteria; Table_Type : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "table_type", op, join, To_String( Table_Type ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Table_Type;


   procedure Add_Table_Type( c : in out d.Criteria; Table_Type : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "table_type", op, join, Table_Type, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Table_Type;


   procedure Add_Table_Subtype( c : in out d.Criteria; Table_Subtype : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "table_subtype", op, join, To_String( Table_Subtype ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Table_Subtype;


   procedure Add_Table_Subtype( c : in out d.Criteria; Table_Subtype : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "table_subtype", op, join, Table_Subtype, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Table_Subtype;


   
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Model_Table_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "model_table_name", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Model_Table_Name_To_Orderings;


   procedure Add_Filename_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "filename", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Filename_To_Orderings;


   procedure Add_Has_Totals_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "has_totals", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Has_Totals_To_Orderings;


   procedure Add_Table_Type_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "table_type", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Table_Type_To_Orderings;


   procedure Add_Table_Subtype_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "table_subtype", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Table_Subtype_To_Orderings;


   
end Disaggregated_Data_Table_Description_IO;
