--
-- Created by ada_generator.py on 2012-02-14 10:54:30.003328
-- 
with Wsc_Db_Data;

with Strings_Edit.UTF8.Handling;

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

with Ada.Exceptions;  
with Ada.Strings; 
with Ada.Strings.Wide_Fixed;
with Ada.Characters.Conversions;
with Ada.Strings.Unbounded; 
with Text_IO;
with Ada.Strings.Maps;
with Connection_Pool;

with Text_Utils;

with GNATColl.Traces;

package body Key_Value_Parameter_IO is

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
         "username, run_id, key, val " &
         " from key_value_parameter " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into key_value_parameter (" &
         "username, run_id, key, val " &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from key_value_parameter ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update key_value_parameter set  ";
  
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "KEY_VALUE_PARAMETER_IO" );

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
      query : constant String := "select max( run_id ) from key_value_parameter";
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
   -- returns true if the primary key parts of Key_Value_Parameter match the defaults in Wsc_Db_Data.Null_Key_Value_Parameter
   --
   --
   -- Does this Key_Value_Parameter equal the default Wsc_Db_Data.Null_Key_Value_Parameter ?
   --
   function Is_Null( Key_Value_Parameter : Wsc_Db_Data.Key_Value_Parameter ) return Boolean is
   use Wsc_Db_Data;
   begin
      return Key_Value_Parameter = Wsc_Db_Data.Null_Key_Value_Parameter;
   end Is_Null;


   
   --
   -- returns the single Key_Value_Parameter matching the primary key fields, or the Wsc_Db_Data.Null_Key_Value_Parameter record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String; Run_Id : integer; Key : Unbounded_String ) return Wsc_Db_Data.Key_Value_Parameter is
      l : Wsc_Db_Data.Key_Value_Parameter_List.Vector;
      Key_Value_Parameter : Wsc_Db_Data.Key_Value_Parameter;
      c : d.Criteria;
   begin      
      Add_Username( c, Username );
      Add_Run_Id( c, Run_Id );
      Add_Key( c, Key );
      l := Retrieve( c );
      if( not Wsc_Db_Data.Key_Value_Parameter_List.is_empty( l ) ) then
         Key_Value_Parameter := Wsc_Db_Data.Key_Value_Parameter_List.First_Element( l );
      else
         Key_Value_Parameter := Wsc_Db_Data.Null_Key_Value_Parameter;
      end if;
      return Key_Value_Parameter;
   end Retrieve_By_PK;
   

   --
   -- Retrieves a list of Wsc_Db_Data.Key_Value_Parameter matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Key_Value_Parameter_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   
   
   --
   -- Retrieves a list of Wsc_Db_Data.Key_Value_Parameter retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Key_Value_Parameter_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.Key_Value_Parameter_List.Vector;
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
      Key: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" &
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" &
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" &
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Val: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" &
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" &
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" &
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'access;
      Key_access : String_Access := Key'access;
      Val_access : String_Access := Val'access;
      --
      -- length holders for each retrieved variable
      --
      Username_len : aliased SQLLEN := Username'Size/8;
      Run_Id_len : aliased SQLLEN := Run_Id'Size/8;
      Key_len : aliased SQLLEN := Key'Size/8;
      Val_len : aliased SQLLEN := Val'Size/8;
      Key_Value_Parameter : Wsc_Db_Data.Key_Value_Parameter;
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
            ColumnNumber     => 3,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Key_access.all'address ),
            BufferLength     => Key_len,
            StrLen_Or_IndPtr => Key_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Val_access.all'address ),
            BufferLength     => Val_len,
            StrLen_Or_IndPtr => Val_len'access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Key_Value_Parameter.Username := Slice_To_Unbounded( Username, 1, Natural( Username_len ) );
            Key_Value_Parameter.Run_Id := Run_Id;
            Key_Value_Parameter.Key := Slice_To_Unbounded( Key, 1, Natural( Key_len ) );
            Key_Value_Parameter.Val := Slice_To_Unbounded( Val, 1, Natural( Val_len ) );
            Wsc_Db_Data.Key_Value_Parameter_List.append( l, Key_Value_Parameter );        
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
   procedure Update( Key_Value_Parameter : Wsc_Db_Data.Key_Value_Parameter ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Val( values_c, Key_Value_Parameter.Val );
      --
      -- primary key fields
      --
      Add_Username( pk_c, Key_Value_Parameter.Username );
      Add_Run_Id( pk_c, Key_Value_Parameter.Run_Id );
      Add_Key( pk_c, Key_Value_Parameter.Key );
      query := query & d.To_String( values_c, "," ) & d.to_string( pk_c );
      Log( "update; executing query" & To_String(query) );
      begin -- exception block      
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         Log( "update; execute query OK" );
      exception 
         when No_Data => Null; -- ignore if no updates made
         when Error : others =>
            Log( "key_value_io.adb ERROR: update: failed with message " & Exception_Information(Error)  );
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
   procedure Save( Key_Value_Parameter : Wsc_Db_Data.Key_Value_Parameter; overwrite : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      Key_Value_Parameter_Tmp : Wsc_Db_Data.Key_Value_Parameter;
   begin
      -- if( overwrite ) then
         -- Key_Value_Parameter_Tmp := retrieve_By_PK( Key_Value_Parameter.Username, Key_Value_Parameter.Run_Id, Key_Value_Parameter.Key );
         -- if( not is_Null( Key_Value_Parameter_Tmp )) then
            -- Update( Key_Value_Parameter );
            -- return;
         -- end if;
      -- end if;
      Add_Username( c, Key_Value_Parameter.Username );
      Add_Run_Id( c, Key_Value_Parameter.Run_Id );
      Add_Key( c, Key_Value_Parameter.Key );
      Add_Val( c, Key_Value_Parameter.Val );
      query := query & "( "  & d.To_Crude_Array_Of_Values( c ) & " )";
      Log( "save; executing query" & To_String(query) );
      begin
         connection := Connection_Pool.Lease;
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         Log( "save; execute query OK" );
         
      exception 
         when Error : others =>
            Log( "key_value_io.adb ERROR: save; execute query failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "save: exception thrown " & Exception_Information(Error) );
      end;
      begin
         dodbc.Close_Prepared_Statement( ps );
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            Log( "key_value_io.adb ERROR: save/close " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "save/close: exception " & Exception_Information(Error) );
      end;
      
   end Save;

   procedure Bulk_Save( username : Unbounded_String; run_id : Natural; kvs : Text_Buffer ) is   
   use Text_Utils;
   use Ada.Text_IO;
   use Text_Utils.String_Maps_Package;
      ps : SQLHDBC := SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := TuS( "insert into key_value_parameter( username, run_id, key, val ) values ");
      size       : constant Natural := Natural( kvs.Length );
      n          : Natural := 1;
      c : Cursor := kvs.First;
      f          : File_Type;
      ok         : Boolean;
   begin
      loop            
         declare
            crit : d.Criteria;
            k          : Unbounded_String := Key( c );  
            v          : Unbounded_String := Element( c ); 
         begin
            Add_Username( crit, Username );
            Add_Run_Id( crit, Run_Id );
            Add_Key( crit, k );
            Add_Val( crit, v );
            query := query & "( "  & d.To_Crude_Array_Of_Values( crit ) & " )";
            if( n < size )then 
               query := query & ",";
            end if;
            Next( c );
         end;
         exit when n = size;
         n := n + 1;
      end loop;
      Log( "retrieve made this as query " & TS( query ));
      Create( f, Out_File, "/tmp/filexxx.sql" );
      Put_Line( f, TS( query ));
      Close( f );
      ok := DB_Commons.Load_Postgres( "/tmp/filexxx.sql", "/tmp/res" );
      -- begin
         -- connection := Connection_Pool.Lease;
         -- ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         -- SQLExecute( ps );
         -- Log( "save; execute query OK" );
      -- exception 
         -- when Error : others =>
            -- Log( "key_value_io.adb ERROR: "save; execute query failed with message " & Exception_Information(Error)  );
            -- Raise_Exception( d.DB_Exception'Identity, 
               -- "save: exception thrown " & Exception_Information(Error) );
      -- end;
      -- begin
         -- dodbc.Close_Prepared_Statement( ps );
         -- Connection_Pool.Return_Connection( connection );
      -- exception 
         -- when Error : others =>
            -- Log( "key_value_io.adb ERROR: "save/close " & Exception_Information(Error)  );
            -- Raise_Exception( d.DB_Exception'Identity, 
               -- "save/close: exception " & Exception_Information(Error) );
      -- end;
   end Bulk_Save;
   
   
   --
   -- Save the compelete given record 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Bulk_Save2( username : Unbounded_String; run_id : Natural; kvs : Text_Buffer ) is   
   use Text_Utils;
   use Text_Utils.String_Maps_Package;
      ps : SQLHDBC := SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "insert into key_value_parameter( username, run_id, key, val ) values( ?, ?, ?, ? )";
      --
      -- access variables for any variables retrieved via access types
      --
      c : Cursor := kvs.First;
      size       : constant Natural := Natural( kvs.Length );
      n          : Natural := 1;
  begin
      Log( "retrieve made this as query " & query );
      begin -- exception block
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         loop            
            declare
               type String_Access is access all Wide_String;
               a_username : aliased Wide_String := Strings_Edit.UTF8.Handling.To_Wide_String( TS( username ));
               a_run_Id   : aliased Integer := run_id;
               username_access : String_Access := a_username'access;
               Username_len : aliased SQLLEN := a_username'Size/8;
               Run_Id_len : aliased SQLLEN := a_run_Id'Size/8;
               k          : aliased Wide_String := Strings_Edit.UTF8.Handling.To_Wide_String( TS( Key( c )));  
               v          : aliased Wide_String := Strings_Edit.UTF8.Handling.To_Wide_String( TS( Element( c ))); 
               key_len    : aliased SQLLEN := k'Size/8;
               val_len    : aliased SQLLEN := v'Size/8;
               indic      : aliased SQLLEN;
               -- key_access : String_Access := k'access;
               -- val_access : String_Access := v'access;
            begin
             -- Log( "on param |" & k & "| username |" & a_username & "| run_id |" & run_id'Img );
       
             -- SQLBindParameter(
                  -- StatementHandle  => ps,
                  -- ParameterNumber  => 1,
                  -- Value            => a_username'Access,
                  -- Length           => username_len'Access );
              SQLBindParameter(
                 StatementHandle  => ps,
                 ParameterNumber  => 1,
                 InputOutputType  => SQL_PARAM_INPUT,
                 ValueType        => SQL_C_WCHAR,
                 ParameterType    => SQL_WVARCHAR,
                 ColumnSize       => 32,
                 DecimalDigits    => 0,
                 Value            => To_SQLPOINTER( a_username'Access ),
                 BufferLength     => Username_len,
                 StrLen_Or_IndPtr => Username_len'access );
                
            dodbc.I_Out_Binding.SQLBindParameter(
                  StatementHandle  => ps,
                  ParameterNumber  => 2,
                  InputOutputType  => SQL_PARAM_INPUT,
                  Value            => a_run_id'access,
                  Indicator        => indic'access );

             SQLBindParameter(
                  StatementHandle  => ps,
                  ParameterNumber  => 3,
                  InputOutputType  => SQL_PARAM_INPUT,
                  ValueType        => SQL_C_WCHAR,
                  ParameterType    => SQL_WVARCHAR,
                  ColumnSize       => 250,
                  DecimalDigits    => 0,
                  Value            => To_SQLPOINTER( k'Access ),
                  BufferLength     => key_len,
                  StrLen_Or_IndPtr => key_len'access );
             SQLBindParameter(
                  StatementHandle  => ps,
                  ParameterNumber  => 4,
                  InputOutputType  => SQL_PARAM_INPUT,
                  ValueType        => SQL_C_WCHAR,
                  ParameterType    => SQL_WVARCHAR,
                  ColumnSize       => 250,
                  DecimalDigits    => 0,
                  Value            => To_SQLPOINTER( v'Access ), -- s.all'address
                  BufferLength     => val_len,
                  StrLen_Or_IndPtr => val_len'access );
 

               -- SQLBindParameter(
                  -- StatementHandle  => ps,
                  -- ParameterNumber  => 1,
                  -- Value            => a_username'Access,
                  -- Length           => username_len'Access );
                  
               -- SQLBindParameter(
                  -- StatementHandle  => ps,
                  -- ParameterNumber  => 3,
                  -- Value            => k'Access,
                  -- Length           => key_len'Access );
               -- SQLBindParameter(
                  -- StatementHandle  => ps,
                  -- ParameterNumber  => 4,
                  -- Value            => v'Access,
                  -- Length           => val_len'Access );
               SQLExecute( ps );
            end;
            Next( c );
            exit when n = size;
            n := n + 1;
         end loop;
            
         -- kvs.Iterate( Store_One'Access );           
         Log( "retrieve: Query Run OK" );
      exception 
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
   end Bulk_Save2;
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Key_Value_Parameter
   --

   procedure Delete( Key_Value_Parameter : in out Wsc_Db_Data.Key_Value_Parameter ) is
         c : d.Criteria;
   begin  
      Add_Username( c, Key_Value_Parameter.Username );
      Add_Run_Id( c, Key_Value_Parameter.Run_Id );
      Add_Key( c, Key_Value_Parameter.Key );
      delete( c );
      Key_Value_Parameter := Wsc_Db_Data.Null_Key_Value_Parameter;
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
            Log( "key_value_io.adb ERROR: delete; execute query failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "delete: exception thrown " & Exception_Information(Error) );
      end;
      begin -- try catch block for close
         dodbc.Close_Prepared_Statement( ps );
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            Log( "key_value_io.adb ERROR: delete; execute query failed with message " & Exception_Information(Error)  );
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


   procedure Add_Key( c : in out d.Criteria; Key : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "key", op, join, To_String( Key ), 250 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Key;


   procedure Add_Key( c : in out d.Criteria; Key : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "key", op, join, Key, 250 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Key;


   procedure Add_Val( c : in out d.Criteria; Val : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "val", op, join, To_String( Val ), 250 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Val;


   procedure Add_Val( c : in out d.Criteria; Val : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "val", op, join, Val, 250 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Val;


   
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


   procedure Add_Key_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "key", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Key_To_Orderings;


   procedure Add_Val_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "val", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Val_To_Orderings;


   
end Key_Value_Parameter_IO;
