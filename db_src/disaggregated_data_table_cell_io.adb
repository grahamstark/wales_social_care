--
-- Created by ada_generator.py on 2012-02-22 07:11:49.460559
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
with Base_Model_Types;


package body Disaggregated_Data_Table_Cell_IO is

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
         "run_id, username, model_table_name, row_num, col_num, wave, iteration, system_number, value1, value2," &
         "value3, value4, value5, value6, i, p1, p2, p3 " &
         " from disaggregated_data_table_cell " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into disaggregated_data_table_cell (" &
         "run_id, username, model_table_name, row_num, col_num, wave, iteration, system_number, value1, value2," &
         "value3, value4, value5, value6, i, p1, p2, p3 " &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from disaggregated_data_table_cell ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update disaggregated_data_table_cell set  ";
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "DISAGGREGATED_DATA_TABLE_CELL_IO" );
   
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
      query : constant String := "select max( run_id ) from disaggregated_data_table_cell";
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
   -- Next highest avaiable value of Row_Num - useful for saving  
   --
   function Next_Free_Row_Num return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( row_num ) from disaggregated_data_table_cell";
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
   end Next_Free_Row_Num;


   -- 
   -- Next highest avaiable value of Col_Num - useful for saving  
   --
   function Next_Free_Col_Num return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( col_num ) from disaggregated_data_table_cell";
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
   end Next_Free_Col_Num;


   -- 
   -- Next highest avaiable value of Iteration - useful for saving  
   --
   function Next_Free_Iteration return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( iteration ) from disaggregated_data_table_cell";
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
   -- Next highest avaiable value of System_Number - useful for saving  
   --
   function Next_Free_System_Number return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( system_number ) from disaggregated_data_table_cell";
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
   end Next_Free_System_Number;



   --
   -- returns true if the primary key parts of Disaggregated_Data_Table_Cell match the defaults in Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell
   --
   --
   -- Does this Disaggregated_Data_Table_Cell equal the default Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell ?
   --
   function Is_Null( Disaggregated_Data_Table_Cell : Wsc_Db_Data.Disaggregated_Data_Table_Cell ) return Boolean is
   use Wsc_Db_Data;
   begin
      return Disaggregated_Data_Table_Cell = Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell;
   end Is_Null;


   
   --
   -- returns the single Disaggregated_Data_Table_Cell matching the primary key fields, or the Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell record
   -- if no such record exists
   --
   function Retrieve_By_PK( Run_Id : integer; Username : Unbounded_String; Model_Table_Name : Unbounded_String; Row_Num : integer; Col_Num : integer; Wave : Unbounded_String; Iteration : integer; System_Number : integer ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell is
      l : Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.Vector;
      Disaggregated_Data_Table_Cell : Wsc_Db_Data.Disaggregated_Data_Table_Cell;
      c : d.Criteria;
   begin      
      Add_Run_Id( c, Run_Id );
      Add_Username( c, Username );
      Add_Model_Table_Name( c, Model_Table_Name );
      Add_Row_Num( c, Row_Num );
      Add_Col_Num( c, Col_Num );
      Add_Wave( c, Wave );
      Add_Iteration( c, Iteration );
      Add_System_Number( c, System_Number );
      l := Retrieve( c );
      if( not Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.is_empty( l ) ) then
         Disaggregated_Data_Table_Cell := Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.First_Element( l );
      else
         Disaggregated_Data_Table_Cell := Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell;
      end if;
      return Disaggregated_Data_Table_Cell;
   end Retrieve_By_PK;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table_Cell matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table_Cell retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.Vector;
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
      Row_Num: aliased integer;
      Col_Num: aliased integer;
      Wave: aliased String := 
            "@@";
      Iteration: aliased integer;
      System_Number: aliased integer;
      Value1: aliased Real;
      Value2: aliased Real;
      Value3: aliased Real;
      Value4: aliased Real;
      Value5: aliased Real;
      Value6: aliased Real;
      I: aliased Big_Integer;
      P1: aliased integer;
      P2: aliased integer;
      P3: aliased integer;
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'access;
      Model_Table_Name_access : String_Access := Model_Table_Name'access;
      Wave_access : String_Access := Wave'access;
      Value1_access : Real_Access := Value1'access;
      Value2_access : Real_Access := Value2'access;
      Value3_access : Real_Access := Value3'access;
      Value4_access : Real_Access := Value4'access;
      Value5_access : Real_Access := Value5'access;
      Value6_access : Real_Access := Value6'access;
      --
      -- length holders for each retrieved variable
      --
      Run_Id_len : aliased SQLLEN := Run_Id'Size/8;
      Username_len : aliased SQLLEN := Username'Size/8;
      Model_Table_Name_len : aliased SQLLEN := Model_Table_Name'Size/8;
      Row_Num_len : aliased SQLLEN := Row_Num'Size/8;
      Col_Num_len : aliased SQLLEN := Col_Num'Size/8;
      Wave_len : aliased SQLLEN := Wave'Size/8;
      Iteration_len : aliased SQLLEN := Iteration'Size/8;
      System_Number_len : aliased SQLLEN := System_Number'Size/8;
      Value1_len : aliased SQLLEN := Value1'Size/8;
      Value2_len : aliased SQLLEN := Value2'Size/8;
      Value3_len : aliased SQLLEN := Value3'Size/8;
      Value4_len : aliased SQLLEN := Value4'Size/8;
      Value5_len : aliased SQLLEN := Value5'Size/8;
      Value6_len : aliased SQLLEN := Value6'Size/8;
      I_len : aliased SQLLEN := I'Size/8;
      P1_len : aliased SQLLEN := P1'Size/8;
      P2_len : aliased SQLLEN := P2'Size/8;
      P3_len : aliased SQLLEN := P3'Size/8;
      Disaggregated_Data_Table_Cell : Wsc_Db_Data.Disaggregated_Data_Table_Cell;
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
            TargetValue      => Row_Num'access,
            IndPtr           => Row_Num_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 5,
            TargetValue      => Col_Num'access,
            IndPtr           => Col_Num_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 6,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Wave_access.all'address ),
            BufferLength     => Wave_len,
            StrLen_Or_IndPtr => Wave_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 7,
            TargetValue      => Iteration'access,
            IndPtr           => Iteration_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 8,
            TargetValue      => System_Number'access,
            IndPtr           => System_Number_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 9,
            TargetValuePtr   => To_SQLPOINTER( Value1_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Value1_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 10,
            TargetValuePtr   => To_SQLPOINTER( Value2_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Value2_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 11,
            TargetValuePtr   => To_SQLPOINTER( Value3_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Value3_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 12,
            TargetValuePtr   => To_SQLPOINTER( Value4_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Value4_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 13,
            TargetValuePtr   => To_SQLPOINTER( Value5_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Value5_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 14,
            TargetValuePtr   => To_SQLPOINTER( Value6_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Value6_len'access );
         dodbc.L_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 15,
            TargetValue      => I'access,
            IndPtr           => I_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 16,
            TargetValue      => P1'access,
            IndPtr           => P1_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 17,
            TargetValue      => P2'access,
            IndPtr           => P2_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 18,
            TargetValue      => P3'access,
            IndPtr           => P3_len'access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Disaggregated_Data_Table_Cell.Run_Id := Run_Id;
            Disaggregated_Data_Table_Cell.Username := Slice_To_Unbounded( Username, 1, Natural( Username_len ) );
            Disaggregated_Data_Table_Cell.Model_Table_Name := Slice_To_Unbounded( Model_Table_Name, 1, Natural( Model_Table_Name_len ) );
            Disaggregated_Data_Table_Cell.Row_Num := Row_Num;
            Disaggregated_Data_Table_Cell.Col_Num := Col_Num;
            Disaggregated_Data_Table_Cell.Wave := Slice_To_Unbounded( Wave, 1, Natural( Wave_len ) );
            Disaggregated_Data_Table_Cell.Iteration := Iteration;
            Disaggregated_Data_Table_Cell.System_Number := System_Number;
            Disaggregated_Data_Table_Cell.Value1:= Real( Value1_access.all );
            Disaggregated_Data_Table_Cell.Value2:= Real( Value2_access.all );
            Disaggregated_Data_Table_Cell.Value3:= Real( Value3_access.all );
            Disaggregated_Data_Table_Cell.Value4:= Real( Value4_access.all );
            Disaggregated_Data_Table_Cell.Value5:= Real( Value5_access.all );
            Disaggregated_Data_Table_Cell.Value6:= Real( Value6_access.all );
            Disaggregated_Data_Table_Cell.I := I;
            Disaggregated_Data_Table_Cell.P1 := P1;
            Disaggregated_Data_Table_Cell.P2 := P2;
            Disaggregated_Data_Table_Cell.P3 := P3;
            Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.append( l, Disaggregated_Data_Table_Cell );        
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
   procedure Update( Disaggregated_Data_Table_Cell : Wsc_Db_Data.Disaggregated_Data_Table_Cell ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Value1( values_c, Disaggregated_Data_Table_Cell.Value1 );
      Add_Value2( values_c, Disaggregated_Data_Table_Cell.Value2 );
      Add_Value3( values_c, Disaggregated_Data_Table_Cell.Value3 );
      Add_Value4( values_c, Disaggregated_Data_Table_Cell.Value4 );
      Add_Value5( values_c, Disaggregated_Data_Table_Cell.Value5 );
      Add_Value6( values_c, Disaggregated_Data_Table_Cell.Value6 );
      Add_I( values_c, Disaggregated_Data_Table_Cell.I );
      Add_P1( values_c, Disaggregated_Data_Table_Cell.P1 );
      Add_P2( values_c, Disaggregated_Data_Table_Cell.P2 );
      Add_P3( values_c, Disaggregated_Data_Table_Cell.P3 );
      --
      -- primary key fields
      --
      Add_Run_Id( pk_c, Disaggregated_Data_Table_Cell.Run_Id );
      Add_Username( pk_c, Disaggregated_Data_Table_Cell.Username );
      Add_Model_Table_Name( pk_c, Disaggregated_Data_Table_Cell.Model_Table_Name );
      Add_Row_Num( pk_c, Disaggregated_Data_Table_Cell.Row_Num );
      Add_Col_Num( pk_c, Disaggregated_Data_Table_Cell.Col_Num );
      Add_Wave( pk_c, Disaggregated_Data_Table_Cell.Wave );
      Add_Iteration( pk_c, Disaggregated_Data_Table_Cell.Iteration );
      Add_System_Number( pk_c, Disaggregated_Data_Table_Cell.System_Number );
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

   procedure Truncate_Values( Disaggregated_Data_Table_Cell : in out Wsc_Db_Data.Disaggregated_Data_Table_Cell ) is
      
      use type Base_Model_Types.Real;
      
      function Near_Zero_Round( m : Real ) return Real is
      begin
         if( m > -0.000001 ) and ( m < 0.000001 )then
            return 0.0;
         end if;
         return m;
      end Near_Zero_Round;

   begin
      Disaggregated_Data_Table_Cell.Value1 := Near_Zero_Round( Disaggregated_Data_Table_Cell.Value1 );
      Disaggregated_Data_Table_Cell.Value2 := Near_Zero_Round( Disaggregated_Data_Table_Cell.Value2 );
      Disaggregated_Data_Table_Cell.Value3 := Near_Zero_Round( Disaggregated_Data_Table_Cell.Value3 );
      Disaggregated_Data_Table_Cell.Value4 := Near_Zero_Round( Disaggregated_Data_Table_Cell.Value4 );
      Disaggregated_Data_Table_Cell.Value5 := Near_Zero_Round( Disaggregated_Data_Table_Cell.Value5 );
      Disaggregated_Data_Table_Cell.Value6 := Near_Zero_Round( Disaggregated_Data_Table_Cell.Value6 );
   end Truncate_Values;

   --
   -- Save the compelete given record 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Disaggregated_Data_Table_Cell : in out Wsc_Db_Data.Disaggregated_Data_Table_Cell; overwrite : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      Disaggregated_Data_Table_Cell_Tmp : Wsc_Db_Data.Disaggregated_Data_Table_Cell;
   begin
      Truncate_Values( Disaggregated_Data_Table_Cell );
      if( overwrite ) then
         Disaggregated_Data_Table_Cell_Tmp := retrieve_By_PK( Disaggregated_Data_Table_Cell.Run_Id, Disaggregated_Data_Table_Cell.Username, Disaggregated_Data_Table_Cell.Model_Table_Name, Disaggregated_Data_Table_Cell.Row_Num, Disaggregated_Data_Table_Cell.Col_Num, Disaggregated_Data_Table_Cell.Wave, Disaggregated_Data_Table_Cell.Iteration, Disaggregated_Data_Table_Cell.System_Number );
         if( not is_Null( Disaggregated_Data_Table_Cell_Tmp )) then
            Update( Disaggregated_Data_Table_Cell );
            return;
         end if;
      end if;
      Add_Run_Id( c, Disaggregated_Data_Table_Cell.Run_Id );
      Add_Username( c, Disaggregated_Data_Table_Cell.Username );
      Add_Model_Table_Name( c, Disaggregated_Data_Table_Cell.Model_Table_Name );
      Add_Row_Num( c, Disaggregated_Data_Table_Cell.Row_Num );
      Add_Col_Num( c, Disaggregated_Data_Table_Cell.Col_Num );
      Add_Wave( c, Disaggregated_Data_Table_Cell.Wave );
      Add_Iteration( c, Disaggregated_Data_Table_Cell.Iteration );
      Add_System_Number( c, Disaggregated_Data_Table_Cell.System_Number );
      Add_Value1( c, Disaggregated_Data_Table_Cell.Value1 );
      Add_Value2( c, Disaggregated_Data_Table_Cell.Value2 );
      Add_Value3( c, Disaggregated_Data_Table_Cell.Value3 );
      Add_Value4( c, Disaggregated_Data_Table_Cell.Value4 );
      Add_Value5( c, Disaggregated_Data_Table_Cell.Value5 );
      Add_Value6( c, Disaggregated_Data_Table_Cell.Value6 );
      Add_I( c, Disaggregated_Data_Table_Cell.I );
      Add_P1( c, Disaggregated_Data_Table_Cell.P1 );
      Add_P2( c, Disaggregated_Data_Table_Cell.P2 );
      Add_P3( c, Disaggregated_Data_Table_Cell.P3 );
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
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell
   --

   procedure Delete( Disaggregated_Data_Table_Cell : in out Wsc_Db_Data.Disaggregated_Data_Table_Cell ) is
         c : d.Criteria;
   begin  
      Add_Run_Id( c, Disaggregated_Data_Table_Cell.Run_Id );
      Add_Username( c, Disaggregated_Data_Table_Cell.Username );
      Add_Model_Table_Name( c, Disaggregated_Data_Table_Cell.Model_Table_Name );
      Add_Row_Num( c, Disaggregated_Data_Table_Cell.Row_Num );
      Add_Col_Num( c, Disaggregated_Data_Table_Cell.Col_Num );
      Add_Wave( c, Disaggregated_Data_Table_Cell.Wave );
      Add_Iteration( c, Disaggregated_Data_Table_Cell.Iteration );
      Add_System_Number( c, Disaggregated_Data_Table_Cell.System_Number );
      delete( c );
      Disaggregated_Data_Table_Cell := Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell;
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


   procedure Add_Row_Num( c : in out d.Criteria; Row_Num : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "row_num", op, join, Row_Num );
   begin
      d.add_to_criteria( c, elem );
   end Add_Row_Num;


   procedure Add_Col_Num( c : in out d.Criteria; Col_Num : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "col_num", op, join, Col_Num );
   begin
      d.add_to_criteria( c, elem );
   end Add_Col_Num;


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


   procedure Add_Iteration( c : in out d.Criteria; Iteration : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "iteration", op, join, Iteration );
   begin
      d.add_to_criteria( c, elem );
   end Add_Iteration;


   procedure Add_System_Number( c : in out d.Criteria; System_Number : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "system_number", op, join, System_Number );
   begin
      d.add_to_criteria( c, elem );
   end Add_System_Number;


   procedure Add_Value1( c : in out d.Criteria; Value1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "value1", op, join, Value1 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value1;


   procedure Add_Value2( c : in out d.Criteria; Value2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "value2", op, join, Value2 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value2;


   procedure Add_Value3( c : in out d.Criteria; Value3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "value3", op, join, Value3 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value3;


   procedure Add_Value4( c : in out d.Criteria; Value4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "value4", op, join, Value4 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value4;


   procedure Add_Value5( c : in out d.Criteria; Value5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "value5", op, join, Value5 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value5;


   procedure Add_Value6( c : in out d.Criteria; Value6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "value6", op, join, Value6 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value6;


   procedure Add_I( c : in out d.Criteria; I : Big_Integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "i", op, join, I );
   begin
      d.add_to_criteria( c, elem );
   end Add_I;


   procedure Add_P1( c : in out d.Criteria; P1 : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "p1", op, join, P1 );
   begin
      d.add_to_criteria( c, elem );
   end Add_P1;


   procedure Add_P2( c : in out d.Criteria; P2 : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "p2", op, join, P2 );
   begin
      d.add_to_criteria( c, elem );
   end Add_P2;


   procedure Add_P3( c : in out d.Criteria; P3 : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "p3", op, join, P3 );
   begin
      d.add_to_criteria( c, elem );
   end Add_P3;


   
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


   procedure Add_Row_Num_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "row_num", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Row_Num_To_Orderings;


   procedure Add_Col_Num_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "col_num", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Col_Num_To_Orderings;


   procedure Add_Wave_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "wave", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Wave_To_Orderings;


   procedure Add_Iteration_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "iteration", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Iteration_To_Orderings;


   procedure Add_System_Number_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "system_number", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_System_Number_To_Orderings;


   procedure Add_Value1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "value1", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value1_To_Orderings;


   procedure Add_Value2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "value2", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value2_To_Orderings;


   procedure Add_Value3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "value3", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value3_To_Orderings;


   procedure Add_Value4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "value4", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value4_To_Orderings;


   procedure Add_Value5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "value5", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value5_To_Orderings;


   procedure Add_Value6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "value6", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Value6_To_Orderings;


   procedure Add_I_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "i", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_I_To_Orderings;


   procedure Add_P1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "p1", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_P1_To_Orderings;


   procedure Add_P2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "p2", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_P2_To_Orderings;


   procedure Add_P3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "p3", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_P3_To_Orderings;


   
end Disaggregated_Data_Table_Cell_IO;
