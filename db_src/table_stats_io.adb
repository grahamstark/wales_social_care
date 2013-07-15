--
-- Created by ada_generator.py on 2012-03-09 09:04:51.208964
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
with GNATColl.Traces;

with Ada.Exceptions;  
with Ada.Strings; 
with Ada.Strings.Wide_Fixed;
with Ada.Characters.Conversions;
with Ada.Strings.Unbounded; 
with Text_IO;
with Ada.Strings.Maps;
with Connection_Pool;




package body Table_Stats_IO is

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
         "run_id, username, model_table_name, row_num, col_num, wave, system_number, nvalues, rmean_1, rmin_1," &
         "rmax_1, rmed_1, sddev_1, dec1_1, dec10_1, rmean_2, rmin_2, rmax_2, rmed_2, sddev_2," &
         "dec1_2, dec10_2, rmean_3, rmin_3, rmax_3, rmed_3, sddev_3, dec1_3, dec10_3, rmean_4," &
         "rmin_4, rmax_4, rmed_4, sddev_4, dec1_4, dec10_4, rmean_5, rmin_5, rmax_5, rmed_5," &
         "sddev_5, dec1_5, dec10_5, rmean_6, rmin_6, rmax_6, rmed_6, sddev_6, dec1_6, dec10_6" &
         " from table_stats " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into table_stats (" &
         "run_id, username, model_table_name, row_num, col_num, wave, system_number, nvalues, rmean_1, rmin_1," &
         "rmax_1, rmed_1, sddev_1, dec1_1, dec10_1, rmean_2, rmin_2, rmax_2, rmed_2, sddev_2," &
         "dec1_2, dec10_2, rmean_3, rmin_3, rmax_3, rmed_3, sddev_3, dec1_3, dec10_3, rmean_4," &
         "rmin_4, rmax_4, rmed_4, sddev_4, dec1_4, dec10_4, rmean_5, rmin_5, rmax_5, rmed_5," &
         "sddev_5, dec1_5, dec10_5, rmean_6, rmin_6, rmax_6, rmed_6, sddev_6, dec1_6, dec10_6" &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from table_stats ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update table_stats set  ";
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "TABLE_STATS_IO" );
   
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
      query : constant String := "select max( run_id ) from table_stats";
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
      query : constant String := "select max( row_num ) from table_stats";
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
      query : constant String := "select max( col_num ) from table_stats";
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
   -- Next highest avaiable value of System_Number - useful for saving  
   --
   function Next_Free_System_Number return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( system_number ) from table_stats";
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
   -- returns true if the primary key parts of Table_Stats match the defaults in Wsc_Db_Data.Null_Table_Stats
   --
   --
   -- Does this Table_Stats equal the default Wsc_Db_Data.Null_Table_Stats ?
   --
   function Is_Null( Table_Stats : Wsc_Db_Data.Table_Stats ) return Boolean is
   use Wsc_Db_Data;
   begin
      return Table_Stats = Wsc_Db_Data.Null_Table_Stats;
   end Is_Null;


   
   --
   -- returns the single Table_Stats matching the primary key fields, or the Wsc_Db_Data.Null_Table_Stats record
   -- if no such record exists
   --
   function Retrieve_By_PK( Run_Id : integer; Username : Unbounded_String; Model_Table_Name : Unbounded_String; Row_Num : integer; Col_Num : integer; Wave : Unbounded_String; System_Number : integer ) return Wsc_Db_Data.Table_Stats is
      l : Wsc_Db_Data.Table_Stats_List.Vector;
      Table_Stats : Wsc_Db_Data.Table_Stats;
      c : d.Criteria;
   begin      
      Add_Run_Id( c, Run_Id );
      Add_Username( c, Username );
      Add_Model_Table_Name( c, Model_Table_Name );
      Add_Row_Num( c, Row_Num );
      Add_Col_Num( c, Col_Num );
      Add_Wave( c, Wave );
      Add_System_Number( c, System_Number );
      l := Retrieve( c );
      if( not Wsc_Db_Data.Table_Stats_List.is_empty( l ) ) then
         Table_Stats := Wsc_Db_Data.Table_Stats_List.First_Element( l );
      else
         Table_Stats := Wsc_Db_Data.Null_Table_Stats;
      end if;
      return Table_Stats;
   end Retrieve_By_PK;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Table_Stats matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Table_Stats_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Table_Stats retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Table_Stats_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.Table_Stats_List.Vector;
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
      System_Number: aliased integer;
      Nvalues: aliased integer;
      Rmean_1: aliased Real;
      Rmin_1: aliased Real;
      Rmax_1: aliased Real;
      Rmed_1: aliased Real;
      Sddev_1: aliased Real;
      Dec1_1: aliased Real;
      Dec10_1: aliased Real;
      Rmean_2: aliased Real;
      Rmin_2: aliased Real;
      Rmax_2: aliased Real;
      Rmed_2: aliased Real;
      Sddev_2: aliased Real;
      Dec1_2: aliased Real;
      Dec10_2: aliased Real;
      Rmean_3: aliased Real;
      Rmin_3: aliased Real;
      Rmax_3: aliased Real;
      Rmed_3: aliased Real;
      Sddev_3: aliased Real;
      Dec1_3: aliased Real;
      Dec10_3: aliased Real;
      Rmean_4: aliased Real;
      Rmin_4: aliased Real;
      Rmax_4: aliased Real;
      Rmed_4: aliased Real;
      Sddev_4: aliased Real;
      Dec1_4: aliased Real;
      Dec10_4: aliased Real;
      Rmean_5: aliased Real;
      Rmin_5: aliased Real;
      Rmax_5: aliased Real;
      Rmed_5: aliased Real;
      Sddev_5: aliased Real;
      Dec1_5: aliased Real;
      Dec10_5: aliased Real;
      Rmean_6: aliased Real;
      Rmin_6: aliased Real;
      Rmax_6: aliased Real;
      Rmed_6: aliased Real;
      Sddev_6: aliased Real;
      Dec1_6: aliased Real;
      Dec10_6: aliased Real;
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'access;
      Model_Table_Name_access : String_Access := Model_Table_Name'access;
      Wave_access : String_Access := Wave'access;
      Rmean_1_access : Real_Access := Rmean_1'access;
      Rmin_1_access : Real_Access := Rmin_1'access;
      Rmax_1_access : Real_Access := Rmax_1'access;
      Rmed_1_access : Real_Access := Rmed_1'access;
      Sddev_1_access : Real_Access := Sddev_1'access;
      Dec1_1_access : Real_Access := Dec1_1'access;
      Dec10_1_access : Real_Access := Dec10_1'access;
      Rmean_2_access : Real_Access := Rmean_2'access;
      Rmin_2_access : Real_Access := Rmin_2'access;
      Rmax_2_access : Real_Access := Rmax_2'access;
      Rmed_2_access : Real_Access := Rmed_2'access;
      Sddev_2_access : Real_Access := Sddev_2'access;
      Dec1_2_access : Real_Access := Dec1_2'access;
      Dec10_2_access : Real_Access := Dec10_2'access;
      Rmean_3_access : Real_Access := Rmean_3'access;
      Rmin_3_access : Real_Access := Rmin_3'access;
      Rmax_3_access : Real_Access := Rmax_3'access;
      Rmed_3_access : Real_Access := Rmed_3'access;
      Sddev_3_access : Real_Access := Sddev_3'access;
      Dec1_3_access : Real_Access := Dec1_3'access;
      Dec10_3_access : Real_Access := Dec10_3'access;
      Rmean_4_access : Real_Access := Rmean_4'access;
      Rmin_4_access : Real_Access := Rmin_4'access;
      Rmax_4_access : Real_Access := Rmax_4'access;
      Rmed_4_access : Real_Access := Rmed_4'access;
      Sddev_4_access : Real_Access := Sddev_4'access;
      Dec1_4_access : Real_Access := Dec1_4'access;
      Dec10_4_access : Real_Access := Dec10_4'access;
      Rmean_5_access : Real_Access := Rmean_5'access;
      Rmin_5_access : Real_Access := Rmin_5'access;
      Rmax_5_access : Real_Access := Rmax_5'access;
      Rmed_5_access : Real_Access := Rmed_5'access;
      Sddev_5_access : Real_Access := Sddev_5'access;
      Dec1_5_access : Real_Access := Dec1_5'access;
      Dec10_5_access : Real_Access := Dec10_5'access;
      Rmean_6_access : Real_Access := Rmean_6'access;
      Rmin_6_access : Real_Access := Rmin_6'access;
      Rmax_6_access : Real_Access := Rmax_6'access;
      Rmed_6_access : Real_Access := Rmed_6'access;
      Sddev_6_access : Real_Access := Sddev_6'access;
      Dec1_6_access : Real_Access := Dec1_6'access;
      Dec10_6_access : Real_Access := Dec10_6'access;
      --
      -- length holders for each retrieved variable
      --
      Run_Id_len : aliased SQLLEN := Run_Id'Size/8;
      Username_len : aliased SQLLEN := Username'Size/8;
      Model_Table_Name_len : aliased SQLLEN := Model_Table_Name'Size/8;
      Row_Num_len : aliased SQLLEN := Row_Num'Size/8;
      Col_Num_len : aliased SQLLEN := Col_Num'Size/8;
      Wave_len : aliased SQLLEN := Wave'Size/8;
      System_Number_len : aliased SQLLEN := System_Number'Size/8;
      Nvalues_len : aliased SQLLEN := Nvalues'Size/8;
      Rmean_1_len : aliased SQLLEN := Rmean_1'Size/8;
      Rmin_1_len : aliased SQLLEN := Rmin_1'Size/8;
      Rmax_1_len : aliased SQLLEN := Rmax_1'Size/8;
      Rmed_1_len : aliased SQLLEN := Rmed_1'Size/8;
      Sddev_1_len : aliased SQLLEN := Sddev_1'Size/8;
      Dec1_1_len : aliased SQLLEN := Dec1_1'Size/8;
      Dec10_1_len : aliased SQLLEN := Dec10_1'Size/8;
      Rmean_2_len : aliased SQLLEN := Rmean_2'Size/8;
      Rmin_2_len : aliased SQLLEN := Rmin_2'Size/8;
      Rmax_2_len : aliased SQLLEN := Rmax_2'Size/8;
      Rmed_2_len : aliased SQLLEN := Rmed_2'Size/8;
      Sddev_2_len : aliased SQLLEN := Sddev_2'Size/8;
      Dec1_2_len : aliased SQLLEN := Dec1_2'Size/8;
      Dec10_2_len : aliased SQLLEN := Dec10_2'Size/8;
      Rmean_3_len : aliased SQLLEN := Rmean_3'Size/8;
      Rmin_3_len : aliased SQLLEN := Rmin_3'Size/8;
      Rmax_3_len : aliased SQLLEN := Rmax_3'Size/8;
      Rmed_3_len : aliased SQLLEN := Rmed_3'Size/8;
      Sddev_3_len : aliased SQLLEN := Sddev_3'Size/8;
      Dec1_3_len : aliased SQLLEN := Dec1_3'Size/8;
      Dec10_3_len : aliased SQLLEN := Dec10_3'Size/8;
      Rmean_4_len : aliased SQLLEN := Rmean_4'Size/8;
      Rmin_4_len : aliased SQLLEN := Rmin_4'Size/8;
      Rmax_4_len : aliased SQLLEN := Rmax_4'Size/8;
      Rmed_4_len : aliased SQLLEN := Rmed_4'Size/8;
      Sddev_4_len : aliased SQLLEN := Sddev_4'Size/8;
      Dec1_4_len : aliased SQLLEN := Dec1_4'Size/8;
      Dec10_4_len : aliased SQLLEN := Dec10_4'Size/8;
      Rmean_5_len : aliased SQLLEN := Rmean_5'Size/8;
      Rmin_5_len : aliased SQLLEN := Rmin_5'Size/8;
      Rmax_5_len : aliased SQLLEN := Rmax_5'Size/8;
      Rmed_5_len : aliased SQLLEN := Rmed_5'Size/8;
      Sddev_5_len : aliased SQLLEN := Sddev_5'Size/8;
      Dec1_5_len : aliased SQLLEN := Dec1_5'Size/8;
      Dec10_5_len : aliased SQLLEN := Dec10_5'Size/8;
      Rmean_6_len : aliased SQLLEN := Rmean_6'Size/8;
      Rmin_6_len : aliased SQLLEN := Rmin_6'Size/8;
      Rmax_6_len : aliased SQLLEN := Rmax_6'Size/8;
      Rmed_6_len : aliased SQLLEN := Rmed_6'Size/8;
      Sddev_6_len : aliased SQLLEN := Sddev_6'Size/8;
      Dec1_6_len : aliased SQLLEN := Dec1_6'Size/8;
      Dec10_6_len : aliased SQLLEN := Dec10_6'Size/8;
      Table_Stats : Wsc_Db_Data.Table_Stats;
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
            TargetValue      => System_Number'access,
            IndPtr           => System_Number_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 8,
            TargetValue      => Nvalues'access,
            IndPtr           => Nvalues_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 9,
            TargetValuePtr   => To_SQLPOINTER( Rmean_1_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmean_1_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 10,
            TargetValuePtr   => To_SQLPOINTER( Rmin_1_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmin_1_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 11,
            TargetValuePtr   => To_SQLPOINTER( Rmax_1_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmax_1_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 12,
            TargetValuePtr   => To_SQLPOINTER( Rmed_1_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmed_1_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 13,
            TargetValuePtr   => To_SQLPOINTER( Sddev_1_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Sddev_1_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 14,
            TargetValuePtr   => To_SQLPOINTER( Dec1_1_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Dec1_1_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 15,
            TargetValuePtr   => To_SQLPOINTER( Dec10_1_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Dec10_1_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 16,
            TargetValuePtr   => To_SQLPOINTER( Rmean_2_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmean_2_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 17,
            TargetValuePtr   => To_SQLPOINTER( Rmin_2_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmin_2_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 18,
            TargetValuePtr   => To_SQLPOINTER( Rmax_2_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmax_2_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 19,
            TargetValuePtr   => To_SQLPOINTER( Rmed_2_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmed_2_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 20,
            TargetValuePtr   => To_SQLPOINTER( Sddev_2_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Sddev_2_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 21,
            TargetValuePtr   => To_SQLPOINTER( Dec1_2_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Dec1_2_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 22,
            TargetValuePtr   => To_SQLPOINTER( Dec10_2_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Dec10_2_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 23,
            TargetValuePtr   => To_SQLPOINTER( Rmean_3_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmean_3_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 24,
            TargetValuePtr   => To_SQLPOINTER( Rmin_3_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmin_3_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 25,
            TargetValuePtr   => To_SQLPOINTER( Rmax_3_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmax_3_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 26,
            TargetValuePtr   => To_SQLPOINTER( Rmed_3_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmed_3_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 27,
            TargetValuePtr   => To_SQLPOINTER( Sddev_3_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Sddev_3_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 28,
            TargetValuePtr   => To_SQLPOINTER( Dec1_3_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Dec1_3_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 29,
            TargetValuePtr   => To_SQLPOINTER( Dec10_3_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Dec10_3_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 30,
            TargetValuePtr   => To_SQLPOINTER( Rmean_4_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmean_4_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 31,
            TargetValuePtr   => To_SQLPOINTER( Rmin_4_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmin_4_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 32,
            TargetValuePtr   => To_SQLPOINTER( Rmax_4_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmax_4_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 33,
            TargetValuePtr   => To_SQLPOINTER( Rmed_4_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmed_4_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 34,
            TargetValuePtr   => To_SQLPOINTER( Sddev_4_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Sddev_4_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 35,
            TargetValuePtr   => To_SQLPOINTER( Dec1_4_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Dec1_4_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 36,
            TargetValuePtr   => To_SQLPOINTER( Dec10_4_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Dec10_4_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 37,
            TargetValuePtr   => To_SQLPOINTER( Rmean_5_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmean_5_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 38,
            TargetValuePtr   => To_SQLPOINTER( Rmin_5_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmin_5_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 39,
            TargetValuePtr   => To_SQLPOINTER( Rmax_5_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmax_5_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 40,
            TargetValuePtr   => To_SQLPOINTER( Rmed_5_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmed_5_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 41,
            TargetValuePtr   => To_SQLPOINTER( Sddev_5_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Sddev_5_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 42,
            TargetValuePtr   => To_SQLPOINTER( Dec1_5_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Dec1_5_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 43,
            TargetValuePtr   => To_SQLPOINTER( Dec10_5_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Dec10_5_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 44,
            TargetValuePtr   => To_SQLPOINTER( Rmean_6_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmean_6_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 45,
            TargetValuePtr   => To_SQLPOINTER( Rmin_6_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmin_6_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 46,
            TargetValuePtr   => To_SQLPOINTER( Rmax_6_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmax_6_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 47,
            TargetValuePtr   => To_SQLPOINTER( Rmed_6_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Rmed_6_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 48,
            TargetValuePtr   => To_SQLPOINTER( Sddev_6_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Sddev_6_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 49,
            TargetValuePtr   => To_SQLPOINTER( Dec1_6_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Dec1_6_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 50,
            TargetValuePtr   => To_SQLPOINTER( Dec10_6_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Dec10_6_len'access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Table_Stats.Run_Id := Run_Id;
            Table_Stats.Username := Slice_To_Unbounded( Username, 1, Natural( Username_len ) );
            Table_Stats.Model_Table_Name := Slice_To_Unbounded( Model_Table_Name, 1, Natural( Model_Table_Name_len ) );
            Table_Stats.Row_Num := Row_Num;
            Table_Stats.Col_Num := Col_Num;
            Table_Stats.Wave := Slice_To_Unbounded( Wave, 1, Natural( Wave_len ) );
            Table_Stats.System_Number := System_Number;
            Table_Stats.Nvalues := Nvalues;
            Table_Stats.Rmean_1:= Real( Rmean_1_access.all );
            Table_Stats.Rmin_1:= Real( Rmin_1_access.all );
            Table_Stats.Rmax_1:= Real( Rmax_1_access.all );
            Table_Stats.Rmed_1:= Real( Rmed_1_access.all );
            Table_Stats.Sddev_1:= Real( Sddev_1_access.all );
            Table_Stats.Dec1_1:= Real( Dec1_1_access.all );
            Table_Stats.Dec10_1:= Real( Dec10_1_access.all );
            Table_Stats.Rmean_2:= Real( Rmean_2_access.all );
            Table_Stats.Rmin_2:= Real( Rmin_2_access.all );
            Table_Stats.Rmax_2:= Real( Rmax_2_access.all );
            Table_Stats.Rmed_2:= Real( Rmed_2_access.all );
            Table_Stats.Sddev_2:= Real( Sddev_2_access.all );
            Table_Stats.Dec1_2:= Real( Dec1_2_access.all );
            Table_Stats.Dec10_2:= Real( Dec10_2_access.all );
            Table_Stats.Rmean_3:= Real( Rmean_3_access.all );
            Table_Stats.Rmin_3:= Real( Rmin_3_access.all );
            Table_Stats.Rmax_3:= Real( Rmax_3_access.all );
            Table_Stats.Rmed_3:= Real( Rmed_3_access.all );
            Table_Stats.Sddev_3:= Real( Sddev_3_access.all );
            Table_Stats.Dec1_3:= Real( Dec1_3_access.all );
            Table_Stats.Dec10_3:= Real( Dec10_3_access.all );
            Table_Stats.Rmean_4:= Real( Rmean_4_access.all );
            Table_Stats.Rmin_4:= Real( Rmin_4_access.all );
            Table_Stats.Rmax_4:= Real( Rmax_4_access.all );
            Table_Stats.Rmed_4:= Real( Rmed_4_access.all );
            Table_Stats.Sddev_4:= Real( Sddev_4_access.all );
            Table_Stats.Dec1_4:= Real( Dec1_4_access.all );
            Table_Stats.Dec10_4:= Real( Dec10_4_access.all );
            Table_Stats.Rmean_5:= Real( Rmean_5_access.all );
            Table_Stats.Rmin_5:= Real( Rmin_5_access.all );
            Table_Stats.Rmax_5:= Real( Rmax_5_access.all );
            Table_Stats.Rmed_5:= Real( Rmed_5_access.all );
            Table_Stats.Sddev_5:= Real( Sddev_5_access.all );
            Table_Stats.Dec1_5:= Real( Dec1_5_access.all );
            Table_Stats.Dec10_5:= Real( Dec10_5_access.all );
            Table_Stats.Rmean_6:= Real( Rmean_6_access.all );
            Table_Stats.Rmin_6:= Real( Rmin_6_access.all );
            Table_Stats.Rmax_6:= Real( Rmax_6_access.all );
            Table_Stats.Rmed_6:= Real( Rmed_6_access.all );
            Table_Stats.Sddev_6:= Real( Sddev_6_access.all );
            Table_Stats.Dec1_6:= Real( Dec1_6_access.all );
            Table_Stats.Dec10_6:= Real( Dec10_6_access.all );
            Wsc_Db_Data.Table_Stats_List.append( l, Table_Stats );        
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
   procedure Update( Table_Stats : Wsc_Db_Data.Table_Stats ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Nvalues( values_c, Table_Stats.Nvalues );
      Add_Rmean_1( values_c, Table_Stats.Rmean_1 );
      Add_Rmin_1( values_c, Table_Stats.Rmin_1 );
      Add_Rmax_1( values_c, Table_Stats.Rmax_1 );
      Add_Rmed_1( values_c, Table_Stats.Rmed_1 );
      Add_Sddev_1( values_c, Table_Stats.Sddev_1 );
      Add_Dec1_1( values_c, Table_Stats.Dec1_1 );
      Add_Dec10_1( values_c, Table_Stats.Dec10_1 );
      Add_Rmean_2( values_c, Table_Stats.Rmean_2 );
      Add_Rmin_2( values_c, Table_Stats.Rmin_2 );
      Add_Rmax_2( values_c, Table_Stats.Rmax_2 );
      Add_Rmed_2( values_c, Table_Stats.Rmed_2 );
      Add_Sddev_2( values_c, Table_Stats.Sddev_2 );
      Add_Dec1_2( values_c, Table_Stats.Dec1_2 );
      Add_Dec10_2( values_c, Table_Stats.Dec10_2 );
      Add_Rmean_3( values_c, Table_Stats.Rmean_3 );
      Add_Rmin_3( values_c, Table_Stats.Rmin_3 );
      Add_Rmax_3( values_c, Table_Stats.Rmax_3 );
      Add_Rmed_3( values_c, Table_Stats.Rmed_3 );
      Add_Sddev_3( values_c, Table_Stats.Sddev_3 );
      Add_Dec1_3( values_c, Table_Stats.Dec1_3 );
      Add_Dec10_3( values_c, Table_Stats.Dec10_3 );
      Add_Rmean_4( values_c, Table_Stats.Rmean_4 );
      Add_Rmin_4( values_c, Table_Stats.Rmin_4 );
      Add_Rmax_4( values_c, Table_Stats.Rmax_4 );
      Add_Rmed_4( values_c, Table_Stats.Rmed_4 );
      Add_Sddev_4( values_c, Table_Stats.Sddev_4 );
      Add_Dec1_4( values_c, Table_Stats.Dec1_4 );
      Add_Dec10_4( values_c, Table_Stats.Dec10_4 );
      Add_Rmean_5( values_c, Table_Stats.Rmean_5 );
      Add_Rmin_5( values_c, Table_Stats.Rmin_5 );
      Add_Rmax_5( values_c, Table_Stats.Rmax_5 );
      Add_Rmed_5( values_c, Table_Stats.Rmed_5 );
      Add_Sddev_5( values_c, Table_Stats.Sddev_5 );
      Add_Dec1_5( values_c, Table_Stats.Dec1_5 );
      Add_Dec10_5( values_c, Table_Stats.Dec10_5 );
      Add_Rmean_6( values_c, Table_Stats.Rmean_6 );
      Add_Rmin_6( values_c, Table_Stats.Rmin_6 );
      Add_Rmax_6( values_c, Table_Stats.Rmax_6 );
      Add_Rmed_6( values_c, Table_Stats.Rmed_6 );
      Add_Sddev_6( values_c, Table_Stats.Sddev_6 );
      Add_Dec1_6( values_c, Table_Stats.Dec1_6 );
      Add_Dec10_6( values_c, Table_Stats.Dec10_6 );
      --
      -- primary key fields
      --
      Add_Run_Id( pk_c, Table_Stats.Run_Id );
      Add_Username( pk_c, Table_Stats.Username );
      Add_Model_Table_Name( pk_c, Table_Stats.Model_Table_Name );
      Add_Row_Num( pk_c, Table_Stats.Row_Num );
      Add_Col_Num( pk_c, Table_Stats.Col_Num );
      Add_Wave( pk_c, Table_Stats.Wave );
      Add_System_Number( pk_c, Table_Stats.System_Number );
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
   procedure Save( Table_Stats : Wsc_Db_Data.Table_Stats; overwrite : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      Table_Stats_Tmp : Wsc_Db_Data.Table_Stats;
   begin
      if( overwrite ) then
         Table_Stats_Tmp := retrieve_By_PK( Table_Stats.Run_Id, Table_Stats.Username, Table_Stats.Model_Table_Name, Table_Stats.Row_Num, Table_Stats.Col_Num, Table_Stats.Wave, Table_Stats.System_Number );
         if( not is_Null( Table_Stats_Tmp )) then
            Update( Table_Stats );
            return;
         end if;
      end if;
      Add_Run_Id( c, Table_Stats.Run_Id );
      Add_Username( c, Table_Stats.Username );
      Add_Model_Table_Name( c, Table_Stats.Model_Table_Name );
      Add_Row_Num( c, Table_Stats.Row_Num );
      Add_Col_Num( c, Table_Stats.Col_Num );
      Add_Wave( c, Table_Stats.Wave );
      Add_System_Number( c, Table_Stats.System_Number );
      Add_Nvalues( c, Table_Stats.Nvalues );
      Add_Rmean_1( c, Table_Stats.Rmean_1 );
      Add_Rmin_1( c, Table_Stats.Rmin_1 );
      Add_Rmax_1( c, Table_Stats.Rmax_1 );
      Add_Rmed_1( c, Table_Stats.Rmed_1 );
      Add_Sddev_1( c, Table_Stats.Sddev_1 );
      Add_Dec1_1( c, Table_Stats.Dec1_1 );
      Add_Dec10_1( c, Table_Stats.Dec10_1 );
      Add_Rmean_2( c, Table_Stats.Rmean_2 );
      Add_Rmin_2( c, Table_Stats.Rmin_2 );
      Add_Rmax_2( c, Table_Stats.Rmax_2 );
      Add_Rmed_2( c, Table_Stats.Rmed_2 );
      Add_Sddev_2( c, Table_Stats.Sddev_2 );
      Add_Dec1_2( c, Table_Stats.Dec1_2 );
      Add_Dec10_2( c, Table_Stats.Dec10_2 );
      Add_Rmean_3( c, Table_Stats.Rmean_3 );
      Add_Rmin_3( c, Table_Stats.Rmin_3 );
      Add_Rmax_3( c, Table_Stats.Rmax_3 );
      Add_Rmed_3( c, Table_Stats.Rmed_3 );
      Add_Sddev_3( c, Table_Stats.Sddev_3 );
      Add_Dec1_3( c, Table_Stats.Dec1_3 );
      Add_Dec10_3( c, Table_Stats.Dec10_3 );
      Add_Rmean_4( c, Table_Stats.Rmean_4 );
      Add_Rmin_4( c, Table_Stats.Rmin_4 );
      Add_Rmax_4( c, Table_Stats.Rmax_4 );
      Add_Rmed_4( c, Table_Stats.Rmed_4 );
      Add_Sddev_4( c, Table_Stats.Sddev_4 );
      Add_Dec1_4( c, Table_Stats.Dec1_4 );
      Add_Dec10_4( c, Table_Stats.Dec10_4 );
      Add_Rmean_5( c, Table_Stats.Rmean_5 );
      Add_Rmin_5( c, Table_Stats.Rmin_5 );
      Add_Rmax_5( c, Table_Stats.Rmax_5 );
      Add_Rmed_5( c, Table_Stats.Rmed_5 );
      Add_Sddev_5( c, Table_Stats.Sddev_5 );
      Add_Dec1_5( c, Table_Stats.Dec1_5 );
      Add_Dec10_5( c, Table_Stats.Dec10_5 );
      Add_Rmean_6( c, Table_Stats.Rmean_6 );
      Add_Rmin_6( c, Table_Stats.Rmin_6 );
      Add_Rmax_6( c, Table_Stats.Rmax_6 );
      Add_Rmed_6( c, Table_Stats.Rmed_6 );
      Add_Sddev_6( c, Table_Stats.Sddev_6 );
      Add_Dec1_6( c, Table_Stats.Dec1_6 );
      Add_Dec10_6( c, Table_Stats.Dec10_6 );
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
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Table_Stats
   --

   procedure Delete( Table_Stats : in out Wsc_Db_Data.Table_Stats ) is
         c : d.Criteria;
   begin  
      Add_Run_Id( c, Table_Stats.Run_Id );
      Add_Username( c, Table_Stats.Username );
      Add_Model_Table_Name( c, Table_Stats.Model_Table_Name );
      Add_Row_Num( c, Table_Stats.Row_Num );
      Add_Col_Num( c, Table_Stats.Col_Num );
      Add_Wave( c, Table_Stats.Wave );
      Add_System_Number( c, Table_Stats.System_Number );
      delete( c );
      Table_Stats := Wsc_Db_Data.Null_Table_Stats;
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


   procedure Add_System_Number( c : in out d.Criteria; System_Number : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "system_number", op, join, System_Number );
   begin
      d.add_to_criteria( c, elem );
   end Add_System_Number;


   procedure Add_Nvalues( c : in out d.Criteria; Nvalues : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "nvalues", op, join, Nvalues );
   begin
      d.add_to_criteria( c, elem );
   end Add_Nvalues;


   procedure Add_Rmean_1( c : in out d.Criteria; Rmean_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmean_1", op, join, Rmean_1 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmean_1;


   procedure Add_Rmin_1( c : in out d.Criteria; Rmin_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmin_1", op, join, Rmin_1 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmin_1;


   procedure Add_Rmax_1( c : in out d.Criteria; Rmax_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmax_1", op, join, Rmax_1 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmax_1;


   procedure Add_Rmed_1( c : in out d.Criteria; Rmed_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmed_1", op, join, Rmed_1 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmed_1;


   procedure Add_Sddev_1( c : in out d.Criteria; Sddev_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "sddev_1", op, join, Sddev_1 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sddev_1;


   procedure Add_Dec1_1( c : in out d.Criteria; Dec1_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dec1_1", op, join, Dec1_1 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec1_1;


   procedure Add_Dec10_1( c : in out d.Criteria; Dec10_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dec10_1", op, join, Dec10_1 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec10_1;


   procedure Add_Rmean_2( c : in out d.Criteria; Rmean_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmean_2", op, join, Rmean_2 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmean_2;


   procedure Add_Rmin_2( c : in out d.Criteria; Rmin_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmin_2", op, join, Rmin_2 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmin_2;


   procedure Add_Rmax_2( c : in out d.Criteria; Rmax_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmax_2", op, join, Rmax_2 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmax_2;


   procedure Add_Rmed_2( c : in out d.Criteria; Rmed_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmed_2", op, join, Rmed_2 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmed_2;


   procedure Add_Sddev_2( c : in out d.Criteria; Sddev_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "sddev_2", op, join, Sddev_2 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sddev_2;


   procedure Add_Dec1_2( c : in out d.Criteria; Dec1_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dec1_2", op, join, Dec1_2 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec1_2;


   procedure Add_Dec10_2( c : in out d.Criteria; Dec10_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dec10_2", op, join, Dec10_2 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec10_2;


   procedure Add_Rmean_3( c : in out d.Criteria; Rmean_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmean_3", op, join, Rmean_3 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmean_3;


   procedure Add_Rmin_3( c : in out d.Criteria; Rmin_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmin_3", op, join, Rmin_3 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmin_3;


   procedure Add_Rmax_3( c : in out d.Criteria; Rmax_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmax_3", op, join, Rmax_3 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmax_3;


   procedure Add_Rmed_3( c : in out d.Criteria; Rmed_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmed_3", op, join, Rmed_3 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmed_3;


   procedure Add_Sddev_3( c : in out d.Criteria; Sddev_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "sddev_3", op, join, Sddev_3 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sddev_3;


   procedure Add_Dec1_3( c : in out d.Criteria; Dec1_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dec1_3", op, join, Dec1_3 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec1_3;


   procedure Add_Dec10_3( c : in out d.Criteria; Dec10_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dec10_3", op, join, Dec10_3 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec10_3;


   procedure Add_Rmean_4( c : in out d.Criteria; Rmean_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmean_4", op, join, Rmean_4 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmean_4;


   procedure Add_Rmin_4( c : in out d.Criteria; Rmin_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmin_4", op, join, Rmin_4 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmin_4;


   procedure Add_Rmax_4( c : in out d.Criteria; Rmax_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmax_4", op, join, Rmax_4 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmax_4;


   procedure Add_Rmed_4( c : in out d.Criteria; Rmed_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmed_4", op, join, Rmed_4 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmed_4;


   procedure Add_Sddev_4( c : in out d.Criteria; Sddev_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "sddev_4", op, join, Sddev_4 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sddev_4;


   procedure Add_Dec1_4( c : in out d.Criteria; Dec1_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dec1_4", op, join, Dec1_4 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec1_4;


   procedure Add_Dec10_4( c : in out d.Criteria; Dec10_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dec10_4", op, join, Dec10_4 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec10_4;


   procedure Add_Rmean_5( c : in out d.Criteria; Rmean_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmean_5", op, join, Rmean_5 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmean_5;


   procedure Add_Rmin_5( c : in out d.Criteria; Rmin_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmin_5", op, join, Rmin_5 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmin_5;


   procedure Add_Rmax_5( c : in out d.Criteria; Rmax_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmax_5", op, join, Rmax_5 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmax_5;


   procedure Add_Rmed_5( c : in out d.Criteria; Rmed_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmed_5", op, join, Rmed_5 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmed_5;


   procedure Add_Sddev_5( c : in out d.Criteria; Sddev_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "sddev_5", op, join, Sddev_5 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sddev_5;


   procedure Add_Dec1_5( c : in out d.Criteria; Dec1_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dec1_5", op, join, Dec1_5 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec1_5;


   procedure Add_Dec10_5( c : in out d.Criteria; Dec10_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dec10_5", op, join, Dec10_5 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec10_5;


   procedure Add_Rmean_6( c : in out d.Criteria; Rmean_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmean_6", op, join, Rmean_6 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmean_6;


   procedure Add_Rmin_6( c : in out d.Criteria; Rmin_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmin_6", op, join, Rmin_6 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmin_6;


   procedure Add_Rmax_6( c : in out d.Criteria; Rmax_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmax_6", op, join, Rmax_6 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmax_6;


   procedure Add_Rmed_6( c : in out d.Criteria; Rmed_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "rmed_6", op, join, Rmed_6 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmed_6;


   procedure Add_Sddev_6( c : in out d.Criteria; Sddev_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "sddev_6", op, join, Sddev_6 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sddev_6;


   procedure Add_Dec1_6( c : in out d.Criteria; Dec1_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dec1_6", op, join, Dec1_6 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec1_6;


   procedure Add_Dec10_6( c : in out d.Criteria; Dec10_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "dec10_6", op, join, Dec10_6 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec10_6;


   
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


   procedure Add_System_Number_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "system_number", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_System_Number_To_Orderings;


   procedure Add_Nvalues_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "nvalues", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Nvalues_To_Orderings;


   procedure Add_Rmean_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmean_1", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmean_1_To_Orderings;


   procedure Add_Rmin_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmin_1", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmin_1_To_Orderings;


   procedure Add_Rmax_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmax_1", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmax_1_To_Orderings;


   procedure Add_Rmed_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmed_1", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmed_1_To_Orderings;


   procedure Add_Sddev_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "sddev_1", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sddev_1_To_Orderings;


   procedure Add_Dec1_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dec1_1", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec1_1_To_Orderings;


   procedure Add_Dec10_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dec10_1", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec10_1_To_Orderings;


   procedure Add_Rmean_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmean_2", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmean_2_To_Orderings;


   procedure Add_Rmin_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmin_2", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmin_2_To_Orderings;


   procedure Add_Rmax_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmax_2", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmax_2_To_Orderings;


   procedure Add_Rmed_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmed_2", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmed_2_To_Orderings;


   procedure Add_Sddev_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "sddev_2", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sddev_2_To_Orderings;


   procedure Add_Dec1_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dec1_2", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec1_2_To_Orderings;


   procedure Add_Dec10_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dec10_2", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec10_2_To_Orderings;


   procedure Add_Rmean_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmean_3", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmean_3_To_Orderings;


   procedure Add_Rmin_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmin_3", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmin_3_To_Orderings;


   procedure Add_Rmax_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmax_3", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmax_3_To_Orderings;


   procedure Add_Rmed_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmed_3", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmed_3_To_Orderings;


   procedure Add_Sddev_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "sddev_3", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sddev_3_To_Orderings;


   procedure Add_Dec1_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dec1_3", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec1_3_To_Orderings;


   procedure Add_Dec10_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dec10_3", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec10_3_To_Orderings;


   procedure Add_Rmean_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmean_4", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmean_4_To_Orderings;


   procedure Add_Rmin_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmin_4", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmin_4_To_Orderings;


   procedure Add_Rmax_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmax_4", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmax_4_To_Orderings;


   procedure Add_Rmed_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmed_4", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmed_4_To_Orderings;


   procedure Add_Sddev_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "sddev_4", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sddev_4_To_Orderings;


   procedure Add_Dec1_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dec1_4", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec1_4_To_Orderings;


   procedure Add_Dec10_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dec10_4", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec10_4_To_Orderings;


   procedure Add_Rmean_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmean_5", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmean_5_To_Orderings;


   procedure Add_Rmin_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmin_5", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmin_5_To_Orderings;


   procedure Add_Rmax_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmax_5", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmax_5_To_Orderings;


   procedure Add_Rmed_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmed_5", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmed_5_To_Orderings;


   procedure Add_Sddev_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "sddev_5", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sddev_5_To_Orderings;


   procedure Add_Dec1_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dec1_5", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec1_5_To_Orderings;


   procedure Add_Dec10_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dec10_5", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec10_5_To_Orderings;


   procedure Add_Rmean_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmean_6", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmean_6_To_Orderings;


   procedure Add_Rmin_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmin_6", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmin_6_To_Orderings;


   procedure Add_Rmax_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmax_6", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmax_6_To_Orderings;


   procedure Add_Rmed_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "rmed_6", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Rmed_6_To_Orderings;


   procedure Add_Sddev_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "sddev_6", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sddev_6_To_Orderings;


   procedure Add_Dec1_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dec1_6", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec1_6_To_Orderings;


   procedure Add_Dec10_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "dec10_6", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Dec10_6_To_Orderings;


   
end Table_Stats_IO;
