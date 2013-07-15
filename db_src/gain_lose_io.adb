--
-- Created by ada_generator.py on 2012-07-03 11:21:08.955373
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
with Templates_Parser;
with Model.WSC.Global_Settings;
with GNATColl.Traces;


package body Gain_Lose_IO is

   use Ada.Strings.Unbounded;
   use Ada.Exceptions;
   use Ada.Strings;

   package dodbc renames DB_Commons.ODBC;
   package sql renames GNU.DB.SQLCLI;
   package sql_info renames GNU.DB.SQLCLI.Info;
   use sql;
   use Base_Model_Types;
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "GAIN_LOSE_IO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;



   --
   -- generic packages to handle each possible type of decimal, if any, go here
   --
   
   --
   -- Retrieves a list of Wsc_Db_Data.Gain_Lose retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Gain_Lose_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.Gain_Lose_List.Vector;
      ps : SQLHDBC := SQL_NULL_HANDLE;
      has_data : Boolean := false;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := sqlstr;
      --
      -- aliased local versions of fields 
      --
      Username: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Run_Id: aliased integer;
      Iteration: aliased integer;
      Pid: aliased Big_Integer;
      Wave: aliased String := 
            "@@";
      Hid: aliased Big_Integer;
      Pre: aliased Real;
      Post: aliased Real;
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'access;
      Wave_access : String_Access := Wave'access;
      Pre_access : Real_Access := Pre'access;
      Post_access : Real_Access := Post'access;
      --
      -- length holders for each retrieved variable
      --
      Username_len : aliased SQLLEN := Username'Size/8;
      Run_Id_len : aliased SQLLEN := Run_Id'Size/8;
      Iteration_len : aliased SQLLEN := Iteration'Size/8;
      Pid_len : aliased SQLLEN := Pid'Size/8;
      Wave_len : aliased SQLLEN := Wave'Size/8;
      Hid_len : aliased SQLLEN := Hid'Size/8;
      Pre_len : aliased SQLLEN := Pre'Size/8;
      Post_len : aliased SQLLEN := Post'Size/8;
      Gain_Lose : Wsc_Db_Data.Gain_Lose;
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
            TargetValue      => Iteration'access,
            IndPtr           => Iteration_len'access );
         dodbc.L_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetValue      => Hid'access,
            IndPtr           => Hid_len'access );
         dodbc.L_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 5,
            TargetValue      => Pid'access,
            IndPtr           => Pid_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 6,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Wave_access.all'address ),
            BufferLength     => Wave_len,
            StrLen_Or_IndPtr => Wave_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 7,
            TargetValuePtr   => To_SQLPOINTER( Pre_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Pre_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 8,
            TargetValuePtr   => To_SQLPOINTER( Post_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Post_len'access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Gain_Lose.Username := Base_Types.Slice_To_Unbounded( Username, 1, Natural( Username_len ) );
            Gain_Lose.Run_Id := Run_Id;
            Gain_Lose.Iteration := Iteration;
            Gain_Lose.Pid := Pid;
            Gain_Lose.Wave := Base_Types.Slice_To_Unbounded( Wave, 1, Natural( Wave_len ) );
            Gain_Lose.Hid := Hid;
            Gain_Lose.Pre:= Amount( Pre_access.all );
            Gain_Lose.Post:= Amount( Post_access.all );
            Wsc_Db_Data.Gain_Lose_List.append( l, Gain_Lose );        
         end loop;
         Log( "retrieve: Query Run OK; wave is " & To_String( Gain_Lose.Wave));
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
   
   function Retrieve_Income( 
      run_id    : Positive;
      username  : Unbounded_String;
      iteration : Positive;
      wave      : Waves;
      which     : Incomes_Type;
      op        : d.operation_type:= d.LT;
      max_results : Positive := 10 ) return Wsc_Db_Data.Gain_Lose_List.Vector is
  use Templates_Parser;
      s : Unbounded_String;
      translations   : Translate_Set;
   begin
      Insert( translations, Assoc( "OP", d.To_String( op )));
      Insert( translations, Assoc( "INCOME_TYPE", Incomes_Type'Pos( which )));
      Insert( translations, Assoc( "ITERATION", Positive'Image( iteration )));
      Insert( translations, Assoc( "RUN-ID", Positive'Image( run_id )));
      Insert( translations, Assoc( "WAVE", To_String( wave )));
      Insert( translations, Assoc( "USERNAME", username ));
      Insert( translations, Assoc( "LIMIT", Positive'Image( max_results )));
      
      s := Templates_Parser.Parse( Model.WSC.Global_Settings.template_components_path & "income_query_template.sql", translations );
      return Retrieve( To_String( s ));
   end Retrieve_Income;
   
   function Retrieve_Result( 
      run_id    : Positive;
      username  : Unbounded_String;
      iteration : Positive;
      wave      : Waves;
      which     : Results_Field_Name;
      op        : d.operation_type:= d.LT;
      max_results : Positive := 10 ) return Wsc_Db_Data.Gain_Lose_List.Vector is
  use Templates_Parser;
      s : Unbounded_String;
      translations   : Translate_Set;
   begin
      Insert( translations, Assoc( "OP", d.To_String( op )));
      Insert( translations, Assoc( "FIELD-NAME", To_String( which )));
      Insert( translations, Assoc( "ITERATION", Positive'Image( iteration )));
      Insert( translations, Assoc( "RUN-ID", Positive'Image( run_id )));
      Insert( translations, Assoc( "WAVE", To_String( wave )));
      Insert( translations, Assoc( "USERNAME", username ));
      Insert( translations, Assoc( "LIMIT", Positive'Image( max_results )));
      
      s := Templates_Parser.Parse( Model.WSC.Global_Settings.template_components_path & "results_query_template.sql", translations );
      return Retrieve( To_String( s ));
   end Retrieve_Result;
   

end Gain_Lose_IO;
