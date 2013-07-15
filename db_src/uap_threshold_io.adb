--
-- Created by ada_generator.py on 2012-05-22 11:59:45.760057
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
with Base_Model_Types;

with GNATColl.Traces;



package body Uap_Threshold_IO is

   use Ada.Strings.Unbounded;
   use Ada.Exceptions;
   use Ada.Strings;
   use Base_Model_Types;
   
   package dodbc renames DB_Commons.ODBC;
   package sql renames GNU.DB.SQLCLI;
   package sql_info renames GNU.DB.SQLCLI.Info;
   use sql;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "UAP_THRESHOLD_IO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


    
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


   procedure Add_Sysno( c : in out d.Criteria; Sysno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "sysno", op, join, Sysno );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sysno;


   procedure Add_Iteration( c : in out d.Criteria; Iteration : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "iteration", op, join, Iteration );
   begin
      d.add_to_criteria( c, elem );
   end Add_Iteration;


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


   procedure Add_Uap_Level( c : in out d.Criteria; level : UAP_Level; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "uap_level", op, join, Integer( UAP_Level'Pos( level )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Uap_Level;


   procedure Add_Threshold( c : in out d.Criteria; Threshold : Amount; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "threshold", op, join, Threshold );
   begin
      d.add_to_criteria( c, elem );
   end Add_Threshold;


   
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


   procedure Add_Sysno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "sysno", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Sysno_To_Orderings;


   procedure Add_Iteration_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "iteration", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Iteration_To_Orderings;


   procedure Add_Wave_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "wave", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Wave_To_Orderings;


   procedure Add_Uap_Level_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "uap_level", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Uap_Level_To_Orderings;


   procedure Add_Threshold_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "threshold", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Threshold_To_Orderings;
 
   --
   -- generic packages to handle each possible type of decimal, if any, go here
   --
   
   --
   -- Select all variables; substring to be competed with output from some criteria
   --
   SELECT_PART : constant String := "select " &
         "run_id, username, sysno, iteration, wave, uap_level, threshold " &
         " from uap_threshold " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into uap_threshold (" &
         "run_id, username, sysno, iteration, wave, uap_level, threshold " &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from uap_threshold ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update uap_threshold set  ";
   --
   -- Retrieves a list of Wsc_Db_Data.Uap_Threshold retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return UAP_Array is
      type Amount_Access is access all Amount;
      type String_Access is access all String;

      l : UAP_Array;
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
      Sysno: aliased integer;
      Iteration: aliased integer;
      Wave: aliased String := 
            "@@";
      uaplev: aliased Integer;
      Threshold: aliased Amount;
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'access;
      Wave_access : String_Access := Wave'access;
      Threshold_access : Amount_Access := Threshold'access;
      --
      -- length holders for each retrieved variable
      --
      Run_Id_len : aliased SQLLEN := Run_Id'Size/8;
      Username_len : aliased SQLLEN := Username'Size/8;
      Sysno_len : aliased SQLLEN := Sysno'Size/8;
      Iteration_len : aliased SQLLEN := Iteration'Size/8;
      Wave_len : aliased SQLLEN := Wave'Size/8;
      uaplev_len : aliased SQLLEN := uaplev'Size/8;
      Threshold_len : aliased SQLLEN := Threshold'Size/8;
      level : UAP_Level;
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
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 3,
            TargetValue      => Sysno'access,
            IndPtr           => Sysno_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetValue      => Iteration'access,
            IndPtr           => Iteration_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 5,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Wave_access.all'address ),
            BufferLength     => Wave_len,
            StrLen_Or_IndPtr => Wave_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 6,
            TargetValue      => uaplev'access,
            IndPtr           => uaplev_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 7,
            TargetValuePtr   => To_SQLPOINTER( Threshold_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Threshold_len'access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            level := UAP_Level'Val( uaplev );
            l( level ) := Amount( Threshold_access.all );        
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

   
   function Retrieve( 
      run_id : integer; 
      username : Unbounded_String; 
      sysno : integer; 
      iteration : integer; 
      wave : Waves ) return UAP_Array is
      l : UAP_Array;
      c : d.Criteria;
   begin
      Add_Run_Id( c, run_id );
      Add_Username( c, username );
      Add_Sysno( c, sysno );
      Add_Iteration( c, iteration );
      Add_Wave( c, wave'img );
      l := Retrieve( d.To_String( c ));
      return l;     
   end Retrieve;
 

   procedure Save( 
      run_id    : integer; 
      username  : Unbounded_String; 
      sysno     : integer; 
      iteration : integer; 
      wave      : Waves;
      uaps      : UAP_Array; 
      overwrite : Boolean := True ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
   begin
      connection := Connection_Pool.Lease;
      for level in UAP_Level loop
         declare
            c : d.Criteria;
            query : Unbounded_String;
         begin
            Add_Run_Id( c, run_id );
            Add_Username( c, username );
            Add_Sysno( c, sysno );
            Add_Iteration( c, iteration );
            Add_Wave( c, wave'img );
            Add_Uap_Level( c, level );
            Add_Threshold( c, uaps( level ));
            query := INSERT_PART & To_Unbounded_String( " ( " ) & d.To_Crude_Array_Of_Values( c ) & " )";
            Log( "save; executing query" & To_String(query) );
            begin
               ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
               SQLExecute( ps );
               Log( "save; execute query OK" );
            exception 
               when Error : others =>
                  GNATColl.Traces.Trace( log_trace, error, "save; execute query failed with message " );
                  Raise_Exception( d.DB_Exception'Identity, 
                     "save: exception thrown " & Exception_Information(Error) );
            end;
         end;
      end loop;   
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


   
end Uap_Threshold_IO;
