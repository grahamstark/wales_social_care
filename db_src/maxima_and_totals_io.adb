--
-- Created by ada_generator.py on 2011-11-29 16:58:56.622943
-- 
with Wsc_Db_Data;


with Ada.Containers.Vectors;

with environment;

with db_commons; 
with db_commons.odbc; 

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
with text_io;
with Ada.Strings.Maps;
with WSC_Enums;

with Connection_Pool;
with GNATColl.Traces;

package body Maxima_And_Totals_IO is

   use Ada.Strings.Unbounded;
   use Ada.Exceptions;
   use Ada.Strings;

   package dodbc renames db_commons.odbc;
   package sql renames GNU.DB.SQLCLI;
   package sql_info renames GNU.DB.SQLCLI.Info;
   use sql;
   use base_types;
   --
   -- generic packages to handle each possible type of decimal, if any, go here
   --
   
   --
   -- Select all variables; substring to be competed with output from some criteria
   --
   SELECT_PART : constant String := "select " &  
        "case when sum( la_contributions ) is null then 0.0 else sum( la_contributions )* 52.0 end as lifetime_la_contributions, " &
        "case when sum( client_contributions ) is null then 0.0 else sum( client_contributions ) * 52.0 end as lifetime_client_contributions, " & 
        "case when sum( gross_care_costs ) is null then 0.0 else sum( gross_care_costs )* 52.0 end as lifetime_gross_payments, " &
        "case when sum( capital_contribution ) is null then 0.0 else sum( capital_contribution )* 52.0 end as lifetime_capital_contributions, " & 
        "case when max( la_contributions ) is null then 0.0 else max( la_contributions ) end as highest_la_contribution " &
        " from personal_results "; 

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MAXIMA_AND_TOTALS_IO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;
   --
   -- returns true if the primary key parts of Maxima_And_Totals match the defaults in Wsc_Db_Data.Null_Maxima_And_Totals
   --
   --
   -- Does this Maxima_And_Totals equal the default Wsc_Db_Data.Null_Maxima_And_Totals ?
   --
   function Is_Null( Maxima_And_Totals : Wsc_Db_Data.Maxima_And_Totals ) return Boolean is
   use Wsc_Db_Data;
   begin
      return Maxima_And_Totals = Wsc_Db_Data.Null_Maxima_And_Totals;
   end Is_Null;

   --
   -- Retrieves a list of Wsc_Db_Data.Maxima_And_Totals matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Maxima_And_Totals_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Maxima_And_Totals retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Maxima_And_Totals_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.Maxima_And_Totals_List.Vector;
      ps : SQLHDBC := SQL_NULL_HANDLE;
      has_data : Boolean := false;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := SELECT_PART & " " & sqlstr;
      Lifetime_La_Contributions: aliased Real;
      Lifetime_Client_Contributions: aliased Real;
      Lifetime_Gross_Payments: aliased Real;
      Lifetime_Capital_Contributions: aliased Real;
      Highest_La_Contribution: aliased Real;
      --
      -- access variables for any variables retrieved via access types
      --
      Lifetime_La_Contributions_access : Real_Access := Lifetime_La_Contributions'access;
      Lifetime_Client_Contributions_access : Real_Access := Lifetime_Client_Contributions'access;
      Lifetime_Gross_Payments_access : Real_Access := Lifetime_Gross_Payments'access;
      Lifetime_Capital_Contributions_access : Real_Access := Lifetime_Capital_Contributions'access;
      Highest_La_Contribution_access : Real_Access := Highest_La_Contribution'access;
      --
      -- length holders for each retrieved variable
      --
      Lifetime_La_Contributions_len : aliased SQLLEN := Lifetime_La_Contributions'Size/8;
      Lifetime_Client_Contributions_len : aliased SQLLEN := Lifetime_Client_Contributions'Size/8;
      Lifetime_Gross_Payments_len : aliased SQLLEN := Lifetime_Gross_Payments'Size/8;
      Lifetime_Capital_Contributions_len : aliased SQLLEN := Lifetime_Capital_Contributions'Size/8;
      Highest_La_Contribution_len : aliased SQLLEN := Highest_La_Contribution'Size/8;
      Maxima_And_Totals : Wsc_Db_Data.Maxima_And_Totals;
   begin
      Log( "retrieve made this as query " & query );
      begin -- exception block
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 1,
            TargetValuePtr   => To_SQLPOINTER( Lifetime_La_Contributions_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Lifetime_La_Contributions_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 2,
            TargetValuePtr   => To_SQLPOINTER( Lifetime_Client_Contributions_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Lifetime_Client_Contributions_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 3,
            TargetValuePtr   => To_SQLPOINTER( Lifetime_Gross_Payments_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Lifetime_Gross_Payments_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 4,
            TargetValuePtr   => To_SQLPOINTER( Lifetime_Capital_Contributions_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Lifetime_Capital_Contributions_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 5,
            TargetValuePtr   => To_SQLPOINTER( Highest_La_Contribution_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Highest_La_Contribution_len'access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Maxima_And_Totals.Lifetime_La_Contributions:= Real( Lifetime_La_Contributions_access.all );
            Maxima_And_Totals.Lifetime_Client_Contributions:= Real( Lifetime_Client_Contributions_access.all );
            Maxima_And_Totals.Lifetime_Gross_Payments:= Real( Lifetime_Gross_Payments_access.all );
            Maxima_And_Totals.Lifetime_Capital_Contributions:= Real( Lifetime_Capital_Contributions_access.all );
            Maxima_And_Totals.Highest_La_Contribution:= Real( Highest_La_Contribution_access.all );
            Wsc_Db_Data.Maxima_And_Totals_List.append( l, Maxima_And_Totals );        
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


   procedure Add_Pid( c : in out d.Criteria; Pid : Big_Integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "pid", op, join, Pid );
   begin
      d.add_to_criteria( c, elem );
   end Add_Pid;


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

   function Make_Wave_Year( wave : String ) return String is
   begin
      return "year_from_wave( '" & wave & "' )";
   end Make_Wave_Year;

   --
   -- this is a huge hack because we treat waves as strings rather than enums and we've made a year_from_wave function.
   -- we need to replace waves with years as integers everywhere.
   --
   procedure Add_Wave( c : in out d.Criteria; Wave : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( 
      varname     => "year_from_wave( wave )", 
      op          => op, 
      is_string   => False,
      join        => join, 
      value       => Make_Wave_Year( To_String( Wave )));
   begin
      d.add_to_criteria( c, elem );
   end Add_Wave;


   procedure Add_Wave( c : in out d.Criteria; Wave : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( 
      varname => "year_from_wave( wave )", 
      op        => op, 
      is_string => False, 
      join      => join, 
      value     => Make_Wave_Year( Wave ));
   begin
      d.add_to_criteria( c, elem );
   end Add_Wave;

   procedure Add_Hid( c : in out d.Criteria; Hid : Big_Integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "hid", op, join, Hid );
   begin
      d.add_to_criteria( c, elem );
   end Add_Hid;


   procedure Add_Buno( c : in out d.Criteria; Buno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "buno", op, join, Buno );
   begin
      d.add_to_criteria( c, elem );
   end Add_Buno;


   procedure Add_Adno( c : in out d.Criteria; Adno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "adno", op, join, Adno );
   begin
      d.add_to_criteria( c, elem );
   end Add_Adno;
   
end Maxima_And_Totals_IO;
