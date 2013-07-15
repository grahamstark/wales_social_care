--
-- Created by ada_generator.py on 2012-07-24 19:03:55.753020
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


with DB_Logger;

package body Personal_Results_IO is

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
         "username, run_id, sysno, iteration, pid, wave, hid, buno, adno, passes_non_residential_capital_test," &
         "passes_non_residential_income_test, passes_residential_capital_test, passes_residential_income_test, passes_residential_means_test, passes_non_residential_means_test, la_contributions, client_contributions, gross_care_costs, total_payments_to_date, disposable_income," &
         "net_income, marginal_rate, capital_contribution, minimum_income_guarantee, hours_of_care_la, hours_of_care_private, uap, remaining_capital_stock " &
         " from personal_results " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into personal_results (" &
         "username, run_id, sysno, iteration, pid, wave, hid, buno, adno, passes_non_residential_capital_test," &
         "passes_non_residential_income_test, passes_residential_capital_test, passes_residential_income_test, passes_residential_means_test, passes_non_residential_means_test, la_contributions, client_contributions, gross_care_costs, total_payments_to_date, disposable_income," &
         "net_income, marginal_rate, capital_contribution, minimum_income_guarantee, hours_of_care_la, hours_of_care_private, uap, remaining_capital_stock " &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from personal_results ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update personal_results set  ";
   
   
   -- 
   -- Next highest avaiable value of Run_Id - useful for saving  
   --
   function Next_Free_Run_Id return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( run_id ) from personal_results";
      ai : aliased integer;
      has_data : boolean := true; 
      output_len : aliased sql.SQLINTEGER;     
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
   -- Next highest avaiable value of Sysno - useful for saving  
   --
   function Next_Free_Sysno return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( sysno ) from personal_results";
      ai : aliased integer;
      has_data : boolean := true; 
      output_len : aliased sql.SQLINTEGER;     
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
   end Next_Free_Sysno;


   -- 
   -- Next highest avaiable value of Iteration - useful for saving  
   --
   function Next_Free_Iteration return integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( iteration ) from personal_results";
      ai : aliased integer;
      has_data : boolean := true; 
      output_len : aliased sql.SQLINTEGER;     
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
   -- Next highest avaiable value of Pid - useful for saving  
   --
   function Next_Free_Pid return Big_Integer is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := "select max( pid ) from personal_results";
      ai : aliased Big_Integer;
      has_data : boolean := true; 
      output_len : aliased sql.SQLINTEGER;     
   begin
      ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
      dodbc.L_Out_Binding.SQLBindCol( 
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
   end Next_Free_Pid;



   --
   -- returns true if the primary key parts of Personal_Results match the defaults in Wsc_Db_Data.Null_Personal_Results
   --
   --
   -- Does this Personal_Results equal the default Wsc_Db_Data.Null_Personal_Results ?
   --
   function Is_Null( Personal_Results : Wsc_Db_Data.Personal_Results ) return Boolean is
   use Wsc_Db_Data;
   begin
      return Personal_Results = Wsc_Db_Data.Null_Personal_Results;
   end Is_Null;


   
   --
   -- returns the single Personal_Results matching the primary key fields, or the Wsc_Db_Data.Null_Personal_Results record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String; Run_Id : integer; Sysno : integer; Iteration : integer; Pid : Big_Integer; Wave : Unbounded_String ) return Wsc_Db_Data.Personal_Results is
      l : Wsc_Db_Data.Personal_Results_List.Vector;
      Personal_Results : Wsc_Db_Data.Personal_Results;
      c : d.Criteria;
   begin      
      Add_Username( c, Username );
      Add_Run_Id( c, Run_Id );
      Add_Sysno( c, Sysno );
      Add_Iteration( c, Iteration );
      Add_Pid( c, Pid );
      Add_Wave( c, Wave );
      l := Retrieve( c );
      if( not Wsc_Db_Data.Personal_Results_List.is_empty( l ) ) then
         Personal_Results := Wsc_Db_Data.Personal_Results_List.First_Element( l );
      else
         Personal_Results := Wsc_Db_Data.Null_Personal_Results;
      end if;
      return Personal_Results;
   end Retrieve_By_PK;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Personal_Results matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Personal_Results_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   
   --
   -- Retrieves a list of Wsc_Db_Data.Personal_Results retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Personal_Results_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.Personal_Results_List.Vector;
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
      Sysno: aliased integer;
      Iteration: aliased integer;
      Pid: aliased Big_Integer;
      Wave: aliased String := 
            "@@";
      Hid: aliased Big_Integer;
      Buno: aliased integer;
      Adno: aliased integer;
      Passes_Non_Residential_Capital_Test: aliased Integer;
      Passes_Non_Residential_Income_Test: aliased Integer;
      Passes_Residential_Capital_Test: aliased Integer;
      Passes_Residential_Income_Test: aliased Integer;
      Passes_Residential_Means_Test: aliased Integer;
      Passes_Non_Residential_Means_Test: aliased Integer;
      La_Contributions: aliased Real;
      Client_Contributions: aliased Real;
      Gross_Care_Costs: aliased Real;
      Total_Payments_To_Date: aliased Real;
      Disposable_Income: aliased Real;
      Net_Income: aliased Real;
      Marginal_Rate: aliased Real;
      Capital_Contribution: aliased Real;
      Minimum_Income_Guarantee: aliased Real;
      Hours_Of_Care_La: aliased Real;
      Hours_Of_Care_Private: aliased Real;
      Uap: aliased Integer;
      Remaining_Capital_Stock: aliased Real;
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'Access;
      Wave_access : String_Access := Wave'Access;
      La_Contributions_access : Real_Access := La_Contributions'Access;
      Client_Contributions_access : Real_Access := Client_Contributions'Access;
      Gross_Care_Costs_access : Real_Access := Gross_Care_Costs'Access;
      Total_Payments_To_Date_access : Real_Access := Total_Payments_To_Date'Access;
      Disposable_Income_access : Real_Access := Disposable_Income'Access;
      Net_Income_access : Real_Access := Net_Income'Access;
      Marginal_Rate_access : Real_Access := Marginal_Rate'Access;
      Capital_Contribution_access : Real_Access := Capital_Contribution'Access;
      Minimum_Income_Guarantee_access : Real_Access := Minimum_Income_Guarantee'Access;
      Hours_Of_Care_La_access : Real_Access := Hours_Of_Care_La'Access;
      Hours_Of_Care_Private_access : Real_Access := Hours_Of_Care_Private'Access;
      Remaining_Capital_Stock_access : Real_Access := Remaining_Capital_Stock'Access;
      --
      -- length holders for each retrieved variable
      --
      Username_len : aliased SQLINTEGER := Username'Size/8;
      Run_Id_len : aliased SQLINTEGER := Run_Id'Size/8;
      Sysno_len : aliased SQLINTEGER := Sysno'Size/8;
      Iteration_len : aliased SQLINTEGER := Iteration'Size/8;
      Pid_len : aliased SQLINTEGER := Pid'Size/8;
      Wave_len : aliased SQLINTEGER := Wave'Size/8;
      Hid_len : aliased SQLINTEGER := Hid'Size/8;
      Buno_len : aliased SQLINTEGER := Buno'Size/8;
      Adno_len : aliased SQLINTEGER := Adno'Size/8;
      Passes_Non_Residential_Capital_Test_len : aliased SQLINTEGER := Passes_Non_Residential_Capital_Test'Size/8;
      Passes_Non_Residential_Income_Test_len : aliased SQLINTEGER := Passes_Non_Residential_Income_Test'Size/8;
      Passes_Residential_Capital_Test_len : aliased SQLINTEGER := Passes_Residential_Capital_Test'Size/8;
      Passes_Residential_Income_Test_len : aliased SQLINTEGER := Passes_Residential_Income_Test'Size/8;
      Passes_Residential_Means_Test_len : aliased SQLINTEGER := Passes_Residential_Means_Test'Size/8;
      Passes_Non_Residential_Means_Test_len : aliased SQLINTEGER := Passes_Non_Residential_Means_Test'Size/8;
      La_Contributions_len : aliased SQLINTEGER := La_Contributions'Size/8;
      Client_Contributions_len : aliased SQLINTEGER := Client_Contributions'Size/8;
      Gross_Care_Costs_len : aliased SQLINTEGER := Gross_Care_Costs'Size/8;
      Total_Payments_To_Date_len : aliased SQLINTEGER := Total_Payments_To_Date'Size/8;
      Disposable_Income_len : aliased SQLINTEGER := Disposable_Income'Size/8;
      Net_Income_len : aliased SQLINTEGER := Net_Income'Size/8;
      Marginal_Rate_len : aliased SQLINTEGER := Marginal_Rate'Size/8;
      Capital_Contribution_len : aliased SQLINTEGER := Capital_Contribution'Size/8;
      Minimum_Income_Guarantee_len : aliased SQLINTEGER := Minimum_Income_Guarantee'Size/8;
      Hours_Of_Care_La_len : aliased SQLINTEGER := Hours_Of_Care_La'Size/8;
      Hours_Of_Care_Private_len : aliased SQLINTEGER := Hours_Of_Care_Private'Size/8;
      Uap_len : aliased SQLINTEGER := Uap'Size/8;
      Remaining_Capital_Stock_len : aliased SQLINTEGER := Remaining_Capital_Stock'Size/8;
      Personal_Results : Wsc_Db_Data.Personal_Results;
   begin
      DB_Logger.info( "retrieve made this as query " & query );
      begin -- exception block
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 1,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Username_access.all'Address ),
            BufferLength     => Username_len,
            StrLen_Or_IndPtr => Username_len'Access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 2,
            TargetValue      => Run_Id'access,
            IndPtr           => Run_Id_len'access );
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
         dodbc.L_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 5,
            TargetValue      => Pid'access,
            IndPtr           => Pid_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 6,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Wave_access.all'Address ),
            BufferLength     => Wave_len,
            StrLen_Or_IndPtr => Wave_len'Access );
         dodbc.L_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 7,
            TargetValue      => Hid'access,
            IndPtr           => Hid_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 8,
            TargetValue      => Buno'access,
            IndPtr           => Buno_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 9,
            TargetValue      => Adno'access,
            IndPtr           => Adno_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 10,
            TargetValue      => Passes_Non_Residential_Capital_Test'access,
            IndPtr           => Passes_Non_Residential_Capital_Test_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 11,
            TargetValue      => Passes_Non_Residential_Income_Test'access,
            IndPtr           => Passes_Non_Residential_Income_Test_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 12,
            TargetValue      => Passes_Residential_Capital_Test'access,
            IndPtr           => Passes_Residential_Capital_Test_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 13,
            TargetValue      => Passes_Residential_Income_Test'access,
            IndPtr           => Passes_Residential_Income_Test_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 14,
            TargetValue      => Passes_Residential_Means_Test'access,
            IndPtr           => Passes_Residential_Means_Test_len'access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 15,
            TargetValue      => Passes_Non_Residential_Means_Test'access,
            IndPtr           => Passes_Non_Residential_Means_Test_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 16,
            TargetValuePtr   => To_SQLPOINTER( La_Contributions_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => La_Contributions_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 17,
            TargetValuePtr   => To_SQLPOINTER( Client_Contributions_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Client_Contributions_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 18,
            TargetValuePtr   => To_SQLPOINTER( Gross_Care_Costs_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Gross_Care_Costs_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 19,
            TargetValuePtr   => To_SQLPOINTER( Total_Payments_To_Date_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Total_Payments_To_Date_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 20,
            TargetValuePtr   => To_SQLPOINTER( Disposable_Income_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Disposable_Income_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 21,
            TargetValuePtr   => To_SQLPOINTER( Net_Income_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Net_Income_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 22,
            TargetValuePtr   => To_SQLPOINTER( Marginal_Rate_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Marginal_Rate_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 23,
            TargetValuePtr   => To_SQLPOINTER( Capital_Contribution_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Capital_Contribution_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 24,
            TargetValuePtr   => To_SQLPOINTER( Minimum_Income_Guarantee_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Minimum_Income_Guarantee_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 25,
            TargetValuePtr   => To_SQLPOINTER( Hours_Of_Care_La_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Hours_Of_Care_La_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 26,
            TargetValuePtr   => To_SQLPOINTER( Hours_Of_Care_Private_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Hours_Of_Care_Private_len'Access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 27,
            TargetValue      => Uap'access,
            IndPtr           => Uap_len'access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 28,
            TargetValuePtr   => To_SQLPOINTER( Remaining_Capital_Stock_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Remaining_Capital_Stock_len'Access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Personal_Results.Username := Slice_To_Unbounded( Username, 1, Natural( Username_len ) );
            Personal_Results.Run_Id := Run_Id;
            Personal_Results.Sysno := Sysno;
            Personal_Results.Iteration := Iteration;
            Personal_Results.Pid := Pid;
            Personal_Results.Wave := Slice_To_Unbounded( Wave, 1, Natural( Wave_len ) );
            Personal_Results.Hid := Hid;
            Personal_Results.Buno := Buno;
            Personal_Results.Adno := Adno;
            Personal_Results.Passes_Non_Residential_Capital_Test := personal_results_passes_non_residential_capital_test_Enum'Val( Passes_Non_Residential_Capital_Test );
            Personal_Results.Passes_Non_Residential_Income_Test := personal_results_passes_non_residential_income_test_Enum'Val( Passes_Non_Residential_Income_Test );
            Personal_Results.Passes_Residential_Capital_Test := personal_results_passes_residential_capital_test_Enum'Val( Passes_Residential_Capital_Test );
            Personal_Results.Passes_Residential_Income_Test := personal_results_passes_residential_income_test_Enum'Val( Passes_Residential_Income_Test );
            Personal_Results.Passes_Residential_Means_Test := personal_results_passes_residential_means_test_Enum'Val( Passes_Residential_Means_Test );
            Personal_Results.Passes_Non_Residential_Means_Test := personal_results_passes_non_residential_means_test_Enum'Val( Passes_Non_Residential_Means_Test );
            Personal_Results.La_Contributions:= Real( La_Contributions_access.all );
            Personal_Results.Client_Contributions:= Real( Client_Contributions_access.all );
            Personal_Results.Gross_Care_Costs:= Real( Gross_Care_Costs_access.all );
            Personal_Results.Total_Payments_To_Date:= Real( Total_Payments_To_Date_access.all );
            Personal_Results.Disposable_Income:= Real( Disposable_Income_access.all );
            Personal_Results.Net_Income:= Real( Net_Income_access.all );
            Personal_Results.Marginal_Rate:= Real( Marginal_Rate_access.all );
            Personal_Results.Capital_Contribution:= Real( Capital_Contribution_access.all );
            Personal_Results.Minimum_Income_Guarantee:= Real( Minimum_Income_Guarantee_access.all );
            Personal_Results.Hours_Of_Care_La:= Real( Hours_Of_Care_La_access.all );
            Personal_Results.Hours_Of_Care_Private:= Real( Hours_Of_Care_Private_access.all );
            Personal_Results.Uap := personal_results_uap_Enum'Val( Uap );
            Personal_Results.Remaining_Capital_Stock:= Amount( Remaining_Capital_Stock_access.all );
            Wsc_Db_Data.Personal_Results_List.append( l, Personal_Results );        
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
   procedure Update( Personal_Results : Wsc_Db_Data.Personal_Results ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Hid( values_c, Personal_Results.Hid );
      Add_Buno( values_c, Personal_Results.Buno );
      Add_Adno( values_c, Personal_Results.Adno );
      Add_Passes_Non_Residential_Capital_Test( values_c, Personal_Results.Passes_Non_Residential_Capital_Test );
      Add_Passes_Non_Residential_Income_Test( values_c, Personal_Results.Passes_Non_Residential_Income_Test );
      Add_Passes_Residential_Capital_Test( values_c, Personal_Results.Passes_Residential_Capital_Test );
      Add_Passes_Residential_Income_Test( values_c, Personal_Results.Passes_Residential_Income_Test );
      Add_Passes_Residential_Means_Test( values_c, Personal_Results.Passes_Residential_Means_Test );
      Add_Passes_Non_Residential_Means_Test( values_c, Personal_Results.Passes_Non_Residential_Means_Test );
      Add_La_Contributions( values_c, Personal_Results.La_Contributions );
      Add_Client_Contributions( values_c, Personal_Results.Client_Contributions );
      Add_Gross_Care_Costs( values_c, Personal_Results.Gross_Care_Costs );
      Add_Total_Payments_To_Date( values_c, Personal_Results.Total_Payments_To_Date );
      Add_Disposable_Income( values_c, Personal_Results.Disposable_Income );
      Add_Net_Income( values_c, Personal_Results.Net_Income );
      Add_Marginal_Rate( values_c, Personal_Results.Marginal_Rate );
      Add_Capital_Contribution( values_c, Personal_Results.Capital_Contribution );
      Add_Minimum_Income_Guarantee( values_c, Personal_Results.Minimum_Income_Guarantee );
      Add_Hours_Of_Care_La( values_c, Personal_Results.Hours_Of_Care_La );
      Add_Hours_Of_Care_Private( values_c, Personal_Results.Hours_Of_Care_Private );
      Add_Uap( values_c, Personal_Results.Uap );
      Add_Remaining_Capital_Stock( values_c, Personal_Results.Remaining_Capital_Stock );
      --
      -- primary key fields
      --
      Add_Username( pk_c, Personal_Results.Username );
      Add_Run_Id( pk_c, Personal_Results.Run_Id );
      Add_Sysno( pk_c, Personal_Results.Sysno );
      Add_Iteration( pk_c, Personal_Results.Iteration );
      Add_Pid( pk_c, Personal_Results.Pid );
      Add_Wave( pk_c, Personal_Results.Wave );
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
   procedure Save( Personal_Results : Wsc_Db_Data.Personal_Results; overwrite : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      Personal_Results_Tmp : Wsc_Db_Data.Personal_Results;
   begin
      if( overwrite ) then
         Personal_Results_Tmp := retrieve_By_PK( Personal_Results.Username, Personal_Results.Run_Id, Personal_Results.Sysno, Personal_Results.Iteration, Personal_Results.Pid, Personal_Results.Wave );
         if( not is_Null( Personal_Results_Tmp )) then
            Update( Personal_Results );
            return;
         end if;
      end if;
      Add_Username( c, Personal_Results.Username );
      Add_Run_Id( c, Personal_Results.Run_Id );
      Add_Sysno( c, Personal_Results.Sysno );
      Add_Iteration( c, Personal_Results.Iteration );
      Add_Pid( c, Personal_Results.Pid );
      Add_Wave( c, Personal_Results.Wave );
      Add_Hid( c, Personal_Results.Hid );
      Add_Buno( c, Personal_Results.Buno );
      Add_Adno( c, Personal_Results.Adno );
      Add_Passes_Non_Residential_Capital_Test( c, Personal_Results.Passes_Non_Residential_Capital_Test );
      Add_Passes_Non_Residential_Income_Test( c, Personal_Results.Passes_Non_Residential_Income_Test );
      Add_Passes_Residential_Capital_Test( c, Personal_Results.Passes_Residential_Capital_Test );
      Add_Passes_Residential_Income_Test( c, Personal_Results.Passes_Residential_Income_Test );
      Add_Passes_Residential_Means_Test( c, Personal_Results.Passes_Residential_Means_Test );
      Add_Passes_Non_Residential_Means_Test( c, Personal_Results.Passes_Non_Residential_Means_Test );
      Add_La_Contributions( c, Personal_Results.La_Contributions );
      Add_Client_Contributions( c, Personal_Results.Client_Contributions );
      Add_Gross_Care_Costs( c, Personal_Results.Gross_Care_Costs );
      Add_Total_Payments_To_Date( c, Personal_Results.Total_Payments_To_Date );
      Add_Disposable_Income( c, Personal_Results.Disposable_Income );
      Add_Net_Income( c, Personal_Results.Net_Income );
      Add_Marginal_Rate( c, Personal_Results.Marginal_Rate );
      Add_Capital_Contribution( c, Personal_Results.Capital_Contribution );
      Add_Minimum_Income_Guarantee( c, Personal_Results.Minimum_Income_Guarantee );
      Add_Hours_Of_Care_La( c, Personal_Results.Hours_Of_Care_La );
      Add_Hours_Of_Care_Private( c, Personal_Results.Hours_Of_Care_Private );
      Add_Uap( c, Personal_Results.Uap );
      Add_Remaining_Capital_Stock( c, Personal_Results.Remaining_Capital_Stock );
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
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Personal_Results
   --

   procedure Delete( Personal_Results : in out Wsc_Db_Data.Personal_Results ) is
         c : d.Criteria;
   begin  
      Add_Username( c, Personal_Results.Username );
      Add_Run_Id( c, Personal_Results.Run_Id );
      Add_Sysno( c, Personal_Results.Sysno );
      Add_Iteration( c, Personal_Results.Iteration );
      Add_Pid( c, Personal_Results.Pid );
      Add_Wave( c, Personal_Results.Wave );
      delete( c );
      Personal_Results := Wsc_Db_Data.Null_Personal_Results;
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


   procedure Add_Pid( c : in out d.Criteria; Pid : Big_Integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "pid", op, join, Pid );
   begin
      d.add_to_criteria( c, elem );
   end Add_Pid;


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


   procedure Add_Passes_Non_Residential_Capital_Test( c : in out d.Criteria; Passes_Non_Residential_Capital_Test : personal_results_passes_non_residential_capital_test_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "passes_non_residential_capital_test", op, join, Integer( personal_results_passes_non_residential_capital_test_Enum'Pos( Passes_Non_Residential_Capital_Test )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Passes_Non_Residential_Capital_Test;


   procedure Add_Passes_Non_Residential_Income_Test( c : in out d.Criteria; Passes_Non_Residential_Income_Test : personal_results_passes_non_residential_income_test_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "passes_non_residential_income_test", op, join, Integer( personal_results_passes_non_residential_income_test_Enum'Pos( Passes_Non_Residential_Income_Test )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Passes_Non_Residential_Income_Test;


   procedure Add_Passes_Residential_Capital_Test( c : in out d.Criteria; Passes_Residential_Capital_Test : personal_results_passes_residential_capital_test_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "passes_residential_capital_test", op, join, Integer( personal_results_passes_residential_capital_test_Enum'Pos( Passes_Residential_Capital_Test )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Passes_Residential_Capital_Test;


   procedure Add_Passes_Residential_Income_Test( c : in out d.Criteria; Passes_Residential_Income_Test : personal_results_passes_residential_income_test_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "passes_residential_income_test", op, join, Integer( personal_results_passes_residential_income_test_Enum'Pos( Passes_Residential_Income_Test )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Passes_Residential_Income_Test;


   procedure Add_Passes_Residential_Means_Test( c : in out d.Criteria; Passes_Residential_Means_Test : personal_results_passes_residential_means_test_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "passes_residential_means_test", op, join, Integer( personal_results_passes_residential_means_test_Enum'Pos( Passes_Residential_Means_Test )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Passes_Residential_Means_Test;


   procedure Add_Passes_Non_Residential_Means_Test( c : in out d.Criteria; Passes_Non_Residential_Means_Test : personal_results_passes_non_residential_means_test_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "passes_non_residential_means_test", op, join, Integer( personal_results_passes_non_residential_means_test_Enum'Pos( Passes_Non_Residential_Means_Test )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Passes_Non_Residential_Means_Test;


   procedure Add_La_Contributions( c : in out d.Criteria; La_Contributions : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "la_contributions", op, join, La_Contributions );
   begin
      d.add_to_criteria( c, elem );
   end Add_La_Contributions;


   procedure Add_Client_Contributions( c : in out d.Criteria; Client_Contributions : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "client_contributions", op, join, Client_Contributions );
   begin
      d.add_to_criteria( c, elem );
   end Add_Client_Contributions;


   procedure Add_Gross_Care_Costs( c : in out d.Criteria; Gross_Care_Costs : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "gross_care_costs", op, join, Gross_Care_Costs );
   begin
      d.add_to_criteria( c, elem );
   end Add_Gross_Care_Costs;


   procedure Add_Total_Payments_To_Date( c : in out d.Criteria; Total_Payments_To_Date : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "total_payments_to_date", op, join, Total_Payments_To_Date );
   begin
      d.add_to_criteria( c, elem );
   end Add_Total_Payments_To_Date;


   procedure Add_Disposable_Income( c : in out d.Criteria; Disposable_Income : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "disposable_income", op, join, Disposable_Income );
   begin
      d.add_to_criteria( c, elem );
   end Add_Disposable_Income;


   procedure Add_Net_Income( c : in out d.Criteria; Net_Income : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "net_income", op, join, Net_Income );
   begin
      d.add_to_criteria( c, elem );
   end Add_Net_Income;


   procedure Add_Marginal_Rate( c : in out d.Criteria; Marginal_Rate : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "marginal_rate", op, join, Marginal_Rate );
   begin
      d.add_to_criteria( c, elem );
   end Add_Marginal_Rate;


   procedure Add_Capital_Contribution( c : in out d.Criteria; Capital_Contribution : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "capital_contribution", op, join, Capital_Contribution );
   begin
      d.add_to_criteria( c, elem );
   end Add_Capital_Contribution;


   procedure Add_Minimum_Income_Guarantee( c : in out d.Criteria; Minimum_Income_Guarantee : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "minimum_income_guarantee", op, join, Minimum_Income_Guarantee );
   begin
      d.add_to_criteria( c, elem );
   end Add_Minimum_Income_Guarantee;


   procedure Add_Hours_Of_Care_La( c : in out d.Criteria; Hours_Of_Care_La : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "hours_of_care_la", op, join, Hours_Of_Care_La );
   begin
      d.add_to_criteria( c, elem );
   end Add_Hours_Of_Care_La;


   procedure Add_Hours_Of_Care_Private( c : in out d.Criteria; Hours_Of_Care_Private : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "hours_of_care_private", op, join, Hours_Of_Care_Private );
   begin
      d.add_to_criteria( c, elem );
   end Add_Hours_Of_Care_Private;


   procedure Add_Uap( c : in out d.Criteria; Uap : personal_results_uap_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "uap", op, join, Integer( personal_results_uap_Enum'Pos( Uap )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Uap;


   procedure Add_Remaining_Capital_Stock( c : in out d.Criteria; Remaining_Capital_Stock : Amount; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "remaining_capital_stock", op, join, Real( Remaining_Capital_Stock ) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Remaining_Capital_Stock;


   
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


   procedure Add_Pid_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "pid", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Pid_To_Orderings;


   procedure Add_Wave_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "wave", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Wave_To_Orderings;


   procedure Add_Hid_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "hid", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Hid_To_Orderings;


   procedure Add_Buno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "buno", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Buno_To_Orderings;


   procedure Add_Adno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "adno", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Adno_To_Orderings;


   procedure Add_Passes_Non_Residential_Capital_Test_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "passes_non_residential_capital_test", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Passes_Non_Residential_Capital_Test_To_Orderings;


   procedure Add_Passes_Non_Residential_Income_Test_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "passes_non_residential_income_test", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Passes_Non_Residential_Income_Test_To_Orderings;


   procedure Add_Passes_Residential_Capital_Test_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "passes_residential_capital_test", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Passes_Residential_Capital_Test_To_Orderings;


   procedure Add_Passes_Residential_Income_Test_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "passes_residential_income_test", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Passes_Residential_Income_Test_To_Orderings;


   procedure Add_Passes_Residential_Means_Test_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "passes_residential_means_test", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Passes_Residential_Means_Test_To_Orderings;


   procedure Add_Passes_Non_Residential_Means_Test_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "passes_non_residential_means_test", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Passes_Non_Residential_Means_Test_To_Orderings;


   procedure Add_La_Contributions_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "la_contributions", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_La_Contributions_To_Orderings;


   procedure Add_Client_Contributions_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "client_contributions", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Client_Contributions_To_Orderings;


   procedure Add_Gross_Care_Costs_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "gross_care_costs", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Gross_Care_Costs_To_Orderings;


   procedure Add_Total_Payments_To_Date_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "total_payments_to_date", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Total_Payments_To_Date_To_Orderings;


   procedure Add_Disposable_Income_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "disposable_income", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Disposable_Income_To_Orderings;


   procedure Add_Net_Income_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "net_income", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Net_Income_To_Orderings;


   procedure Add_Marginal_Rate_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "marginal_rate", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Marginal_Rate_To_Orderings;


   procedure Add_Capital_Contribution_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "capital_contribution", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Capital_Contribution_To_Orderings;


   procedure Add_Minimum_Income_Guarantee_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "minimum_income_guarantee", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Minimum_Income_Guarantee_To_Orderings;


   procedure Add_Hours_Of_Care_La_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "hours_of_care_la", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Hours_Of_Care_La_To_Orderings;


   procedure Add_Hours_Of_Care_Private_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "hours_of_care_private", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Hours_Of_Care_Private_To_Orderings;


   procedure Add_Uap_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "uap", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Uap_To_Orderings;


   procedure Add_Remaining_Capital_Stock_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "remaining_capital_stock", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Remaining_Capital_Stock_To_Orderings;


   
end Personal_Results_IO;
