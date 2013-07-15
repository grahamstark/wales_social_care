--
-- Created by ada_generator.py on 2012-07-24 19:03:55.797121
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

package body Maxima_And_Totals_IO is

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
         "lifetime_la_contributions, lifetime_client_contributions, lifetime_gross_payments, lifetime_capital_contributions, highest_la_contribution " &
         " from maxima_and_totals " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into maxima_and_totals (" &
         "lifetime_la_contributions, lifetime_client_contributions, lifetime_gross_payments, lifetime_capital_contributions, highest_la_contribution " &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from maxima_and_totals ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update maxima_and_totals set  ";
   
   

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
   -- returns the single Maxima_And_Totals matching the primary key fields, or the Wsc_Db_Data.Null_Maxima_And_Totals record
   -- if no such record exists
   --

   
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
      --
      -- aliased local versions of fields 
      --
      Lifetime_La_Contributions: aliased Real;
      Lifetime_Client_Contributions: aliased Real;
      Lifetime_Gross_Payments: aliased Real;
      Lifetime_Capital_Contributions: aliased Real;
      Highest_La_Contribution: aliased Real;
      --
      -- access variables for any variables retrieved via access types
      --
      Lifetime_La_Contributions_access : Real_Access := Lifetime_La_Contributions'Access;
      Lifetime_Client_Contributions_access : Real_Access := Lifetime_Client_Contributions'Access;
      Lifetime_Gross_Payments_access : Real_Access := Lifetime_Gross_Payments'Access;
      Lifetime_Capital_Contributions_access : Real_Access := Lifetime_Capital_Contributions'Access;
      Highest_La_Contribution_access : Real_Access := Highest_La_Contribution'Access;
      --
      -- length holders for each retrieved variable
      --
      Lifetime_La_Contributions_len : aliased SQLINTEGER := Lifetime_La_Contributions'Size/8;
      Lifetime_Client_Contributions_len : aliased SQLINTEGER := Lifetime_Client_Contributions'Size/8;
      Lifetime_Gross_Payments_len : aliased SQLINTEGER := Lifetime_Gross_Payments'Size/8;
      Lifetime_Capital_Contributions_len : aliased SQLINTEGER := Lifetime_Capital_Contributions'Size/8;
      Highest_La_Contribution_len : aliased SQLINTEGER := Highest_La_Contribution'Size/8;
      Maxima_And_Totals : Wsc_Db_Data.Maxima_And_Totals;
   begin
      DB_Logger.info( "retrieve made this as query " & query );
      begin -- exception block
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 1,
            TargetValuePtr   => To_SQLPOINTER( Lifetime_La_Contributions_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Lifetime_La_Contributions_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 2,
            TargetValuePtr   => To_SQLPOINTER( Lifetime_Client_Contributions_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Lifetime_Client_Contributions_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 3,
            TargetValuePtr   => To_SQLPOINTER( Lifetime_Gross_Payments_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Lifetime_Gross_Payments_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 4,
            TargetValuePtr   => To_SQLPOINTER( Lifetime_Capital_Contributions_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Lifetime_Capital_Contributions_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_DOUBLE,
            ColumnNumber     => 5,
            TargetValuePtr   => To_SQLPOINTER( Highest_La_Contribution_access.all'Address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Highest_La_Contribution_len'Access );
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
   procedure Update( Maxima_And_Totals : Wsc_Db_Data.Maxima_And_Totals ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Lifetime_La_Contributions( values_c, Maxima_And_Totals.Lifetime_La_Contributions );
      Add_Lifetime_Client_Contributions( values_c, Maxima_And_Totals.Lifetime_Client_Contributions );
      Add_Lifetime_Gross_Payments( values_c, Maxima_And_Totals.Lifetime_Gross_Payments );
      Add_Lifetime_Capital_Contributions( values_c, Maxima_And_Totals.Lifetime_Capital_Contributions );
      Add_Highest_La_Contribution( values_c, Maxima_And_Totals.Highest_La_Contribution );
      --
      -- primary key fields
      --
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
   procedure Save( Maxima_And_Totals : Wsc_Db_Data.Maxima_And_Totals; overwrite : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      Maxima_And_Totals_Tmp : Wsc_Db_Data.Maxima_And_Totals;
   begin
      Add_Lifetime_La_Contributions( c, Maxima_And_Totals.Lifetime_La_Contributions );
      Add_Lifetime_Client_Contributions( c, Maxima_And_Totals.Lifetime_Client_Contributions );
      Add_Lifetime_Gross_Payments( c, Maxima_And_Totals.Lifetime_Gross_Payments );
      Add_Lifetime_Capital_Contributions( c, Maxima_And_Totals.Lifetime_Capital_Contributions );
      Add_Highest_La_Contribution( c, Maxima_And_Totals.Highest_La_Contribution );
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
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Maxima_And_Totals
   --

   procedure Delete( Maxima_And_Totals : in out Wsc_Db_Data.Maxima_And_Totals ) is
         c : d.Criteria;
   begin  
      delete( c );
      Maxima_And_Totals := Wsc_Db_Data.Null_Maxima_And_Totals;
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
   procedure Add_Lifetime_La_Contributions( c : in out d.Criteria; Lifetime_La_Contributions : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "lifetime_la_contributions", op, join, Lifetime_La_Contributions );
   begin
      d.add_to_criteria( c, elem );
   end Add_Lifetime_La_Contributions;


   procedure Add_Lifetime_Client_Contributions( c : in out d.Criteria; Lifetime_Client_Contributions : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "lifetime_client_contributions", op, join, Lifetime_Client_Contributions );
   begin
      d.add_to_criteria( c, elem );
   end Add_Lifetime_Client_Contributions;


   procedure Add_Lifetime_Gross_Payments( c : in out d.Criteria; Lifetime_Gross_Payments : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "lifetime_gross_payments", op, join, Lifetime_Gross_Payments );
   begin
      d.add_to_criteria( c, elem );
   end Add_Lifetime_Gross_Payments;


   procedure Add_Lifetime_Capital_Contributions( c : in out d.Criteria; Lifetime_Capital_Contributions : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "lifetime_capital_contributions", op, join, Lifetime_Capital_Contributions );
   begin
      d.add_to_criteria( c, elem );
   end Add_Lifetime_Capital_Contributions;


   procedure Add_Highest_La_Contribution( c : in out d.Criteria; Highest_La_Contribution : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "highest_la_contribution", op, join, Highest_La_Contribution );
   begin
      d.add_to_criteria( c, elem );
   end Add_Highest_La_Contribution;


   
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Lifetime_La_Contributions_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "lifetime_la_contributions", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Lifetime_La_Contributions_To_Orderings;


   procedure Add_Lifetime_Client_Contributions_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "lifetime_client_contributions", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Lifetime_Client_Contributions_To_Orderings;


   procedure Add_Lifetime_Gross_Payments_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "lifetime_gross_payments", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Lifetime_Gross_Payments_To_Orderings;


   procedure Add_Lifetime_Capital_Contributions_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "lifetime_capital_contributions", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Lifetime_Capital_Contributions_To_Orderings;


   procedure Add_Highest_La_Contribution_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "highest_la_contribution", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Highest_La_Contribution_To_Orderings;


   
end Maxima_And_Totals_IO;
