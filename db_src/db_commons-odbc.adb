--
-- Created by ada_generator.py on 2012-02-09 16:45:16.127809
-- 
with GNU.DB.SQLCLI;
with GNU.DB.SQLCLI.Environment_Attribute;

with GNATColl.Traces;

with Ada.Strings.Unbounded; 
with Ada.Exceptions;  
with DB_Commons; 
with Ada.Calendar;
with Ada.Text_IO;

package body DB_Commons.ODBC is

   use Ada.Strings.Unbounded;
   use Ada.Exceptions;
   use Ada.Text_IO;
   
   package dbenv renames GNU.DB.SQLCLI.Environment_Attribute;
 
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "DB_COMMONS.ODBC" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   
   function Describe_Column(
      ps     : SQLHSTMT;   
      col_no : SQL_Column_Number ) return String is
      data_type        : aliased SQL_DATA_TYPE;
      column_size      : aliased SQLUINTEGER;
      decimal_digits   : aliased SQLSMALLINT;
      nullable         : aliased SQL_NULLABLE_INFO;
      error_code       : aliased SQLRETURN;
      s : constant String := SQLDescribeCol(
            StatementHandle => ps,
            ColumnNumber    => col_no,
            MaxNameLength   => SQLSMALLINT'Last,
            DataType        => data_type'Access,
            ColumnSize      => column_size'Access,
            DecimalDigits   => decimal_digits'Access,
            Nullable        => nullable'Access,
            ErrorCode       => error_code'Access );
   begin
      return " name |" & s 
            & "| data_type = |" & SQL_DATA_TYPE'Image( data_type )
            & "| column_size = |" & SQLUINTEGER'Image( column_size )
            & "| decimal_digits = |" & SQLSMALLINT'Image( decimal_digits )
            & "| nullable = |" & SQL_NULLABLE_INFO'Image( nullable )
            & "| error_code = |" & SQLRETURN'Image( error_code ) & "| ";
   end Describe_Column;


   
   -- return a string like '2007-09-24 16:07:47'
   function To_String( t : SQL_TIMESTAMP_STRUCT ) return String is
   begin
      return
      "SQL_TIMESTAMP_STRUCT:: year = " & t.year'Img &
      "| month = " & t.month'Img &
      "| day = " & t.day'Img &
      "| hour = " & t.hour'Img &
      "| minute = " & t.minute'Img &
      "| second = " & t.second'Img &
      "| fraction = " & t.fraction'Img;
   end To_String;
   
   function To_String( t : Ada.Calendar.Time ) return String is 
   begin
      return To_String( Ada_Time_to_Timestamp( t ) );
   end To_String;
   
   function Timestamp_To_Date_Time( sqlTime : SQL_TIMESTAMP_STRUCT ) return Date_Time_Rec is
      dt : DB_Commons.Date_Time_Rec;
   begin
      dt.year := integer( sqlTime.year );
      dt.month := integer( sqlTime.month );
      dt.day := integer( sqlTime.day );
      dt.hour := integer( sqlTime.hour );
      dt.minute := integer( sqlTime.minute );
      dt.second := integer( sqlTime.second );
      dt.fraction := real( sqlTime.fraction );
      Log( "sqlTime : " & To_String( sqlTime ));
      Log( "Date_Time_Rec : " & To_String( dt ));
      return dt;
   end Timestamp_To_Date_Time;
   
   function Date_Time_To_Timestamp( dt : DB_Commons.Date_Time_Rec ) return SQL_TIMESTAMP_STRUCT is
      sqlTime : SQL_TIMESTAMP_STRUCT;
   begin
      sqlTime.year := SQLSMALLINT( dt.year );
      sqlTime.month := SQLUSMALLINT( dt.month );
      sqlTime.day := SQLUSMALLINT( dt.day );
      sqlTime.hour := SQLUSMALLINT( dt.hour );
      sqlTime.minute := SQLUSMALLINT( dt.minute );
      sqlTime.second := SQLUSMALLINT( dt.second );
      sqlTime.fraction := SQL_TIME_FRACTION( dt.fraction );
      return sqlTime;
   end Date_Time_To_Timestamp;
   
   function Ada_Time_To_Timestamp( adaTime : Ada.Calendar.Time ) return SQL_TIMESTAMP_STRUCT is
      dt : Date_Time_Rec := Ada_Time_To_Date_Time( adaTime );
   begin
      return Date_Time_To_Timestamp( dt );
   end Ada_Time_To_Timestamp;

   function Timestamp_To_Ada_Time( sqlTime : SQL_TIMESTAMP_STRUCT ) return Ada.Calendar.Time is
     tr : Date_Time_Rec := Timestamp_To_Date_Time( sqlTime );
   begin
      return Date_Time_To_Ada_Time( tr );
   end Timestamp_To_Ada_Time;   
   
   procedure Execute( ps : in out SQLHDBC ) is
   begin
       SQLExecute ( ps );  
   exception 
      when Error: others =>
         Raise_Exception( DB_Exception'Identity, "Error in db_common-odbc.adb: Disconnect " & Exception_Information(Error) );
   end Execute;
   
   procedure Next_Row( ps : in out SQLHDBC; has_data : in out boolean ) is
   begin
      SQLFetch ( ps );
      has_data := true;
   exception
         when No_Data =>
            has_data := false;
         when Error: others =>
            Raise_Exception( DB_Exception'Identity, "Error in db_common-odbc.adb: next_row " & Exception_Information(Error) );
   end Next_Row;
   
   function Connect( Server_Name : String; User_Name : String; password : String ) return Database_Connection is
      con : Database_Connection;
   begin
      SQLAllocHandle( SQL_HANDLE_ENV, SQL_NULL_HANDLE, con.Environment );
      dbenv.SQLSetEnvAttr( con.Environment,  
                      dbenv.Environment_Attribute_ODBC_Version'
                    ( Attribute => dbenv.SQL_ATTR_ODBC_VERSION,
                      Value     => dbenv.SQL_OV_ODBC3 ));

      SQLAllocHandle( SQL_HANDLE_DBC, con.Environment, con.connection );
      SQLConnect 
         (ConnectionHandle    => con.connection,
               ServerName     => Server_Name,
               UserName       => User_Name,
               Authentication => password );
      return con;
   exception 
      when Error: others =>
         Raise_Exception( DB_Exception'Identity, "Error in db_common-odbc.adb: Connect " & 
            "server name: " & server_name & 
            " user_name: " & user_name &
            " password: " & password &
            Exception_Information(Error) );
   end Connect;
   
   procedure Disconnect( con : in out Database_Connection ) is
   begin
      SQLDisconnect ( con.connection );
      SQLFreeHandle (SQL_HANDLE_DBC, con.connection );
      SQLFreeHandle (SQL_HANDLE_ENV, con.Environment );
      con := Null_Database_Connection;
   exception 
      when Error: others =>
         Raise_Exception( DB_Exception'Identity, "Error in db_common-odbc.adb: Disconnect " & Exception_Information(Error) );
   end Disconnect;

   
   function Initialise_Prepared_Statement( con : SQLHDBC; query : String ) return SQLHDBC is
      statement : SQLHDBC;
   begin
      SQLAllocHandle ( SQL_HANDLE_STMT, con, statement );
      SQLPrepare ( statement, query );
      return statement;
   exception
      when Error: others =>
         Raise_Exception( DB_Exception'Identity, "Error in db_common.ODBC.adb: Initialise_Prepared_Statement " & Exception_Information(Error) );      
   end Initialise_Prepared_Statement;

   function Initialise_Prepared_Statement( con : SQLHDBC; query : Unbounded_String ) return SQLHDBC is
   begin
      return Initialise_Prepared_Statement( con, To_String(query) );
   end Initialise_Prepared_Statement;

   
   procedure Close_Prepared_Statement( statement : in out SQLHDBC ) is
   begin
         SQLFreeHandle (SQL_HANDLE_STMT, statement );
         statement := SQL_NULL_HANDLE;
   exception
      when Error: others =>
         Raise_Exception( DB_Exception'Identity, "Error in db_common.ODBC.adb: Connect " & Exception_Information(Error) );      
   end Close_Prepared_Statement;
   
   
   procedure Commit( con : in out SQLHDBC ) is 
   begin
      SQLCommit ( con );
   exception
      when Error: others =>
         Raise_Exception( DB_Exception'Identity, "Error in db_common-odbc.adb.ODBC: Commit " & Exception_Information(Error) );      
   end Commit;
   
   --
   -- Execute a script
   -- otherwise throws DB_Exception exception. 
   --
   procedure Execute_Script( connection : Database_Connection; script : String ) is
      ps : SQLHDBC := SQL_NULL_HANDLE;
   begin
      begin -- exception block      
         ps := Initialise_Prepared_Statement( connection.connection, script );       
         SQLExecute( ps );
      exception 
         when No_Data => Null; -- ignore if no updates made
         when Error : others =>
            Raise_Exception( DB_Exception'Identity, 
               "update: exception thrown " & Exception_Information(Error) );
      end; -- exception block
      Close_Prepared_Statement( ps );
   end Execute_Script;



end DB_Commons.ODBC;
