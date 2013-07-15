--
-- Created by ada_generator.py on 2012-02-10 02:09:15.516322
-- 
with Wsc_Db_Data;


with Ada.Containers.Vectors;
with Ada.Calendar;
with Environment;

with DB_Commons; 
with DB_Commons.ODBC; 

with GNATColl.Traces;
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

with Run_IO;


with Model.WSC.Users;

package body User_Type_IO is

   use Ada.Strings.Unbounded;
   use Ada.Exceptions;
   use Ada.Strings;
   use type Ada.Containers.Count_Type;
   
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
         "username, password, title, description, email, work_dir, utype, lang, preferences, last_used" &
         " from user_type " ;
   
   --
   -- Insert all variables; substring to be competed with output from some criteria
   --
   INSERT_PART : constant String := "insert into user_type (" &
         "username, password, title, description, email, work_dir, utype, lang, preferences, last_used" &
         " ) values " ;

   
   --
   -- delete all the records identified by the where SQL clause 
   --
   DELETE_PART : constant String := "delete from user_type ";
   
   --
   -- update
   --
   UPDATE_PART : constant String := "update user_type set  ";
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "USER_TYPE_IO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   --
   -- returns true if the primary key parts of User_Type match the defaults in Wsc_Db_Data.Null_User_Type
   --
   --
   -- Does this User_Type equal the default Wsc_Db_Data.Null_User_Type ?
   --
   function Is_Null( User_Type : Wsc_Db_Data.User_Type ) return Boolean is
   use Model.WSC.Users;
   begin
      return User_Type = Wsc_Db_Data.Null_User_Type;
   end Is_Null;


   
   --
   -- returns the single User_Type matching the primary key fields, or the Wsc_Db_Data.Null_User_Type record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String ) return Wsc_Db_Data.User_Type is
      l : Wsc_Db_Data.User_Type_List.Vector;
      User_Type : Wsc_Db_Data.User_Type;
      c : d.Criteria;
   begin      
      Add_Username( c, Username );
      l := Retrieve( c );
      if( not Wsc_Db_Data.User_Type_List.is_empty( l ) ) then
         User_Type := Wsc_Db_Data.User_Type_List.First_Element( l );
      else
         User_Type := Wsc_Db_Data.Null_User_Type;
      end if;
      return User_Type;
   end Retrieve_By_PK;

   
   --
   -- Retrieves a list of Wsc_Db_Data.User_Type matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.User_Type_List.Vector is
   begin      
      return Retrieve( d.to_string( c ) );
   end Retrieve;

   procedure Update_Last_Used( user : in out Wsc_Db_Data.User_Type ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query    : Unbounded_String := UPDATE_PART & To_Unbounded_String( " " );
      pk_c     : d.Criteria;
      values_c : d.Criteria;
   begin
      user.Last_Used := Ada.Calendar.Clock;
      Add_Username( pk_c, user.Username );
      Add_Last_Used( values_c, user.Last_Used );
      query := query & d.To_String( values_c, "," ) & d.to_string( pk_c );
      Log( "Update_Last_Used; executing query" & To_String(query) );
            begin -- exception block      
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         Log( "update; execute query OK" );
      exception 
         when No_Data => Null; -- ignore if no updates made
         when Error : others =>
            Log( "update: failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "update: exception thrown " & Exception_Information(Error) );
      end; -- exception block
      dodbc.Close_Prepared_Statement( ps );
      Connection_Pool.Return_Connection( connection );
   end Update_Last_Used;
   
   --
   -- Retrieves a list of Wsc_Db_Data.User_Type retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.User_Type_List.Vector is
      type Timestamp_Access is access all SQL_TIMESTAMP_STRUCT;
      type Real_Access is access all Real;
      type String_Access is access all String;

      l : Wsc_Db_Data.User_Type_List.Vector;
      ps : SQLHDBC := SQL_NULL_HANDLE;
      has_data : Boolean := false;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : constant String := SELECT_PART & " " & sqlstr;
      --
      -- aliased local versions of fields 
      --
      Username: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Password: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Title: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Description: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Email: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Work_Dir: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Utype: aliased Integer;
      Lang: aliased Integer;
      Preferences: aliased String := 
            "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
      Last_Used: aliased SQL_TIMESTAMP_STRUCT;
      --
      -- access variables for any variables retrieved via access types
      --
      Username_access : String_Access := Username'Access;
      Password_access : String_Access := Password'Access;
      Title_access : String_Access := Title'Access;
      Description_access : String_Access := Description'Access;
      Email_access : String_Access := Email'Access;
      Work_Dir_access : String_Access := Work_Dir'Access;
      Preferences_access : String_Access := Preferences'Access;
      Last_Used_access : Timestamp_Access := Last_Used'Access;
      --
      -- length holders for each retrieved variable
      --
      Username_len : aliased SQLLEN := Username'Length;
      Password_len : aliased SQLLEN := Password'Length;
      Title_len : aliased SQLLEN := Title'Length;
      Description_len : aliased SQLLEN := Description'Length;
      Email_len : aliased SQLLEN := Email'Length;
      Work_Dir_len : aliased SQLLEN := Work_Dir'Length;
      Utype_len : aliased SQLLEN := Utype'Size/8;
      Lang_len : aliased SQLLEN := Lang'Size/8;
      Preferences_len : aliased SQLLEN := Preferences'Length;
      Last_Used_len : aliased SQLLEN := Last_Used'Size/8;
      User_Type : Wsc_Db_Data.User_Type;
   begin
      Log( "retrieve made this as query " & query );
      begin -- exception block
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 1,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Username_access ),
            BufferLength     => username_len,
            StrLen_Or_IndPtr => Username_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 2,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Password_access.all'address ),
            BufferLength     => Password_len,
            StrLen_Or_IndPtr => Password_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 3,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Title_access.all'address ),
            BufferLength     => Title_len,
            StrLen_Or_IndPtr => Title_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 4,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Description_access.all'address ),
            BufferLength     => Description_len,
            StrLen_Or_IndPtr => Description_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 5,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Email_access.all'address ),
            BufferLength     => Email_len,
            StrLen_Or_IndPtr => Email_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 6,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Work_Dir_access.all'address ),
            BufferLength     => Work_Dir_len,
            StrLen_Or_IndPtr => Work_Dir_len'Access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 7,
            TargetValue      => Utype'Access,
            IndPtr           => Utype_len'Access );
         dodbc.I_Out_Binding.SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 8,
            TargetValue      => Lang'Access,
            IndPtr           => Lang_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            ColumnNumber     => 9,
            TargetType       => SQL_C_CHAR,
            TargetValuePtr   => To_SQLPOINTER( Preferences_access.all'address ),
            BufferLength     => Preferences_len,
            StrLen_Or_IndPtr => Preferences_len'Access );
         SQLBindCol(
            StatementHandle  => ps,
            TargetType       => SQL_C_TYPE_TIMESTAMP,
            ColumnNumber     => 10,
            TargetValuePtr   => To_SQLPOINTER( Last_Used_access.all'address ),
            BufferLength     => 0,
            StrLen_Or_IndPtr => Last_Used_len'Access );
         SQLExecute( ps );
         loop
            dodbc.next_row( ps, has_data );
            if( not has_data ) then
               exit;
            end if;
            Log( "username |" & username & "| Username_len" & Username_len'Img );
            User_Type.Username := Slice_To_Unbounded( Username, 1, Natural( Username_len ) );
            User_Type.Password := Slice_To_Unbounded( Password, 1, Natural( Password_len ) );
            User_Type.Title := Slice_To_Unbounded( Title, 1, Natural( Title_len ) );
            User_Type.Description := Slice_To_Unbounded( Description, 1, Natural( Description_len ) );
            User_Type.Email := Slice_To_Unbounded( Email, 1, Natural( Email_len ) );
            if( work_dir_len > 0 )then
               null;
               -- User_Type.Work_Dir := Slice_To_Unbounded( Work_Dir, 1, Natural( Work_Dir_len ) );
            end if;
            User_Type.Utype := User_Class'Val( Utype );
            User_Type.Lang := Languages'Val( Lang );
            if( preferences_len > 0 )then
               null;
               -- User_Type.Preferences := Slice_To_Unbounded( Preferences, 1, Natural( Preferences_len ) );
            end if;
            if( preferences_len > 0 )then
               User_Type.Last_Used := dodbc.Timestamp_To_Ada_Time( Last_Used_access.all );
            end if;
            Wsc_Db_Data.User_Type_List.append( l, User_Type );        
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
      if( l.Length = 0 )then
         Log( "user_type_IO; NO DATA FOUND!" );
      end if;
      return l;
   end Retrieve;

   
   --
   -- Update the given record 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Update( User_Type : Wsc_Db_Data.User_Type ) is
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection := Connection_Pool.Lease;
      query : Unbounded_String := UPDATE_PART & To_Unbounded_String(" ");
      pk_c : d.Criteria;
      values_c : d.Criteria;
   begin
      --
      -- values to be updated
      --
      Add_Password( values_c, User_Type.Password );
      Add_Title( values_c, User_Type.Title );
      Add_Description( values_c, User_Type.Description );
      Add_Email( values_c, User_Type.Email );
      Add_Work_Dir( values_c, User_Type.Work_Dir );
      Add_Utype( values_c, User_Type.Utype );
      Add_Lang( values_c, User_Type.Lang );
      Add_Preferences( values_c, User_Type.Preferences );
      Add_Last_Used( values_c, User_Type.Last_Used );
      --
      -- primary key fields
      --
      Add_Username( pk_c, User_Type.Username );
      query := query & d.To_String( values_c, "," ) & d.to_string( pk_c );
      Log( "update; executing query" & To_String(query) );
      begin -- exception block      
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         Log( "update; execute query OK" );
      exception 
         when No_Data => Null; -- ignore if no updates made
         when Error : others =>
            Log( "update: failed with message " & Exception_Information(Error)  );
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
   procedure Save( User_Type : Wsc_Db_Data.User_Type; overwrite : Boolean := True ) is   
      ps : sql.SQLHDBC := sql.SQL_NULL_HANDLE;
      connection : dodbc.Database_Connection;
      query : Unbounded_String := INSERT_PART & To_Unbounded_String(" ");
      c : d.Criteria;
      User_Type_Tmp : Wsc_Db_Data.User_Type;
   begin
      if( overwrite ) then
         User_Type_Tmp := retrieve_By_PK( User_Type.Username );
         if( not is_Null( User_Type_Tmp )) then
            Update( User_Type );
            return;
         end if;
      end if;
      Add_Username( c, User_Type.Username );
      Add_Password( c, User_Type.Password );
      Add_Title( c, User_Type.Title );
      Add_Description( c, User_Type.Description );
      Add_Email( c, User_Type.Email );
      Add_Work_Dir( c, User_Type.Work_Dir );
      Add_Utype( c, User_Type.Utype );
      Add_Lang( c, User_Type.Lang );
      Add_Preferences( c, User_Type.Preferences );
      Add_Last_Used( c, User_Type.Last_Used );
      query := query & "( "  & d.To_Crude_Array_Of_Values( c ) & " )";
      Log( "save; executing query" & To_String(query) );
      begin
         connection := Connection_Pool.Lease;
         ps := dodbc.Initialise_Prepared_Statement( connection.connection, query );       
         SQLExecute( ps );
         Log( "save; execute query OK" );
         
      exception 
         when Error : others =>
            Log( "save; execute query failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "save: exception thrown " & Exception_Information(Error) );
      end;
      begin
         dodbc.Close_Prepared_Statement( ps );
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            Log( "save/close " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "save/close: exception " & Exception_Information(Error) );
      end;
      
   end Save;


   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_User_Type
   --

   procedure Delete( User_Type : in out Wsc_Db_Data.User_Type ) is
         c : d.Criteria;
   begin  
      Add_Username( c, User_Type.Username );
      delete( c );
      User_Type := Wsc_Db_Data.Null_User_Type;
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
            Log( "delete; execute query failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "delete: exception thrown " & Exception_Information(Error) );
      end;
      begin -- try catch block for close
         dodbc.Close_Prepared_Statement( ps );
         Connection_Pool.Return_Connection( connection );
      exception 
         when Error : others =>
            Log( "delete; execute query failed with message " & Exception_Information(Error)  );
            Raise_Exception( d.DB_Exception'Identity, 
               "delete: exception thrown " & Exception_Information(Error) );
      end;
   end Delete;


   --
   -- functions to retrieve records from tables with foreign keys
   -- referencing the table modelled by this package
   --
   function Retrieve_Associated_Runs( User_Type : Wsc_Db_Data.User_Type ) return Run_List.Vector is
      c : d.Criteria;
   begin
      Run_IO.Add_Username( c, User_Type.Username );
      return Run_IO.retrieve( c );
   end Retrieve_Associated_Runs;



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


   procedure Add_Password( c : in out d.Criteria; Password : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "password", op, join, To_String( Password ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Password;


   procedure Add_Password( c : in out d.Criteria; Password : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "password", op, join, Password, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Password;


   procedure Add_Title( c : in out d.Criteria; Title : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "title", op, join, To_String( Title ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Title;


   procedure Add_Title( c : in out d.Criteria; Title : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "title", op, join, Title, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Title;


   procedure Add_Description( c : in out d.Criteria; Description : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "description", op, join, To_String( Description ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Description;


   procedure Add_Description( c : in out d.Criteria; Description : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "description", op, join, Description, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Description;


   procedure Add_Email( c : in out d.Criteria; Email : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "email", op, join, To_String( Email ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Email;


   procedure Add_Email( c : in out d.Criteria; Email : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "email", op, join, Email, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Email;


   procedure Add_Work_Dir( c : in out d.Criteria; Work_Dir : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "work_dir", op, join, To_String( Work_Dir ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Work_Dir;


   procedure Add_Work_Dir( c : in out d.Criteria; Work_Dir : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "work_dir", op, join, Work_Dir, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Work_Dir;


   procedure Add_Utype( c : in out d.Criteria; Utype : User_Class; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "utype", op, join, Integer( User_Class'Pos( Utype )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Utype;


   procedure Add_Lang( c : in out d.Criteria; Lang : Languages; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "lang", op, join, Integer( Languages'Pos( Lang )) );
   begin
      d.add_to_criteria( c, elem );
   end Add_Lang;


   procedure Add_Preferences( c : in out d.Criteria; Preferences : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "preferences", op, join, To_String( Preferences ), 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Preferences;


   procedure Add_Preferences( c : in out d.Criteria; Preferences : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "preferences", op, join, Preferences, 60 );
   begin
      d.add_to_criteria( c, elem );
   end Add_Preferences;


   procedure Add_Last_Used( c : in out d.Criteria; Last_Used : Ada.Calendar.Time; op : d.operation_type:= d.eq; join : d.join_type := d.join_and ) is   
   elem : d.Criterion := d.make_Criterion_Element( "last_used", op, join, Last_Used );
   begin
      d.add_to_criteria( c, elem );
   end Add_Last_Used;


   
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "username", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Username_To_Orderings;


   procedure Add_Password_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "password", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Password_To_Orderings;


   procedure Add_Title_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "title", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Title_To_Orderings;


   procedure Add_Description_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "description", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Description_To_Orderings;


   procedure Add_Email_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "email", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Email_To_Orderings;


   procedure Add_Work_Dir_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "work_dir", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Work_Dir_To_Orderings;


   procedure Add_Utype_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "utype", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Utype_To_Orderings;


   procedure Add_Lang_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "lang", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Lang_To_Orderings;


   procedure Add_Preferences_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "preferences", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Preferences_To_Orderings;


   procedure Add_Last_Used_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc ) is   
   elem : d.Order_By_Element := d.Make_Order_By_Element( "last_used", direction  );
   begin
      d.add_to_criteria( c, elem );
   end Add_Last_Used_To_Orderings;


   
end User_Type_IO;
