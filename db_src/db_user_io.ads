--
-- Created by ada_generator.py on 2012-02-10 01:44:01.707457
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

package Db_User_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   

   --
   -- returns true if the primary key parts of Db_User match the defaults in Wsc_Db_Data.Null_Db_User
   --
   function Is_Null( Db_User : Wsc_Db_Data.Db_User ) return Boolean;
   
   --
   -- returns the single Db_User matching the primary key fields, or the Wsc_Db_Data.Null_Db_User record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String ) return Wsc_Db_Data.Db_User;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Db_User matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Db_User_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Db_User retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Db_User_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Db_User : Wsc_Db_Data.Db_User; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Db_User
   --
   procedure Delete( Db_User : in out Wsc_Db_Data.Db_User );
   --
   -- delete the records indentified by the criteria
   --
   procedure Delete( c : d.Criteria );
   --
   -- delete all the records identified by the where SQL clause 
   --
   procedure Delete( where_Clause : String );
   --
   -- functions to retrieve records from tables with foreign keys
   -- referencing the table modelled by this package
   --
   function Retrieve_Associated_Runs( Db_User : Wsc_Db_Data.Db_User ) return Wsc_Db_Data.Run_List.Vector;

   --
   -- functions to add something to a criteria
   --
   procedure Add_Username( c : in out d.Criteria; Username : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Username( c : in out d.Criteria; Username : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Password( c : in out d.Criteria; Password : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Password( c : in out d.Criteria; Password : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Title( c : in out d.Criteria; Title : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Title( c : in out d.Criteria; Title : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Description( c : in out d.Criteria; Description : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Description( c : in out d.Criteria; Description : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Email( c : in out d.Criteria; Email : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Email( c : in out d.Criteria; Email : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Work_Dir( c : in out d.Criteria; Work_Dir : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Work_Dir( c : in out d.Criteria; Work_Dir : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Utype( c : in out d.Criteria; Utype : utype_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Lang( c : in out d.Criteria; Lang : lang_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Preferences( c : in out d.Criteria; Preferences : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Preferences( c : in out d.Criteria; Preferences : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Last_Used( c : in out d.Criteria; Last_Used : Ada.Calendar.Time; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Password_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Title_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Description_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Email_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Work_Dir_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Utype_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Lang_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Preferences_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Last_Used_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Db_User_IO;
