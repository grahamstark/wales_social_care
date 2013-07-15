--
-- Created by ada_generator.py on 2011-11-29 16:58:56.623827
-- 
with Wsc_Db_Data;
with db_commons;
with base_types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

package Highest_Previous_Personal_Income_IO is
  
   package d renames db_commons;   
   use base_types;
   use Ada.Strings.Unbounded;
   

   --
   -- returns true if the primary key parts of Highest_Previous_Personal_Income match the defaults in Wsc_Db_Data.Null_Highest_Previous_Personal_Income
   --
   function Is_Null( Highest_Previous_Personal_Income : Wsc_Db_Data.Highest_Previous_Personal_Income ) return Boolean;
   
   --
   -- returns the single Highest_Previous_Personal_Income matching the primary key fields, or the Wsc_Db_Data.Null_Highest_Previous_Personal_Income record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String; Run_Id : Unbounded_String; Pid : Unbounded_String; Wave : Unbounded_String; Income_Name : Unbounded_String ) return Wsc_Db_Data.Highest_Previous_Personal_Income;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Highest_Previous_Personal_Income matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Highest_Previous_Personal_Income_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Highest_Previous_Personal_Income retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Highest_Previous_Personal_Income_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Highest_Previous_Personal_Income : Wsc_Db_Data.Highest_Previous_Personal_Income; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Highest_Previous_Personal_Income
   --
   procedure Delete( Highest_Previous_Personal_Income : in out Wsc_Db_Data.Highest_Previous_Personal_Income );
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

   --
   -- functions to add something to a criteria
   --
   procedure Add_Username( c : in out d.Criteria; Username : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Username( c : in out d.Criteria; Username : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Pid( c : in out d.Criteria; Pid : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Pid( c : in out d.Criteria; Pid : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Wave( c : in out d.Criteria; Wave : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Wave( c : in out d.Criteria; Wave : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Hid( c : in out d.Criteria; Hid : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Hid( c : in out d.Criteria; Hid : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Buno( c : in out d.Criteria; Buno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Adno( c : in out d.Criteria; Adno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Income_Name( c : in out d.Criteria; Income_Name : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Income_Name( c : in out d.Criteria; Income_Name : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Value( c : in out d.Criteria; Value : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Pid_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Wave_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Hid_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Buno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Adno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Income_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Value_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Highest_Previous_Personal_Income_IO;
