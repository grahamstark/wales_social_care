--
-- Created by ada_generator.py on 2012-02-14 10:54:29.998480
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;
with Keyed_Text_Buffer;

package Key_Value_Parameter_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   use Keyed_Text_Buffer;
   function Next_Free_Run_Id return integer;

   --
   -- returns true if the primary key parts of Key_Value_Parameter match the defaults in Wsc_Db_Data.Null_Key_Value_Parameter
   --
   function Is_Null( Key_Value_Parameter : Wsc_Db_Data.Key_Value_Parameter ) return Boolean;
   
   --
   -- returns the single Key_Value_Parameter matching the primary key fields, or the Wsc_Db_Data.Null_Key_Value_Parameter record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String; Run_Id : integer; Key : Unbounded_String ) return Wsc_Db_Data.Key_Value_Parameter;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Key_Value_Parameter matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Key_Value_Parameter_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Key_Value_Parameter retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Key_Value_Parameter_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Key_Value_Parameter : Wsc_Db_Data.Key_Value_Parameter; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Key_Value_Parameter
   --
   procedure Delete( Key_Value_Parameter : in out Wsc_Db_Data.Key_Value_Parameter );
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
   procedure Bulk_Save( username : Unbounded_String; run_id : Natural; kvs : Text_Buffer );
   --
   -- functions to add something to a criteria
   --
   procedure Add_Username( c : in out d.Criteria; Username : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Username( c : in out d.Criteria; Username : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Key( c : in out d.Criteria; Key : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Key( c : in out d.Criteria; Key : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Val( c : in out d.Criteria; Val : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Val( c : in out d.Criteria; Val : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Key_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Val_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Key_Value_Parameter_IO;
