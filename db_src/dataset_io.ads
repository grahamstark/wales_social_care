--
-- Created by ada_generator.py on 2012-05-03 16:03:51.792919
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;
with Model.WSC.Run_Declarations;
   
package Dataset_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   use Model.WSC.Run_Declarations;
   

   --
   -- returns true if the primary key parts of Dataset match the defaults in Wsc_Db_Data.Null_Dataset
   --
   function Is_Null( ds : Dataset ) return Boolean;
   
   --
   -- returns the single Dataset matching the primary key fields, or the Wsc_Db_Data.Null_Dataset record
   -- if no such record exists
   --
   function Retrieve_By_PK( Name : Unbounded_String ) return Dataset;
   
   --
   -- Retrieves a list of Dataset matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Dataset_List.Vector;
   
   --
   -- Retrieves a list of Dataset retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Dataset_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( ds : Dataset; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Dataset
   --
   procedure Delete( ds : in out Dataset );
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
   function Retrieve_Associated_Runs( ds : Dataset ) return Run_List.Vector;

   --
   -- functions to add something to a criteria
   --
   procedure Add_Name( c : in out d.Criteria; Name : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Name( c : in out d.Criteria; Name : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Creator( c : in out d.Criteria; Creator : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Creator( c : in out d.Criteria; Creator : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Title( c : in out d.Criteria; Title : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Title( c : in out d.Criteria; Title : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Creator_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Title_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Dataset_IO;
