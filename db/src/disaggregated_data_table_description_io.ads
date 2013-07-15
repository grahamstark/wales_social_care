--
-- Created by ada_generator.py on 2012-07-24 19:03:55.763462
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

with WSC_Enums;
with Base_Types;

package Disaggregated_Data_Table_Description_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   
   use WSC_Enums;
   use Base_Types;
   

   --
   -- returns true if the primary key parts of Disaggregated_Data_Table_Description match the defaults in Wsc_Db_Data.Null_Disaggregated_Data_Table_Description
   --
   function Is_Null( Disaggregated_Data_Table_Description : Wsc_Db_Data.Disaggregated_Data_Table_Description ) return Boolean;
   
   --
   -- returns the single Disaggregated_Data_Table_Description matching the primary key fields, or the Wsc_Db_Data.Null_Disaggregated_Data_Table_Description record
   -- if no such record exists
   --
   function Retrieve_By_PK( Model_Table_Name : Unbounded_String ) return Wsc_Db_Data.Disaggregated_Data_Table_Description;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table_Description matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Disaggregated_Data_Table_Description_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table_Description retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Disaggregated_Data_Table_Description_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Disaggregated_Data_Table_Description : Wsc_Db_Data.Disaggregated_Data_Table_Description; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Disaggregated_Data_Table_Description
   --
   procedure Delete( Disaggregated_Data_Table_Description : in out Wsc_Db_Data.Disaggregated_Data_Table_Description );
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
   function Retrieve_Associated_Disaggregated_Data_Table_Cell_Descriptions( Disaggregated_Data_Table_Description : Wsc_Db_Data.Disaggregated_Data_Table_Description ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description_List.Vector;
   function Retrieve_Associated_Disaggregated_Data_Tables( Disaggregated_Data_Table_Description : Wsc_Db_Data.Disaggregated_Data_Table_Description ) return Wsc_Db_Data.Disaggregated_Data_Table_List.Vector;

   --
   -- functions to add something to a criteria
   --
   procedure Add_Model_Table_Name( c : in out d.Criteria; Model_Table_Name : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Model_Table_Name( c : in out d.Criteria; Model_Table_Name : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Filename( c : in out d.Criteria; Filename : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Filename( c : in out d.Criteria; Filename : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Has_Totals( c : in out d.Criteria; Has_Totals : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Table_Type( c : in out d.Criteria; Table_Type : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Table_Type( c : in out d.Criteria; Table_Type : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Table_Subtype( c : in out d.Criteria; Table_Subtype : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Table_Subtype( c : in out d.Criteria; Table_Subtype : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Model_Table_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Filename_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Has_Totals_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Table_Type_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Table_Subtype_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Disaggregated_Data_Table_Description_IO;
