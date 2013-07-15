--
-- Created by ada_generator.py on 2012-02-12 11:09:11.276141
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

package Disaggregated_Data_Table_Cell_Description_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   
   function Next_Free_Cell_Pos return integer;

   --
   -- returns true if the primary key parts of Disaggregated_Data_Table_Cell_Description match the defaults in Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell_Description
   --
   function Is_Null( Disaggregated_Data_Table_Cell_Description : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description ) return Boolean;
   
   --
   -- returns the single Disaggregated_Data_Table_Cell_Description matching the primary key fields, or the Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell_Description record
   -- if no such record exists
   --
   function Retrieve_By_PK( Model_Table_Name : Unbounded_String; Cell_Pos : integer ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Disaggregated_Data_Table_Cell_Description : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell_Description
   --
   procedure Delete( Disaggregated_Data_Table_Cell_Description : in out Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description );
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
   function Retrieve_Associated_Disaggregated_Data_Table_Cells( Disaggregated_Data_Table_Cell_Description : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.Vector;

   --
   -- functions to add something to a criteria
   --
   procedure Add_Model_Table_Name( c : in out d.Criteria; Model_Table_Name : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Model_Table_Name( c : in out d.Criteria; Model_Table_Name : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Cell_Pos( c : in out d.Criteria; Cell_Pos : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Cell_Label( c : in out d.Criteria; Cell_Label : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Cell_Label( c : in out d.Criteria; Cell_Label : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Cell_Type( c : in out d.Criteria; Cell_Type : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Cell_Type( c : in out d.Criteria; Cell_Type : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_File_Pos( c : in out d.Criteria; File_Pos : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Model_Table_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Cell_Pos_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Cell_Label_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Cell_Type_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_File_Pos_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Disaggregated_Data_Table_Cell_Description_IO;
