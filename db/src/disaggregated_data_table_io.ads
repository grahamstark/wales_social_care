--
-- Created by ada_generator.py on 2012-07-24 19:03:55.775337
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

with WSC_Enums;
with Base_Types;

package Disaggregated_Data_Table_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   
   use WSC_Enums;
   use Base_Types;
   
   function Next_Free_Run_Id return integer;
   function Next_Free_Iteration return integer;

   --
   -- returns true if the primary key parts of Disaggregated_Data_Table match the defaults in Wsc_Db_Data.Null_Disaggregated_Data_Table
   --
   function Is_Null( Disaggregated_Data_Table : Wsc_Db_Data.Disaggregated_Data_Table ) return Boolean;
   
   --
   -- returns the single Disaggregated_Data_Table matching the primary key fields, or the Wsc_Db_Data.Null_Disaggregated_Data_Table record
   -- if no such record exists
   --
   function Retrieve_By_PK( Run_Id : integer; Username : Unbounded_String; Model_Table_Name : Unbounded_String; Iteration : integer ) return Wsc_Db_Data.Disaggregated_Data_Table;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Disaggregated_Data_Table_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Disaggregated_Data_Table_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Disaggregated_Data_Table : Wsc_Db_Data.Disaggregated_Data_Table; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Disaggregated_Data_Table
   --
   procedure Delete( Disaggregated_Data_Table : in out Wsc_Db_Data.Disaggregated_Data_Table );
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
   function Retrieve_Associated_Disaggregated_Data_Table_Cells( Disaggregated_Data_Table : Wsc_Db_Data.Disaggregated_Data_Table ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.Vector;

   --
   -- functions to add something to a criteria
   --
   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Username( c : in out d.Criteria; Username : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Username( c : in out d.Criteria; Username : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Model_Table_Name( c : in out d.Criteria; Model_Table_Name : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Model_Table_Name( c : in out d.Criteria; Model_Table_Name : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Iteration( c : in out d.Criteria; Iteration : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Model_Table_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Iteration_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Disaggregated_Data_Table_IO;
