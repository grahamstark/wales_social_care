--
-- Created by ada_generator.py on 2012-07-24 19:03:55.780796
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

with WSC_Enums;
with Base_Types;

package Disaggregated_Data_Table_Cell_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   
   use WSC_Enums;
   use Base_Types;
   
   function Next_Free_Run_Id return integer;
   function Next_Free_Row_Num return integer;
   function Next_Free_Col_Num return integer;
   function Next_Free_Iteration return integer;
   function Next_Free_System_Number return integer;

   --
   -- returns true if the primary key parts of Disaggregated_Data_Table_Cell match the defaults in Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell
   --
   function Is_Null( Disaggregated_Data_Table_Cell : Wsc_Db_Data.Disaggregated_Data_Table_Cell ) return Boolean;
   
   --
   -- returns the single Disaggregated_Data_Table_Cell matching the primary key fields, or the Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell record
   -- if no such record exists
   --
   function Retrieve_By_PK( Run_Id : integer; Username : Unbounded_String; Model_Table_Name : Unbounded_String; Row_Num : integer; Col_Num : integer; Wave : Unbounded_String; Iteration : integer; System_Number : integer ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table_Cell matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Disaggregated_Data_Table_Cell retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Disaggregated_Data_Table_Cell : Wsc_Db_Data.Disaggregated_Data_Table_Cell; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Disaggregated_Data_Table_Cell
   --
   procedure Delete( Disaggregated_Data_Table_Cell : in out Wsc_Db_Data.Disaggregated_Data_Table_Cell );
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
   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Username( c : in out d.Criteria; Username : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Username( c : in out d.Criteria; Username : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Model_Table_Name( c : in out d.Criteria; Model_Table_Name : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Model_Table_Name( c : in out d.Criteria; Model_Table_Name : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Row_Num( c : in out d.Criteria; Row_Num : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Col_Num( c : in out d.Criteria; Col_Num : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Wave( c : in out d.Criteria; Wave : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Wave( c : in out d.Criteria; Wave : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Iteration( c : in out d.Criteria; Iteration : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_System_Number( c : in out d.Criteria; System_Number : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Value1( c : in out d.Criteria; Value1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Value2( c : in out d.Criteria; Value2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Value3( c : in out d.Criteria; Value3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Value4( c : in out d.Criteria; Value4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Value5( c : in out d.Criteria; Value5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Value6( c : in out d.Criteria; Value6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_I( c : in out d.Criteria; I : Big_Integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_P1( c : in out d.Criteria; P1 : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_P2( c : in out d.Criteria; P2 : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_P3( c : in out d.Criteria; P3 : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Model_Table_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Row_Num_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Col_Num_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Wave_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Iteration_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_System_Number_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Value1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Value2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Value3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Value4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Value5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Value6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_I_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_P1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_P2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_P3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Disaggregated_Data_Table_Cell_IO;
