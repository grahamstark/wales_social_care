--
-- Created by ada_generator.py on 2012-03-09 09:04:51.180739
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

package Table_Stats_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   
   function Next_Free_Run_Id return integer;
   function Next_Free_Row_Num return integer;
   function Next_Free_Col_Num return integer;
   function Next_Free_System_Number return integer;

   --
   -- returns true if the primary key parts of Table_Stats match the defaults in Wsc_Db_Data.Null_Table_Stats
   --
   function Is_Null( Table_Stats : Wsc_Db_Data.Table_Stats ) return Boolean;
   
   --
   -- returns the single Table_Stats matching the primary key fields, or the Wsc_Db_Data.Null_Table_Stats record
   -- if no such record exists
   --
   function Retrieve_By_PK( Run_Id : integer; Username : Unbounded_String; Model_Table_Name : Unbounded_String; Row_Num : integer; Col_Num : integer; Wave : Unbounded_String; System_Number : integer ) return Wsc_Db_Data.Table_Stats;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Table_Stats matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Table_Stats_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Table_Stats retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Table_Stats_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Table_Stats : Wsc_Db_Data.Table_Stats; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Table_Stats
   --
   procedure Delete( Table_Stats : in out Wsc_Db_Data.Table_Stats );
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
   procedure Add_System_Number( c : in out d.Criteria; System_Number : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Nvalues( c : in out d.Criteria; Nvalues : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmean_1( c : in out d.Criteria; Rmean_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmin_1( c : in out d.Criteria; Rmin_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmax_1( c : in out d.Criteria; Rmax_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmed_1( c : in out d.Criteria; Rmed_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Sddev_1( c : in out d.Criteria; Sddev_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dec1_1( c : in out d.Criteria; Dec1_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dec10_1( c : in out d.Criteria; Dec10_1 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmean_2( c : in out d.Criteria; Rmean_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmin_2( c : in out d.Criteria; Rmin_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmax_2( c : in out d.Criteria; Rmax_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmed_2( c : in out d.Criteria; Rmed_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Sddev_2( c : in out d.Criteria; Sddev_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dec1_2( c : in out d.Criteria; Dec1_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dec10_2( c : in out d.Criteria; Dec10_2 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmean_3( c : in out d.Criteria; Rmean_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmin_3( c : in out d.Criteria; Rmin_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmax_3( c : in out d.Criteria; Rmax_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmed_3( c : in out d.Criteria; Rmed_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Sddev_3( c : in out d.Criteria; Sddev_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dec1_3( c : in out d.Criteria; Dec1_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dec10_3( c : in out d.Criteria; Dec10_3 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmean_4( c : in out d.Criteria; Rmean_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmin_4( c : in out d.Criteria; Rmin_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmax_4( c : in out d.Criteria; Rmax_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmed_4( c : in out d.Criteria; Rmed_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Sddev_4( c : in out d.Criteria; Sddev_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dec1_4( c : in out d.Criteria; Dec1_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dec10_4( c : in out d.Criteria; Dec10_4 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmean_5( c : in out d.Criteria; Rmean_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmin_5( c : in out d.Criteria; Rmin_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmax_5( c : in out d.Criteria; Rmax_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmed_5( c : in out d.Criteria; Rmed_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Sddev_5( c : in out d.Criteria; Sddev_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dec1_5( c : in out d.Criteria; Dec1_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dec10_5( c : in out d.Criteria; Dec10_5 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmean_6( c : in out d.Criteria; Rmean_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmin_6( c : in out d.Criteria; Rmin_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmax_6( c : in out d.Criteria; Rmax_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Rmed_6( c : in out d.Criteria; Rmed_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Sddev_6( c : in out d.Criteria; Sddev_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dec1_6( c : in out d.Criteria; Dec1_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dec10_6( c : in out d.Criteria; Dec10_6 : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Model_Table_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Row_Num_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Col_Num_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Wave_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_System_Number_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Nvalues_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmean_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmin_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmax_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmed_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Sddev_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dec1_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dec10_1_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmean_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmin_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmax_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmed_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Sddev_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dec1_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dec10_2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmean_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmin_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmax_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmed_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Sddev_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dec1_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dec10_3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmean_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmin_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmax_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmed_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Sddev_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dec1_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dec10_4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmean_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmin_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmax_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmed_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Sddev_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dec1_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dec10_5_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmean_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmin_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmax_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Rmed_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Sddev_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dec1_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dec10_6_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Table_Stats_IO;
