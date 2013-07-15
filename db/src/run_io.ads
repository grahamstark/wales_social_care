--
-- Created by ada_generator.py on 2012-07-24 19:03:55.668250
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

with WSC_Enums;
with Base_Types;

package Run_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   
   use WSC_Enums;
   use Base_Types;
   
   function Next_Free_Run_Id return integer;

   --
   -- returns true if the primary key parts of Run match the defaults in Wsc_Db_Data.Null_Run
   --
   function Is_Null( Run : Wsc_Db_Data.Run ) return Boolean;
   
   --
   -- returns the single Run matching the primary key fields, or the Wsc_Db_Data.Null_Run record
   -- if no such record exists
   --
   function Retrieve_By_PK( Run_Id : integer; Username : Unbounded_String ) return Wsc_Db_Data.Run;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Run matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Run_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Run retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Run_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Run : Wsc_Db_Data.Run; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Run
   --
   procedure Delete( Run : in out Wsc_Db_Data.Run );
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
   function Retrieve_Associated_Uap_Thresholds( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Uap_Threshold_List.Vector;
   function Retrieve_Associated_Key_Value_Parameters( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Key_Value_Parameter_List.Vector;
   function Retrieve_Associated_Probit_Thresholds( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Probit_Threshold_List.Vector;
   function Retrieve_Associated_Personal_Results( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Personal_Results_List.Vector;
   function Retrieve_Child_State( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.State;
   function Retrieve_Associated_Disaggregated_Data_Tables( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Disaggregated_Data_Table_List.Vector;
   function Retrieve_Associated_Uprate_Assumptions( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Uprate_Assumption_List.Vector;
   function Retrieve_Associated_Personal_Incomes( Run : Wsc_Db_Data.Run ) return Wsc_Db_Data.Personal_Income_List.Vector;

   --
   -- functions to add something to a criteria
   --
   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Username( c : in out d.Criteria; Username : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Username( c : in out d.Criteria; Username : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Comparison_Username( c : in out d.Criteria; Comparison_Username : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Comparison_Username( c : in out d.Criteria; Comparison_Username : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Comparison_Run_Id( c : in out d.Criteria; Comparison_Run_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Title( c : in out d.Criteria; Title : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Title( c : in out d.Criteria; Title : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Use_Random_Threshold( c : in out d.Criteria; Use_Random_Threshold : boolean; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Num_Iterations( c : in out d.Criteria; Num_Iterations : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Interest_Rate_Pct( c : in out d.Criteria; Interest_Rate_Pct : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Real_Terms( c : in out d.Criteria; Real_Terms : boolean; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Is_Null_Settings( c : in out d.Criteria; Is_Null_Settings : boolean; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Working_Root( c : in out d.Criteria; Working_Root : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Working_Root( c : in out d.Criteria; Working_Root : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Users_Directory( c : in out d.Criteria; Users_Directory : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Users_Directory( c : in out d.Criteria; Users_Directory : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Output_Directory( c : in out d.Criteria; Output_Directory : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Output_Directory( c : in out d.Criteria; Output_Directory : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dir_Separator( c : in out d.Criteria; Dir_Separator : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dir_Separator( c : in out d.Criteria; Dir_Separator : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Session_Id( c : in out d.Criteria; Session_Id : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Session_Id( c : in out d.Criteria; Session_Id : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dataset_Name( c : in out d.Criteria; Dataset_Name : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Dataset_Name( c : in out d.Criteria; Dataset_Name : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Default_Run_Dir_Id( c : in out d.Criteria; Default_Run_Dir_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Start_Year( c : in out d.Criteria; Start_Year : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_End_Year( c : in out d.Criteria; End_Year : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Weighting_Function( c : in out d.Criteria; Weighting_Function : run_weighting_function_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Weighting_Lower_Bound( c : in out d.Criteria; Weighting_Lower_Bound : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Weighting_Upper_Bound( c : in out d.Criteria; Weighting_Upper_Bound : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Status( c : in out d.Criteria; Status : run_status_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Type_Of_Run( c : in out d.Criteria; Type_Of_Run : run_type_of_run_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Comparison_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Comparison_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Title_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Use_Random_Threshold_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Num_Iterations_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Interest_Rate_Pct_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Real_Terms_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Is_Null_Settings_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Working_Root_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Users_Directory_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Output_Directory_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dir_Separator_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Session_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Dataset_Name_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Default_Run_Dir_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Start_Year_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_End_Year_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Weighting_Function_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Weighting_Lower_Bound_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Weighting_Upper_Bound_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Status_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Type_Of_Run_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Run_IO;
