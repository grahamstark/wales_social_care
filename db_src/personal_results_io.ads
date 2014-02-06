--
-- Created by ada_generator.py on 2012-02-10 02:09:15.566191
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

with WSC_Enums;
with WSC_Web_Enums;
with Model.WSC.Run_Settings;

package Personal_Results_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   use WSC_Enums;
   use WSC_Web_Enums;
   use Model.WSC.Run_Settings;
   
   -- returns true if the primary key parts of Personal_Results match the defaults in Wsc_Db_Data.Null_Personal_Results
   --
   function Is_Null( Personal_Results : Wsc_Db_Data.Personal_Results ) return Boolean;
   
   --
   -- returns the single Personal_Results matching the primary key fields, or the Wsc_Db_Data.Null_Personal_Results record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String; Run_Id : integer; Sysno : integer; Iteration : integer; Pid : Big_Integer; Wave : Unbounded_String ) return Wsc_Db_Data.Personal_Results;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Personal_Results matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Personal_Results_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Personal_Results retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Personal_Results_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Personal_Results : Wsc_Db_Data.Personal_Results; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Personal_Results
   --
   procedure Delete( Personal_Results : in out Wsc_Db_Data.Personal_Results );
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
   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Sysno( c : in out d.Criteria; Sysno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Iteration( c : in out d.Criteria; Iteration : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Pid( c : in out d.Criteria; Pid : Big_Integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Wave( c : in out d.Criteria; Wave : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Wave( c : in out d.Criteria; Wave : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Hid( c : in out d.Criteria; Hid : Big_Integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Buno( c : in out d.Criteria; Buno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Adno( c : in out d.Criteria; Adno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Passes_Non_Residential_Capital_Test( c : in out d.Criteria; Passes_Non_Residential_Capital_Test: Means_Test_Result; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Passes_Non_Residential_Income_Test( c : in out d.Criteria; Passes_Non_Residential_Income_Test: Means_Test_Result; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Passes_Residential_Capital_Test( c : in out d.Criteria; Passes_Residential_Capital_Test: Means_Test_Result; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Passes_Residential_Income_Test( c : in out d.Criteria; Passes_Residential_Income_Test: Means_Test_Result; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_La_Contributions( c : in out d.Criteria; La_Contributions : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Client_Contributions( c : in out d.Criteria; Client_Contributions : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Gross_Care_Costs( c : in out d.Criteria; Gross_Care_Costs : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Total_Payments_To_Date( c : in out d.Criteria; Total_Payments_To_Date : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_remaining_capital_stock( c : in out d.Criteria; remaining_capital_stock : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Disposable_Income( c : in out d.Criteria; Disposable_Income : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Net_Income( c : in out d.Criteria; Net_Income : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Marginal_Rate( c : in out d.Criteria; Marginal_Rate : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Capital_Contribution( c : in out d.Criteria; Capital_Contribution : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Minimum_Income_Guarantee( c : in out d.Criteria; Minimum_Income_Guarantee : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Passes_Residential_Means_Test( c : in out d.Criteria; Passes_Residential_Means_Test : Means_Test_Result; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Passes_Non_Residential_Means_Test( c : in out d.Criteria; Passes_Non_Residential_Means_Test : Means_Test_Result; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Hours_Of_Care_La( c : in out d.Criteria; Hours_Of_Care_La : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Hours_Of_Care_Private( c : in out d.Criteria; Hours_Of_Care_Private : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Uap( c : in out d.Criteria; Uap : UAP_Level; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Sysno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Iteration_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Pid_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Wave_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Hid_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Buno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Adno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Passes_Non_Residential_Capital_Test_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Passes_Non_Residential_Income_Test_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Passes_Residential_Capital_Test_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Passes_Residential_Income_Test_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_La_Contributions_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Client_Contributions_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Gross_Care_Costs_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Total_Payments_To_Date_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Disposable_Income_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Net_Income_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Marginal_Rate_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Capital_Contribution_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Minimum_Income_Guarantee_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Passes_Residential_Means_Test_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Passes_Non_Residential_Means_Test_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Hours_Of_Care_La_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Hours_Of_Care_Private_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Uap_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Personal_Results_IO;
