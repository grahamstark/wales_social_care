--
-- Created by ada_generator.py on 2012-02-09 16:45:15.836657
-- 
with Ada.Containers.Vectors;
--
-- FIXME: may not be needed
--
with Ada.Calendar;

with Base_Types; 

with Ada.Strings.Unbounded;

with Model.WSC.Users;
with WSC_Enums;
with WSC_Web_Enums;
with Model.Run_Settings;
with Base_Model_Types;
with Weighting_Commons;
with Model.WSC.Run_Declarations;
with Format_Utils;

package Wsc_Db_Data is

   use Ada.Strings.Unbounded;
   use Base_Types;
   use WSC_Enums;
   use WSC_Web_Enums;
   use Model.Run_Settings;
   use Weighting_Commons;
   
   
   --
   -- record modelling gain_lose : Dummy Gain Lose Table
   --
   type Gain_Lose is record
      Username : Unbounded_String := MISSING_W_KEY;
      Run_Id : integer := MISSING_I_KEY;
      Iteration : integer := MISSING_I_KEY;
      Pid : Big_Integer := MISSING_I_KEY;
      Wave : Unbounded_String := MISSING_W_KEY;
      Hid : Big_Integer := 0;
      Pre : Base_Model_Types.Amount := 0.0;
      Post : Base_Model_Types.Amount := 0.0;
   end record;
   --
   -- container for gain_lose : Dummy Gain Lose Table
   --
   package Gain_Lose_List is new Ada.Containers.Vectors
      (Element_Type => Gain_Lose,
      Index_Type => Positive );
   --
   -- default value for gain_lose : Dummy Gain Lose Table
   --
   Null_Gain_Lose : constant Gain_Lose := (
      Username => MISSING_W_KEY,
      Run_Id => MISSING_I_KEY,
      Iteration => MISSING_I_KEY,
      Pid => MISSING_I_KEY,
      Wave => MISSING_W_KEY,
      Hid => 0,
      Pre => 0.0,
      Post => 0.0
   );
   --
   -- simple print routine for gain_lose : Dummy Gain Lose Table
   --
   function To_String( rec : Gain_Lose ) return String;

   function Gain_Lose_To_Personal_Results( raw : Gain_Lose_List.Vector ) return Personal_Results_Location_List;

   --
   -- record modelling household_capital : remaining household capital
   --
   type Household_Capital is record
      Username : Unbounded_String := MISSING_W_KEY;
      Run_Id : integer := MISSING_I_KEY;
      Sysno : integer := MISSING_I_KEY;
      Iteration : integer := MISSING_I_KEY;
      Hid : Big_Integer := MISSING_I_KEY;
      Wave : Unbounded_String := MISSING_W_KEY;
      Capital_Stock : Base_Model_Types.Amount := 0.0;
   end record;
   --
   -- container for household_capital : remaining household capital
   --
   package Household_Capital_List is new Ada.Containers.Vectors
      (Element_Type => Household_Capital,
      Index_Type => Positive );
   --
   -- default value for household_capital : remaining household capital
   --
   Null_Household_Capital : constant Household_Capital := (
      Username => MISSING_W_KEY,
      Run_Id => MISSING_I_KEY,
      Sysno => MISSING_I_KEY,
      Iteration => MISSING_I_KEY,
      Hid => MISSING_I_KEY,
      Wave => MISSING_W_KEY,
      Capital_Stock => 0.0
   );
   --
   -- simple print routine for household_capital : remaining household capital
   --
   function To_String( rec : Household_Capital ) return String;

  
   subtype Stats_Selection is Positive range 1 .. 6;
         --
      -- record modelling table_stats : Really a view
      --
   type Table_Stats is record
      run_id : Integer := missing_i_key;
      username : Unbounded_String := missing_w_key;
      model_table_name : Unbounded_String := missing_w_key;
      row_num : Integer := missing_i_key;
      col_num : Integer := missing_i_key;
      wave : Unbounded_String := missing_w_key;
      system_number : Integer := missing_i_key;
      nvalues : Integer := 0;
      rmean_1 : Real := 0.0;
      rmin_1 : Real := 0.0;
      rmax_1 : Real := 0.0;
      rmed_1 : Real := 0.0;
      sddev_1 : Real := 0.0;
      dec1_1 : Real := 0.0;
      dec10_1 : Real := 0.0;
      rmean_2 : Real := 0.0;
      rmin_2 : Real := 0.0;
      rmax_2 : Real := 0.0;
      rmed_2 : Real := 0.0;
      sddev_2 : Real := 0.0;
      dec1_2 : Real := 0.0;
      dec10_2 : Real := 0.0;
      rmean_3 : Real := 0.0;
      rmin_3 : Real := 0.0;
      rmax_3 : Real := 0.0;
      rmed_3 : Real := 0.0;
      sddev_3 : Real := 0.0;
      dec1_3 : Real := 0.0;
      dec10_3 : Real := 0.0;
      rmean_4 : Real := 0.0;
      rmin_4 : Real := 0.0;
      rmax_4 : Real := 0.0;
      rmed_4 : Real := 0.0;
      sddev_4 : Real := 0.0;
      dec1_4 : Real := 0.0;
      dec10_4 : Real := 0.0;
      rmean_5 : Real := 0.0;
      rmin_5 : Real := 0.0;
      rmax_5 : Real := 0.0;
      rmed_5 : Real := 0.0;
      sddev_5 : Real := 0.0;
      dec1_5 : Real := 0.0;
      dec10_5 : Real := 0.0;
      rmean_6 : Real := 0.0;
      rmin_6 : Real := 0.0;
      rmax_6 : Real := 0.0;
      rmed_6 : Real := 0.0;
      sddev_6 : Real := 0.0;
      dec1_6 : Real := 0.0;
      dec10_6 : Real := 0.0;
   end record;
   --
   -- container for table_stats : Really a dummy table
   --
   package Table_Stats_List is new Ada.Containers.Vectors
      (Element_Type => Table_Stats,
      Index_Type => Positive );
   --
   -- default value for table_stats : Really a dummy table
   --
   Null_Table_Stats : constant Table_Stats := (
      Run_Id => MISSING_I_KEY,
      Username => MISSING_W_KEY,
      Model_Table_Name => MISSING_W_KEY,
      Row_Num => MISSING_I_KEY,
      Col_Num => MISSING_I_KEY,
      Wave => MISSING_W_KEY,
      System_Number => MISSING_I_KEY,
      Nvalues => 0,
      Rmean_1 => 0.0,
      Rmin_1 => 0.0,
      Rmax_1 => 0.0,
      Rmed_1 => 0.0,
      Sddev_1 => 0.0,
      Dec1_1 => 0.0,
      Dec10_1 => 0.0,
      Rmean_2 => 0.0,
      Rmin_2 => 0.0,
      Rmax_2 => 0.0,
      Rmed_2 => 0.0,
      Sddev_2 => 0.0,
      Dec1_2 => 0.0,
      Dec10_2 => 0.0,
      Rmean_3 => 0.0,
      Rmin_3 => 0.0,
      Rmax_3 => 0.0,
      Rmed_3 => 0.0,
      Sddev_3 => 0.0,
      Dec1_3 => 0.0,
      Dec10_3 => 0.0,
      Rmean_4 => 0.0,
      Rmin_4 => 0.0,
      Rmax_4 => 0.0,
      Rmed_4 => 0.0,
      Sddev_4 => 0.0,
      Dec1_4 => 0.0,
      Dec10_4 => 0.0,
      Rmean_5 => 0.0,
      Rmin_5 => 0.0,
      Rmax_5 => 0.0,
      Rmed_5 => 0.0,
      Sddev_5 => 0.0,
      Dec1_5 => 0.0,
      Dec10_5 => 0.0,
      Rmean_6 => 0.0,
      Rmin_6 => 0.0,
      Rmax_6 => 0.0,
      Rmed_6 => 0.0,
      Sddev_6 => 0.0,
      Dec1_6 => 0.0,
      Dec10_6 => 0.0
   );
   --
   -- simple print routine for table_stats : Really a dummy table
   --
   function To_String( rec : Table_Stats ) return String;

   --
   -- record modelling maxima_and_totals : Really a dummy table
   --
   type Maxima_And_Totals is record
      Lifetime_La_Contributions : Real := 0.0;
      Lifetime_Client_Contributions : Real := 0.0;
      Lifetime_Gross_Payments : Real := 0.0;
      Lifetime_Capital_Contributions : Real := 0.0;
      Highest_La_Contribution : Real := 0.0;
   end record;
   --
   -- container for maxima_and_totals : Really a dummy table
   --
   package Maxima_And_Totals_List is new Ada.Containers.Vectors
      (Element_Type => Maxima_And_Totals,
      Index_Type => Positive );
   --
   -- default value for maxima_and_totals : Really a dummy table
   --
   Null_Maxima_And_Totals : constant Maxima_And_Totals := (
      Lifetime_La_Contributions => 0.0,
      Lifetime_Client_Contributions => 0.0,
      Lifetime_Gross_Payments => 0.0,
      Lifetime_Capital_Contributions => 0.0,
      Highest_La_Contribution => 0.0
   );
   --
   -- simple print routine for maxima_and_totals : Really a dummy table
   --
   function To_String( rec : Maxima_And_Totals ) return String;
   --
   -- record modelling disaggregated_data_table_cell : 
   --
   type Disaggregated_Data_Table_Cell is record
      Run_Id : integer := MISSING_I_KEY;
      Username : Unbounded_String := MISSING_W_KEY;
      Model_Table_Name : Unbounded_String := MISSING_W_KEY;
      Row_Num : integer := MISSING_I_KEY;
      Col_Num : integer := MISSING_I_KEY;
      Wave : Unbounded_String := MISSING_W_KEY;
      Iteration : integer := MISSING_I_KEY;
      System_Number : integer := MISSING_I_KEY;
      Value1 : Real := 0.0;
      Value2 : Real := 0.0;
      Value3 : Real := 0.0;
      Value4 : Real := 0.0;
      Value5 : Real := 0.0;
      Value6 : Real := 0.0;
      I : Big_Integer := 0;
      P1 : integer := 0;
      P2 : integer := 0;
      P3 : integer := 0;
   end record;
   --
   -- container for disaggregated_data_table_cell : 
   --
   package Disaggregated_Data_Table_Cell_List is new Ada.Containers.Vectors
      (Element_Type => Disaggregated_Data_Table_Cell,
      Index_Type => Positive );
   --
   -- default value for disaggregated_data_table_cell : 
   --
   Null_Disaggregated_Data_Table_Cell : constant Disaggregated_Data_Table_Cell := (
      Run_Id => MISSING_I_KEY,
      Username => MISSING_W_KEY,
      Model_Table_Name => MISSING_W_KEY,
      Row_Num => MISSING_I_KEY,
      Col_Num => MISSING_I_KEY,
      Wave => MISSING_W_KEY,
      Iteration => MISSING_I_KEY,
      System_Number => MISSING_I_KEY,
      Value1 => 0.0,
      Value2 => 0.0,
      Value3 => 0.0,
      Value4 => 0.0,
      Value5 => 0.0,
      Value6 => 0.0,
      I => 0,
      P1 => 0,
      P2 => 0,
      P3 => 0
   );
   --
   -- simple print routine for disaggregated_data_table_cell : 
   --
   function To_String( rec : Disaggregated_Data_Table_Cell ) return String;

   --
   -- record modelling disaggregated_data_table : 
   --
   type Disaggregated_Data_Table is record
      Run_Id : integer := MISSING_I_KEY;
      Username : Unbounded_String := MISSING_W_KEY;
      Model_Table_Name : Unbounded_String := MISSING_W_KEY;
      Iteration : integer := MISSING_I_KEY;
      Disaggregated_Data_Table_Cells : Disaggregated_Data_Table_Cell_List.Vector;
   end record;
   --
   -- container for disaggregated_data_table : 
   --
   package Disaggregated_Data_Table_List is new Ada.Containers.Vectors
      (Element_Type => Disaggregated_Data_Table,
      Index_Type => Positive );
   --
   -- default value for disaggregated_data_table : 
   --
   Null_Disaggregated_Data_Table : constant Disaggregated_Data_Table := (
      Run_Id => MISSING_I_KEY,
      Username => MISSING_W_KEY,
      Model_Table_Name => MISSING_W_KEY,
      Iteration => MISSING_I_KEY,
      Disaggregated_Data_Table_Cells => Disaggregated_Data_Table_Cell_List.Empty_Vector
   );
   --
   -- simple print routine for disaggregated_data_table : 
   --
   function To_String( rec : Disaggregated_Data_Table ) return String;

   --
   -- record modelling disaggregated_data_table_cell_description : 
   --
   type Disaggregated_Data_Table_Cell_Description is record
      Model_Table_Name : Unbounded_String := MISSING_W_KEY;
      Cell_Pos : integer := MISSING_I_KEY;
      Cell_Label : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      Cell_Type : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      Cell_Subtype : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      
      File_Pos : integer := 0;
      Disaggregated_Data_Table_Cells : Disaggregated_Data_Table_Cell_List.Vector;
   end record;
   --
   -- container for disaggregated_data_table_cell_description : 
   --
   package Disaggregated_Data_Table_Cell_Description_List is new Ada.Containers.Vectors
      (Element_Type => Disaggregated_Data_Table_Cell_Description,
      Index_Type => Positive );
   --
   -- default value for disaggregated_data_table_cell_description : 
   --
   Null_Disaggregated_Data_Table_Cell_Description : constant Disaggregated_Data_Table_Cell_Description := (
      Model_Table_Name => MISSING_W_KEY,
      Cell_Pos => MISSING_I_KEY,
      Cell_Label => Ada.Strings.Unbounded.Null_Unbounded_String,
      Cell_Type => Ada.Strings.Unbounded.Null_Unbounded_String,
      Cell_Subtype => Ada.Strings.Unbounded.Null_Unbounded_String,
      File_Pos => 0,
      Disaggregated_Data_Table_Cells => Disaggregated_Data_Table_Cell_List.Empty_Vector
   );
   --
   -- simple print routine for disaggregated_data_table_cell_description : 
   --
   function To_String( rec : Disaggregated_Data_Table_Cell_Description ) return String;

   --
   -- record modelling disaggregated_data_table_description : 
   --
   type Disaggregated_Data_Table_Description is record
      Model_Table_Name : Unbounded_String := MISSING_W_KEY;
      Filename : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      Has_Totals : integer := 0;
      Table_Type : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      table_subtype : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      
      Disaggregated_Data_Table_Cell_Descriptions : Disaggregated_Data_Table_Cell_Description_List.Vector;
      Disaggregated_Data_Tables : Disaggregated_Data_Table_List.Vector;
   end record;
   --
   -- container for disaggregated_data_table_description : 
   --
   package Disaggregated_Data_Table_Description_List is new Ada.Containers.Vectors
      (Element_Type => Disaggregated_Data_Table_Description,
      Index_Type => Positive );
   --
   -- default value for disaggregated_data_table_description : 
   --
   Null_Disaggregated_Data_Table_Description : constant Disaggregated_Data_Table_Description := (
      Model_Table_Name => MISSING_W_KEY,
      Filename => Ada.Strings.Unbounded.Null_Unbounded_String,
      Has_Totals => 0,
      Table_Type => Ada.Strings.Unbounded.Null_Unbounded_String,
      table_subtype => Ada.Strings.Unbounded.Null_Unbounded_String,
      Disaggregated_Data_Table_Cell_Descriptions => Disaggregated_Data_Table_Cell_Description_List.Empty_Vector,
      Disaggregated_Data_Tables => Disaggregated_Data_Table_List.Empty_Vector
   );
   --
   -- simple print routine for disaggregated_data_table_description : 
   --
   function To_String( rec : Disaggregated_Data_Table_Description ) return String;
   --
   -- record modelling personal_income : Normalised Income List
   --
   type Personal_Income is record
      Username : Unbounded_String := MISSING_W_KEY;
      Run_Id : integer := MISSING_I_KEY;
      Pid : Big_Integer := MISSING_I_KEY;
      Sysno : integer := MISSING_I_KEY;
      Iteration : integer := MISSING_I_KEY;
      Wave : Unbounded_String := MISSING_W_KEY;
      Hid : Big_Integer := MISSING_I_KEY;
      Buno : integer := 0;
      Adno : integer := 0;
      Income_Type : Incomes_Type := gross_wage;
      Value : Real := 0.0;
   end record;
   --
   -- container for personal_income : Normalised Income List
   --
   package Personal_Income_List is new Ada.Containers.Vectors
      (Element_Type => Personal_Income,
      Index_Type => Positive );
   --
   -- default value for personal_income : Normalised Income List
   --
   Null_Personal_Income : constant Personal_Income := (
      Username => MISSING_W_KEY,
      Run_Id => MISSING_I_KEY,
      Pid => MISSING_I_KEY,
      Sysno => MISSING_I_KEY,
      Iteration => MISSING_I_KEY,
      Wave => MISSING_W_KEY,
      Hid => MISSING_I_KEY,
      Buno => 0,
      Adno => 0,
      Income_Type => gross_wage,
      Value => 0.0
   );
   --
   -- simple print routine for personal_income : Normalised Income List
   --
   function To_String( rec : Personal_Income ) return String;

   --
   -- record modelling personal_results : personal results
   --
   type Personal_Results is record
      Username : Unbounded_String := MISSING_W_KEY;
      Run_Id : integer := MISSING_I_KEY;
      Sysno : integer := MISSING_I_KEY;
      Iteration : integer := MISSING_I_KEY;
      Pid : Big_Integer := MISSING_I_KEY;
      Wave : Unbounded_String := MISSING_W_KEY;
      Hid : Big_Integer := MISSING_I_KEY;
      Buno : integer := 0;
      Adno : integer := 0;
      Passes_Non_Residential_Capital_Test: Means_Test_Result := not_applicable;
      Passes_Non_Residential_Income_Test: Means_Test_Result := not_applicable;
      Passes_Residential_Capital_Test: Means_Test_Result := not_applicable;
      Passes_Residential_Income_Test: Means_Test_Result := not_applicable;
      Passes_Residential_Means_Test : Means_Test_Result := not_applicable;
      Passes_Non_Residential_Means_Test : Means_Test_Result := not_applicable;
      La_Contributions : Real := 0.0;
      Client_Contributions : Real := 0.0;
      Gross_Care_Costs : Real := 0.0;
      Total_Payments_To_Date : Real := 0.0;
      remaining_capital_stock : Real := 0.0;
      Disposable_Income : Real := 0.0;
      Net_Income : Real := 0.0;
      Marginal_Rate : Real := 0.0;
      Capital_Contribution : Real := 0.0;
      Minimum_Income_Guarantee : Real := 0.0;
      Hours_Of_Care_La : Real := 0.0;
      Hours_Of_Care_Private : Real := 0.0;
      Uap : UAP_Level := critical;
   end record;

   type Results_Field_Name is (
         passes_non_residential_capital_test,
         passes_non_residential_income_test,
         passes_residential_capital_test,
         passes_residential_income_test,
         passes_residential_means_test,
         passes_non_residential_means_test,
         la_contributions,
         client_contributions,
         gross_care_costs,
         total_payments_to_date,        
         disposable_income,
         net_income,
         marginal_rate,
         capital_contribution,
         minimum_income_guarantee,
         hours_of_care_la,
         hours_of_care_private,
         uap,
         remaining_capital_stock
      );
   
   function To_String( which : Results_Field_Name ) return String;   
   
   --
   -- container for personal_results : personal results
   --
   package Personal_Results_List is new Ada.Containers.Vectors
      (Element_Type => Personal_Results,
      Index_Type => Positive );
   --
   -- default value for personal_results : personal results
   --
   Null_Personal_Results : constant Personal_Results := (
      Username => MISSING_W_KEY,
      Run_Id => MISSING_I_KEY,
      Sysno => MISSING_I_KEY,
      Iteration => MISSING_I_KEY,
      Pid => MISSING_I_KEY,
      Wave => MISSING_W_KEY,
      Hid => MISSING_I_KEY,
      Buno => 0,
      Adno => 0,
      Passes_Non_Residential_Capital_Test => not_applicable,
      Passes_Non_Residential_Income_Test => not_applicable,
      Passes_Residential_Capital_Test => not_applicable,
      Passes_Residential_Income_Test => not_applicable,
      La_Contributions => 0.0,
      Client_Contributions => 0.0,
      Gross_Care_Costs => 0.0,
      Total_Payments_To_Date => 0.0,
      remaining_capital_stock => 0.0,
      Disposable_Income => 0.0,
      Net_Income => 0.0,
      Marginal_Rate => 0.0,
      Capital_Contribution => 0.0,
      Minimum_Income_Guarantee => 0.0,
      Passes_Residential_Means_Test => not_applicable,
      Passes_Non_Residential_Means_Test => not_applicable,
      Hours_Of_Care_La => 0.0,
      Hours_Of_Care_Private => 0.0,
      Uap => critical
   );
   --
   -- simple print routine for personal_results : personal results
   --
   function To_String( rec : Personal_Results ) return String;

   type Personal_Index is record
      Username  : Unbounded_String := MISSING_W_KEY;
      Run_Id    : integer := MISSING_I_KEY;
      Sysno     : integer := MISSING_I_KEY;
      Iteration : integer := MISSING_I_KEY;
      Pid       : Big_Integer := MISSING_I_KEY;
      Wave      : Unbounded_String := MISSING_W_KEY;
      Hid       : Big_Integer := MISSING_I_KEY;
   end record;
   
   --
   -- container for personal_Index : personal results
   --
   package Personal_Index_List_Package is new Ada.Containers.Vectors
      (Element_Type => Personal_Index,
      Index_Type => Positive );
   subtype Personal_Index_List is Personal_Index_List_Package.Vector; 
   --
   -- default value for personal_Index : personal results
   --
   Null_Personal_Index : constant Personal_Index := (
      Username => MISSING_W_KEY,
      Run_Id => MISSING_I_KEY,
      Sysno => MISSING_I_KEY,
      Iteration => MISSING_I_KEY,
      Pid => MISSING_I_KEY,
      Wave => MISSING_W_KEY,
      Hid => MISSING_I_KEY
   );
   
   function Index_To_Personal_Results( raw : Personal_Index_List ) return Personal_Results_Location_List;
   
   --
   -- record modelling probit_threshold : 
   --
   type Probit_Threshold is record
      Username : Unbounded_String := MISSING_W_KEY;
      Run_Id : integer := MISSING_I_KEY;
      Element : Probit_Threshold_Type := Probit_Threshold_Type'First;
      Threshold : Real := 0.50;
   end record;
   --
   -- container for probit_threshold : 
   --
   package Probit_Threshold_List is new Ada.Containers.Vectors
      (Element_Type => Probit_Threshold,
      Index_Type => Positive );
   --
   -- default value for probit_threshold : 
   --
   Null_Probit_Threshold : constant Probit_Threshold := (
      Username => MISSING_W_KEY,
      Run_Id => MISSING_I_KEY,
      Element => Probit_Threshold_Type'First,
      Threshold => 0.0
   );
   --
   -- simple print routine for probit_threshold : 
   --
   function To_String( rec : Probit_Threshold ) return String;

   --
   -- record modelling uprate_assumption : 
   --
   type Uprate_Assumption is record
      Username : Unbounded_String := MISSING_W_KEY;
      Run_Id : integer := MISSING_I_KEY;
      Percent_Change : Real := 0.0;
      Use_Obr : boolean := false;
      Target : Uprate_Targets := upr_housing_costs;
      Element : Forecast_Element := obr_earnings;
   end record;
   --
   -- container for uprate_assumption : 
   --
   package Uprate_Assumption_List is new Ada.Containers.Vectors
      (Element_Type => Uprate_Assumption,
      Index_Type => Positive );
   --
   -- default value for uprate_assumption : 
   --
   Null_Uprate_Assumption : constant Uprate_Assumption := (
      Username => MISSING_W_KEY,
      Run_Id => MISSING_I_KEY,
      Percent_Change => 0.0,
      Use_Obr => false,
      Target => upr_housing_costs,
      Element => obr_earnings
   );

   --
   -- record modelling key_value_parameter : 
   --
   type Key_Value_Parameter is record
      Username : Unbounded_String := MISSING_W_KEY;
      Run_Id : integer := MISSING_I_KEY;
      Key : Unbounded_String := MISSING_W_KEY;
      Val : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   end record;
   --
   -- container for key_value_parameter : 
   --
   package Key_Value_Parameter_List is new Ada.Containers.Vectors
      (Element_Type => Key_Value_Parameter,
      Index_Type => Positive );
   --
   -- default value for key_value_parameter : 
   --
   Null_Key_Value_Parameter : constant Key_Value_Parameter := (
      Username => MISSING_W_KEY,
      Run_Id => MISSING_I_KEY,
      Key => MISSING_W_KEY,
      Val => Ada.Strings.Unbounded.Null_Unbounded_String
   );
   --
   -- simple print routine for key_value_parameter : 
   --
   function To_String( rec : Key_Value_Parameter ) return String;
   --
   -- simple print routine for uprate_assumption : 
   --
   function To_String( rec : Uprate_Assumption ) return String;

   --
   -- default value for User_Type : user
   --
   -- type State is record
      -- Username : Unbounded_String := MISSING_W_KEY;
      -- Run_Id : integer := MISSING_I_KEY;
      -- Household : integer := 0;
      -- Other_Counter : integer := 0;
      -- Year : integer := 0;
      -- Phase : Phase_Type := not_started;
      -- Health : Health_Type := normal;
      -- Error_Code : integer := 0;
      -- Read_Error : boolean := false;
   -- end record;
   --
   -- container for state : 
   --
   
   subtype State is Model.Run_Settings.State_Type;
   package State_List is new Ada.Containers.Vectors
      (Element_Type => State,
      Index_Type => Positive );
   subtype States is State_List.Vector;
   --
   -- default value for state : 
   --
   function To_String( rec : State ) return String;
   Null_State : State renames Model.Run_Settings.BLANK_STATE_TYPE;
   --
   -- record modelling run : 
   --
      -- --
      -- -- record modelling run : 
      -- --
      -- type Run is record
         -- Run_Id : integer := MISSING_I_KEY;
         -- Username : Unbounded_String := MISSING_W_KEY;
         -- Comparison_Username : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         -- Comparison_Run_Id : integer := 0;
         -- Title : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         -- Use_Random_Threshold : boolean := false;
         -- Num_Iterations : integer := 1;
         -- Interest_Rate_Pct : Real := 0.0;
         -- Real_Terms : boolean := false;
         -- Is_Null_Settings : integer := 0;
         -- Working_Root : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         -- Users_Directory : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         -- Output_Directory : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         -- Dir_Separator : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         -- Session_Id : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         -- Dataset_Dir_Id : integer := 0;
         -- Default_Run_Dir_Id : integer := 0;
         -- Start_Year : integer := 0;
         -- End_Year : integer := 0;
         -- Weighting_Function : Distance_Function_Type := chi_square;
         -- Weighting_Lower_Bound : Real := 0.0;
         -- Weighting_Upper_Bound : Real := 0.0;
         -- Probit_Thresholds : Probit_Threshold_List.Vector;
         -- Personal_Results : Personal_Results_List.Vector;
         -- State_Child :State := Null_State;
         -- Disaggregated_Data_Tables : Disaggregated_Data_Table_List.Vector;
         -- Uprate_Assumptions : Uprate_Assumption_List.Vector;
         -- Personal_Incomes : Personal_Income_List.Vector;
      -- end record;
      -- 
      -- subtype Run is Model.WSC.Run_Declarations.Run;
      
      -- package Run_List renames Model.WSC.Run_Declarations.Run_List;
      -- Null_Run : Run renames Model.WSC.Run_Declarations.NULL_RUN;
      --
      -- container for run : 
      --
      --
      -- default value for run : 
      --
      -- Null_Run : constant Run := (
         -- Run_Id => MISSING_I_KEY,
         -- Username => MISSING_W_KEY,
         -- Comparison_Username => Ada.Strings.Unbounded.Null_Unbounded_String,
         -- Comparison_Run_Id => 0,
         -- Title => Ada.Strings.Unbounded.Null_Unbounded_String,
         -- Use_Random_Threshold => false,
         -- Num_Iterations => 1,
         -- Interest_Rate_Pct => 0.0,
         -- Real_Terms => false,
         -- Is_Null_Settings => 0,
         -- Working_Root => Ada.Strings.Unbounded.Null_Unbounded_String,
         -- Users_Directory => Ada.Strings.Unbounded.Null_Unbounded_String,
         -- Output_Directory => Ada.Strings.Unbounded.Null_Unbounded_String,
         -- Dir_Separator => Ada.Strings.Unbounded.Null_Unbounded_String,
         -- Session_Id => Ada.Strings.Unbounded.Null_Unbounded_String,
         -- Dataset_Dir_Id => 0,
         -- Default_Run_Dir_Id => 0,
         -- Start_Year => 0,
         -- End_Year => 0,
         -- Weighting_Function => chi_square,
         -- Weighting_Lower_Bound => 0.0,
         -- Weighting_Upper_Bound => 0.0,
         -- Probit_Thresholds => Probit_Threshold_List.Empty_Vector,
         -- Personal_Results => Personal_Results_List.Empty_Vector,
         -- State_Child => Null_State,
         -- Disaggregated_Data_Tables => Disaggregated_Data_Table_List.Empty_Vector,
         -- Uprate_Assumptions => Uprate_Assumption_List.Empty_Vector,
         -- Personal_Incomes => Personal_Income_List.Empty_Vector
   -- );   
   
   --
   -- record modelling User_Type : user
   --
   subtype User_Type is Model.WSC.Users.User_Type;

   -- 
   -- type User_Type is record
      -- Username : Unbounded_String := MISSING_W_KEY;
      -- Password : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      -- Title : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      -- Description : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      -- Email : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      -- Work_Dir : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      -- Utype : User_Class := anon;
      -- Lang : Languages := en;
      -- Preferences : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      -- Last_Used : Ada.Calendar.Time := FIRST_DATE;
      -- Runs : Run_List.Vector;
   -- end record;
   -- --
   -- container for User_Type : user
   --
   use type User_Type;
   package User_Type_List is new Ada.Containers.Vectors
      (Element_Type => User_Type,
      Index_Type => Positive );
   --
   -- default value for User_Type : user
   --
   Null_User_Type : User_Type renames Model.WSC.Users.INVALID_USER;
   --
   -- simple print routine for User_Type : user
   --
   function To_String( rec : User_Type ) return String;
        

end Wsc_Db_Data;
