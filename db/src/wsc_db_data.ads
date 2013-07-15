--
-- Created by ada_generator.py on 2012-07-24 19:03:55.529594
-- 
with Ada.Containers.Vectors;
--
-- FIXME: may not be needed
--
with Ada.Calendar;

with Base_Types; use Base_Types;

with Ada.Strings.Unbounded;
with WSC_Enums;
with Base_Types;

package Wsc_Db_Data is

   use Ada.Strings.Unbounded;
   
   use WSC_Enums;
   use Base_Types;

      --
      -- record modelling person : 
      --
      type Person is record
         Dataset_Name : Unbounded_String := MISSING_W_KEY;
         Iteration : integer := MISSING_I_KEY;
         Wave : Unbounded_String := MISSING_W_KEY;
         Hid : Sernum_Value := MISSING_I_KEY;
         Pid : Sernum_Value := MISSING_I_KEY;
         Buno : Benefit_Unit_Number := 0;
         Chno : Child_Number := 0;
         Adno : Adult_Number := 0;
         Pno : Person_Number := 0;
         Age : Age_Range := 0;
         Sex : Gender_Type := Gender_Type'First;
         Activities_Of_Daily_Living_Score : Amount := 0.0;
         Health_Score : Amount := 0.0;
         Years_In_Residential_Care : Integer := 0;
         Respondent_Weight_Basic : Amount := 0.0;
         Respondent_Weight_Extended_1 : Amount := 0.0;
         Respondent_Weight_Extended_2 : Amount := 0.0;
         Enumeration_Weight_Basic : Amount := 0.0;
         Enumeration_Weight_Extended_1 : Amount := 0.0;
         Enumeration_Weight_Extended_2 : Amount := 0.0;
         Hdsp : Head_Or_Spouse := Head_Or_Spouse'First;
         Has_Full_Sample : Boolean := 0;
         Receives_Informal_Care_From_Household_Member : Boolean := false;
         Receives_Informal_Care_From_Non_Householder : Boolean := false;
         Hours_Of_Care_Recieved : Hours_Count := 0;
         Hours_Of_Care_Given : Hours_Count := 0;
         Dies_This_Period : Boolean := false;
         Seperates_This_Period : Boolean := false;
         Manage_Stairs_Help : Help_Needed_Type := Help_Needed_Type'First;
         Get_Around_House_Help : Help_Needed_Type := Help_Needed_Type'First;
         Get_In_Or_Out_Of_Bed_Help : Help_Needed_Type := Help_Needed_Type'First;
         Cut_Toenails_Help : Help_Needed_Type := Help_Needed_Type'First;
         Bathing_Or_Showering_Help : Help_Needed_Type := Help_Needed_Type'First;
         Walk_Down_Road_Help : Help_Needed_Type := Help_Needed_Type'First;
         Manage_Stairs_Difficulty : Difficulty_Type := Difficulty_Type'First;
         Get_Around_House_Difficulty : Difficulty_Type := Difficulty_Type'First;
         Get_In_Or_Out_Of_Bed_Difficulty : Difficulty_Type := Difficulty_Type'First;
         Cut_Toenails_Difficulty : Difficulty_Type := Difficulty_Type'First;
         Bathing_Or_Showering_Difficulty : Difficulty_Type := Difficulty_Type'First;
         Walk_Down_Road_Difficulty : Difficulty_Type := Difficulty_Type'First;
         Employment_Status : Employment_Status_Type := Employment_Status_Type'First;
         Usual_Hours_Worked_Per_Week : Hours_Count := 0;
         Highest_Qualification : Qualification_Type := Qualification_Type'First;
         Is_Disabled : Boolean := false;
         Marital_Status : Marital_Status_Type := Marital_Status_Type'First;
         Partner_Status : Partner_Status_Type := Partner_Status_Type'First;
         Health_Status : Health_Status_Type := Health_Status_Type'First;
         Base_Personal_Wealth : Amount := 0.0;
         Personal_Wealth : Amount := 0.0;
         Disability_Living_Allowance_Mobility_Level : High_Low_Nil_Type := High_Low_Nil_Type'First;
         Disability_Living_Allowance_Care_Level : High_Middle_Low_Nil_Type := High_Middle_Low_Nil_Type'First;
         Attendance_Allowance_Level : High_Low_Nil_Type := High_Low_Nil_Type'First;
      end record;
      --
      -- container for person : 
      --
      package Person_List is new Ada.Containers.Vectors
         (Element_Type => Person,
         Index_Type => Positive );
      --
      -- default value for person : 
      --
      Null_Person : constant Person := (
         Dataset_Name => MISSING_W_KEY,
         Iteration => MISSING_I_KEY,
         Wave => MISSING_W_KEY,
         Hid => MISSING_I_KEY,
         Pid => MISSING_I_KEY,
         Buno => 0,
         Chno => 0,
         Adno => 0,
         Pno => 0,
         Age => 0,
         Sex => Gender_Type'First,
         Activities_Of_Daily_Living_Score => 0.0,
         Health_Score => 0.0,
         Years_In_Residential_Care => 0,
         Respondent_Weight_Basic => 0.0,
         Respondent_Weight_Extended_1 => 0.0,
         Respondent_Weight_Extended_2 => 0.0,
         Enumeration_Weight_Basic => 0.0,
         Enumeration_Weight_Extended_1 => 0.0,
         Enumeration_Weight_Extended_2 => 0.0,
         Hdsp => Head_Or_Spouse'First,
         Has_Full_Sample => 0,
         Receives_Informal_Care_From_Household_Member => false,
         Receives_Informal_Care_From_Non_Householder => false,
         Hours_Of_Care_Recieved => 0,
         Hours_Of_Care_Given => 0,
         Dies_This_Period => false,
         Seperates_This_Period => false,
         Manage_Stairs_Help => Help_Needed_Type'First,
         Get_Around_House_Help => Help_Needed_Type'First,
         Get_In_Or_Out_Of_Bed_Help => Help_Needed_Type'First,
         Cut_Toenails_Help => Help_Needed_Type'First,
         Bathing_Or_Showering_Help => Help_Needed_Type'First,
         Walk_Down_Road_Help => Help_Needed_Type'First,
         Manage_Stairs_Difficulty => Difficulty_Type'First,
         Get_Around_House_Difficulty => Difficulty_Type'First,
         Get_In_Or_Out_Of_Bed_Difficulty => Difficulty_Type'First,
         Cut_Toenails_Difficulty => Difficulty_Type'First,
         Bathing_Or_Showering_Difficulty => Difficulty_Type'First,
         Walk_Down_Road_Difficulty => Difficulty_Type'First,
         Employment_Status => Employment_Status_Type'First,
         Usual_Hours_Worked_Per_Week => 0,
         Highest_Qualification => Qualification_Type'First,
         Is_Disabled => false,
         Marital_Status => Marital_Status_Type'First,
         Partner_Status => Partner_Status_Type'First,
         Health_Status => Health_Status_Type'First,
         Base_Personal_Wealth => 0.0,
         Personal_Wealth => 0.0,
         Disability_Living_Allowance_Mobility_Level => High_Low_Nil_Type'First,
         Disability_Living_Allowance_Care_Level => High_Middle_Low_Nil_Type'First,
         Attendance_Allowance_Level => High_Low_Nil_Type'First
      );
      --
      -- simple print routine for person : 
      --
      function To_String( rec : Person ) return String;

      --
      -- record modelling household_data : 
      --
      type Household_Data is record
         Dataset_Name : Unbounded_String := MISSING_W_KEY;
         Iteration : integer := MISSING_I_KEY;
         Wave : Unbounded_String := MISSING_W_KEY;
         Interview_Date : Ada.Calendar.Time := FIRST_DATE;
         Current_Simulated_Date : Ada.Calendar.Time := FIRST_DATE;
         Hid : Sernum_Value := MISSING_I_KEY;
         Origin_Hid : Sernum_Value := MISSING_I_KEY;
         Tenure : Tenure_Type := Tenure_Type'First;
         Region : Region_Type := Region_Type'First;
         Gross_Rent : Amount := 0.0;
         Net_Rent : Amount := 0.0;
         Mortgage_Outstanding : Amount := 0.0;
         Gross_Housing_Costs : Amount := 0.0;
         Net_Housing_Costs : Amount := 0.0;
         Total_Income : Amount := 0.0;
         House_Value : Amount := 0.0;
         Other_Property_Value : Amount := 0.0;
         Mortgage_Payment : Amount := 0.0;
         Years_Outstanding_On_Mortgage : Natural := 0;
         Years_Outstanding_On_Mortgage : Natural := 0;
         Weight_Basic : Amount := 0.0;
         Weight_Extended_1 : Amount := 0.0;
         Weight_Extended_2 : Amount := 0.0;
         Ct_Band : Council_Tax_Band := Council_Tax_Band'First;
         Has_Full_Sample : Boolean := 0;
         Persons : Person_List.Vector;
      end record;
      --
      -- container for household_data : 
      --
      package Household_Data_List is new Ada.Containers.Vectors
         (Element_Type => Household_Data,
         Index_Type => Positive );
      --
      -- default value for household_data : 
      --
      Null_Household_Data : constant Household_Data := (
         Dataset_Name => MISSING_W_KEY,
         Iteration => MISSING_I_KEY,
         Wave => MISSING_W_KEY,
         Interview_Date => FIRST_DATE,
         Current_Simulated_Date => FIRST_DATE,
         Hid => MISSING_I_KEY,
         Origin_Hid => MISSING_I_KEY,
         Tenure => Tenure_Type'First,
         Region => Region_Type'First,
         Gross_Rent => 0.0,
         Net_Rent => 0.0,
         Mortgage_Outstanding => 0.0,
         Gross_Housing_Costs => 0.0,
         Net_Housing_Costs => 0.0,
         Total_Income => 0.0,
         House_Value => 0.0,
         Other_Property_Value => 0.0,
         Mortgage_Payment => 0.0,
         Years_Outstanding_On_Mortgage => 0,
         Years_Outstanding_On_Mortgage => 0,
         Weight_Basic => 0.0,
         Weight_Extended_1 => 0.0,
         Weight_Extended_2 => 0.0,
         Ct_Band => Council_Tax_Band'First,
         Has_Full_Sample => 0,
         Persons => Person_List.Empty_Vector
      );
      --
      -- simple print routine for household_data : 
      --
      function To_String( rec : Household_Data ) return String;

      --
      -- record modelling gain_lose : Dummy Gain Lose Table
      --
      type Gain_Lose is record
         Username : Unbounded_String := MISSING_W_KEY;
         Run_Id : integer := MISSING_I_KEY;
         Iteration : integer := MISSING_I_KEY;
         Hid : Big_Integer := 0;
         Pid : Big_Integer := MISSING_I_KEY;
         Wave : Unbounded_String := MISSING_W_KEY;
         Pre : Amount := 0.0;
         Post : Amount := 0.0;
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
         Hid => 0,
         Pid => MISSING_I_KEY,
         Wave => MISSING_W_KEY,
         Pre => 0.0,
         Post => 0.0
      );
      --
      -- simple print routine for gain_lose : Dummy Gain Lose Table
      --
      function To_String( rec : Gain_Lose ) return String;

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
         Capital_Stock : Amount := 0.0;
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

      --
      -- record modelling table_stats : Really a dummy table
      --
      type Table_Stats is record
         Run_Id : integer := MISSING_I_KEY;
         Username : Unbounded_String := MISSING_W_KEY;
         Model_Table_Name : Unbounded_String := MISSING_W_KEY;
         Row_Num : integer := MISSING_I_KEY;
         Col_Num : integer := MISSING_I_KEY;
         Wave : Unbounded_String := MISSING_W_KEY;
         System_Number : integer := MISSING_I_KEY;
         Nvalues : integer := 0;
         Rmean_1 : Real := 0.0;
         Rmin_1 : Real := 0.0;
         Rmax_1 : Real := 0.0;
         Rmed_1 : Real := 0.0;
         Sddev_1 : Real := 0.0;
         Dec1_1 : Real := 0.0;
         Dec10_1 : Real := 0.0;
         Rmean_2 : Real := 0.0;
         Rmin_2 : Real := 0.0;
         Rmax_2 : Real := 0.0;
         Rmed_2 : Real := 0.0;
         Sddev_2 : Real := 0.0;
         Dec1_2 : Real := 0.0;
         Dec10_2 : Real := 0.0;
         Rmean_3 : Real := 0.0;
         Rmin_3 : Real := 0.0;
         Rmax_3 : Real := 0.0;
         Rmed_3 : Real := 0.0;
         Sddev_3 : Real := 0.0;
         Dec1_3 : Real := 0.0;
         Dec10_3 : Real := 0.0;
         Rmean_4 : Real := 0.0;
         Rmin_4 : Real := 0.0;
         Rmax_4 : Real := 0.0;
         Rmed_4 : Real := 0.0;
         Sddev_4 : Real := 0.0;
         Dec1_4 : Real := 0.0;
         Dec10_4 : Real := 0.0;
         Rmean_5 : Real := 0.0;
         Rmin_5 : Real := 0.0;
         Rmax_5 : Real := 0.0;
         Rmed_5 : Real := 0.0;
         Sddev_5 : Real := 0.0;
         Dec1_5 : Real := 0.0;
         Dec10_5 : Real := 0.0;
         Rmean_6 : Real := 0.0;
         Rmin_6 : Real := 0.0;
         Rmax_6 : Real := 0.0;
         Rmed_6 : Real := 0.0;
         Sddev_6 : Real := 0.0;
         Dec1_6 : Real := 0.0;
         Dec10_6 : Real := 0.0;
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
      -- record modelling uap_threshold : 
      --
      type Uap_Threshold is record
         Run_Id : integer := MISSING_I_KEY;
         Username : Unbounded_String := MISSING_W_KEY;
         Sysno : integer := MISSING_I_KEY;
         Iteration : integer := MISSING_I_KEY;
         Wave : Unbounded_String := MISSING_W_KEY;
         Uap_Level : uap_threshold_uap_level_Enum := critical;
         Threshold : Real := 0.0;
      end record;
      --
      -- container for uap_threshold : 
      --
      package Uap_Threshold_List is new Ada.Containers.Vectors
         (Element_Type => Uap_Threshold,
         Index_Type => Positive );
      --
      -- default value for uap_threshold : 
      --
      Null_Uap_Threshold : constant Uap_Threshold := (
         Run_Id => MISSING_I_KEY,
         Username => MISSING_W_KEY,
         Sysno => MISSING_I_KEY,
         Iteration => MISSING_I_KEY,
         Wave => MISSING_W_KEY,
         Uap_Level => critical,
         Threshold => 0.0
      );
      --
      -- simple print routine for uap_threshold : 
      --
      function To_String( rec : Uap_Threshold ) return String;

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
         File_Pos : integer := 0;
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
         File_Pos => 0
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
         Table_Subtype : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
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
         Table_Subtype => Ada.Strings.Unbounded.Null_Unbounded_String,
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
         Income_Type : personal_income_income_type_Enum := gross_wage;
         Hid : Big_Integer := 0;
         Buno : integer := 0;
         Adno : integer := 0;
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
         Income_Type => gross_wage,
         Hid => 0,
         Buno => 0,
         Adno => 0,
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
         Hid : Big_Integer := 0;
         Buno : integer := 0;
         Adno : integer := 0;
         Passes_Non_Residential_Capital_Test : personal_results_passes_non_residential_capital_test_Enum := not_applicable;
         Passes_Non_Residential_Income_Test : personal_results_passes_non_residential_income_test_Enum := not_applicable;
         Passes_Residential_Capital_Test : personal_results_passes_residential_capital_test_Enum := not_applicable;
         Passes_Residential_Income_Test : personal_results_passes_residential_income_test_Enum := not_applicable;
         Passes_Residential_Means_Test : personal_results_passes_residential_means_test_Enum := not_applicable;
         Passes_Non_Residential_Means_Test : personal_results_passes_non_residential_means_test_Enum := not_applicable;
         La_Contributions : Real := 0.0;
         Client_Contributions : Real := 0.0;
         Gross_Care_Costs : Real := 0.0;
         Total_Payments_To_Date : Real := 0.0;
         Disposable_Income : Real := 0.0;
         Net_Income : Real := 0.0;
         Marginal_Rate : Real := 0.0;
         Capital_Contribution : Real := 0.0;
         Minimum_Income_Guarantee : Real := 0.0;
         Hours_Of_Care_La : Real := 0.0;
         Hours_Of_Care_Private : Real := 0.0;
         Uap : personal_results_uap_Enum := critical;
         Remaining_Capital_Stock : Amount := 0.0;
      end record;
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
         Hid => 0,
         Buno => 0,
         Adno => 0,
         Passes_Non_Residential_Capital_Test => not_applicable,
         Passes_Non_Residential_Income_Test => not_applicable,
         Passes_Residential_Capital_Test => not_applicable,
         Passes_Residential_Income_Test => not_applicable,
         Passes_Residential_Means_Test => not_applicable,
         Passes_Non_Residential_Means_Test => not_applicable,
         La_Contributions => 0.0,
         Client_Contributions => 0.0,
         Gross_Care_Costs => 0.0,
         Total_Payments_To_Date => 0.0,
         Disposable_Income => 0.0,
         Net_Income => 0.0,
         Marginal_Rate => 0.0,
         Capital_Contribution => 0.0,
         Minimum_Income_Guarantee => 0.0,
         Hours_Of_Care_La => 0.0,
         Hours_Of_Care_Private => 0.0,
         Uap => critical,
         Remaining_Capital_Stock => 0.0
      );
      --
      -- simple print routine for personal_results : personal results
      --
      function To_String( rec : Personal_Results ) return String;

      --
      -- record modelling probit_threshold : 
      --
      type Probit_Threshold is record
         Username : Unbounded_String := MISSING_W_KEY;
         Run_Id : integer := MISSING_I_KEY;
         Element : probit_threshold_element_Enum := dies_this_period;
         Threshold : Real := 0.0;
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
         Element => dies_this_period,
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
         Target : uprate_assumption_target_Enum := upr_housing_costs;
         Element : uprate_assumption_element_Enum := obr_earnings;
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
      -- simple print routine for uprate_assumption : 
      --
      function To_String( rec : Uprate_Assumption ) return String;

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
      -- record modelling state : 
      --
      type State is record
         Username : Unbounded_String := MISSING_W_KEY;
         Run_Id : integer := MISSING_I_KEY;
         Household : integer := 0;
         Other_Counter : integer := 0;
         Year : integer := 0;
         Phase : state_phase_Enum := not_started;
         Health : state_health_Enum := normal;
         Error_Code : integer := 0;
         Read_Error : boolean := false;
         Session_Id : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      end record;
      --
      -- container for state : 
      --
      package State_List is new Ada.Containers.Vectors
         (Element_Type => State,
         Index_Type => Positive );
      --
      -- default value for state : 
      --
      Null_State : constant State := (
         Username => MISSING_W_KEY,
         Run_Id => MISSING_I_KEY,
         Household => 0,
         Other_Counter => 0,
         Year => 0,
         Phase => not_started,
         Health => normal,
         Error_Code => 0,
         Read_Error => false,
         Session_Id => Ada.Strings.Unbounded.Null_Unbounded_String
      );
      --
      -- simple print routine for state : 
      --
      function To_String( rec : State ) return String;

      --
      -- record modelling run : 
      --
      type Run is record
         Run_Id : integer := MISSING_I_KEY;
         Username : Unbounded_String := MISSING_W_KEY;
         Comparison_Username : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Comparison_Run_Id : integer := 0;
         Title : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Use_Random_Threshold : boolean := false;
         Num_Iterations : integer := 1;
         Interest_Rate_Pct : Real := 0.0;
         Real_Terms : boolean := false;
         Is_Null_Settings : boolean := false;
         Working_Root : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Users_Directory : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Output_Directory : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Dir_Separator : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Session_Id : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Dataset_Name : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Default_Run_Dir_Id : integer := 0;
         Start_Year : integer := 0;
         End_Year : integer := 0;
         Weighting_Function : run_weighting_function_Enum := chi_square;
         Weighting_Lower_Bound : Real := 0.0;
         Weighting_Upper_Bound : Real := 0.0;
         Status : run_status_Enum := edited;
         Type_Of_Run : run_type_of_run_Enum := simulation;
         Uap_Thresholds : Uap_Threshold_List.Vector;
         Key_Value_Parameters : Key_Value_Parameter_List.Vector;
         Probit_Thresholds : Probit_Threshold_List.Vector;
         Personal_Results : Personal_Results_List.Vector;
         State_Child :State := Null_State;
         Disaggregated_Data_Tables : Disaggregated_Data_Table_List.Vector;
         Uprate_Assumptions : Uprate_Assumption_List.Vector;
         Personal_Incomes : Personal_Income_List.Vector;
      end record;
      --
      -- container for run : 
      --
      package Run_List is new Ada.Containers.Vectors
         (Element_Type => Run,
         Index_Type => Positive );
      --
      -- default value for run : 
      --
      Null_Run : constant Run := (
         Run_Id => MISSING_I_KEY,
         Username => MISSING_W_KEY,
         Comparison_Username => Ada.Strings.Unbounded.Null_Unbounded_String,
         Comparison_Run_Id => 0,
         Title => Ada.Strings.Unbounded.Null_Unbounded_String,
         Use_Random_Threshold => false,
         Num_Iterations => 1,
         Interest_Rate_Pct => 0.0,
         Real_Terms => false,
         Is_Null_Settings => false,
         Working_Root => Ada.Strings.Unbounded.Null_Unbounded_String,
         Users_Directory => Ada.Strings.Unbounded.Null_Unbounded_String,
         Output_Directory => Ada.Strings.Unbounded.Null_Unbounded_String,
         Dir_Separator => Ada.Strings.Unbounded.Null_Unbounded_String,
         Session_Id => Ada.Strings.Unbounded.Null_Unbounded_String,
         Dataset_Name => Ada.Strings.Unbounded.Null_Unbounded_String,
         Default_Run_Dir_Id => 0,
         Start_Year => 0,
         End_Year => 0,
         Weighting_Function => chi_square,
         Weighting_Lower_Bound => 0.0,
         Weighting_Upper_Bound => 0.0,
         Status => edited,
         Type_Of_Run => simulation,
         Uap_Thresholds => Uap_Threshold_List.Empty_Vector,
         Key_Value_Parameters => Key_Value_Parameter_List.Empty_Vector,
         Probit_Thresholds => Probit_Threshold_List.Empty_Vector,
         Personal_Results => Personal_Results_List.Empty_Vector,
         State_Child => Null_State,
         Disaggregated_Data_Tables => Disaggregated_Data_Table_List.Empty_Vector,
         Uprate_Assumptions => Uprate_Assumption_List.Empty_Vector,
         Personal_Incomes => Personal_Income_List.Empty_Vector
      );
      --
      -- simple print routine for run : 
      --
      function To_String( rec : Run ) return String;

      --
      -- record modelling dataset : 
      --
      type Dataset is record
         Name : Unbounded_String := MISSING_W_KEY;
         Creator : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Title : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Run_Id : integer := 0;
         Household_Datas : Household_Data_List.Vector;
         Persons : Person_List.Vector;
         Runs : Run_List.Vector;
      end record;
      --
      -- container for dataset : 
      --
      package Dataset_List is new Ada.Containers.Vectors
         (Element_Type => Dataset,
         Index_Type => Positive );
      --
      -- default value for dataset : 
      --
      Null_Dataset : constant Dataset := (
         Name => MISSING_W_KEY,
         Creator => Ada.Strings.Unbounded.Null_Unbounded_String,
         Title => Ada.Strings.Unbounded.Null_Unbounded_String,
         Run_Id => 0,
         Household_Datas => Household_Data_List.Empty_Vector,
         Persons => Person_List.Empty_Vector,
         Runs => Run_List.Empty_Vector
      );
      --
      -- simple print routine for dataset : 
      --
      function To_String( rec : Dataset ) return String;

      --
      -- record modelling user_type : user
      --
      type User_Type is record
         Username : Unbounded_String := MISSING_W_KEY;
         Password : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Title : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Description : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Email : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Work_Dir : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Utype : user_type_utype_Enum := anon;
         Lang : user_type_lang_Enum := en;
         Preferences : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Last_Used : Ada.Calendar.Time := FIRST_DATE;
         Runs : Run_List.Vector;
         Datasets : Dataset_List.Vector;
      end record;
      --
      -- container for user_type : user
      --
      package User_Type_List is new Ada.Containers.Vectors
         (Element_Type => User_Type,
         Index_Type => Positive );
      --
      -- default value for user_type : user
      --
      Null_User_Type : constant User_Type := (
         Username => MISSING_W_KEY,
         Password => Ada.Strings.Unbounded.Null_Unbounded_String,
         Title => Ada.Strings.Unbounded.Null_Unbounded_String,
         Description => Ada.Strings.Unbounded.Null_Unbounded_String,
         Email => Ada.Strings.Unbounded.Null_Unbounded_String,
         Work_Dir => Ada.Strings.Unbounded.Null_Unbounded_String,
         Utype => anon,
         Lang => en,
         Preferences => Ada.Strings.Unbounded.Null_Unbounded_String,
         Last_Used => FIRST_DATE,
         Runs => Run_List.Empty_Vector,
         Datasets => Dataset_List.Empty_Vector
      );
      --
      -- simple print routine for user_type : user
      --
      function To_String( rec : User_Type ) return String;

        

end Wsc_Db_Data;
