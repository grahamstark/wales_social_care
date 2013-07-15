--
-- Created by ada_generator.py on 2012-07-24 19:03:55.534072
-- 

with GNAT.Calendar.Time_IO;

package body Wsc_Db_Data is

   use ada.strings.Unbounded;
   package tio renames GNAT.Calendar.Time_IO;

   function To_String( rec : Person ) return String is
   begin
      return  "Person: " &
         "Dataset_Name = " & To_String( rec.Dataset_Name ) &
         "Iteration = " & rec.Iteration'Img &
         "Wave = " & To_String( rec.Wave ) &
         "Hid = " & rec.Hid'Img &
         "Pid = " & rec.Pid'Img &
         "Buno = " & rec.Buno'Img &
         "Chno = " & rec.Chno'Img &
         "Adno = " & rec.Adno'Img &
         "Pno = " & rec.Pno'Img &
         "Age = " & rec.Age'Img &
         "Sex = " & rec.Sex'Img &
         "Activities_Of_Daily_Living_Score = " & rec.Activities_Of_Daily_Living_Score'Img &
         "Health_Score = " & rec.Health_Score'Img &
         "Years_In_Residential_Care = " & rec.Years_In_Residential_Care'Img &
         "Respondent_Weight_Basic = " & rec.Respondent_Weight_Basic'Img &
         "Respondent_Weight_Extended_1 = " & rec.Respondent_Weight_Extended_1'Img &
         "Respondent_Weight_Extended_2 = " & rec.Respondent_Weight_Extended_2'Img &
         "Enumeration_Weight_Basic = " & rec.Enumeration_Weight_Basic'Img &
         "Enumeration_Weight_Extended_1 = " & rec.Enumeration_Weight_Extended_1'Img &
         "Enumeration_Weight_Extended_2 = " & rec.Enumeration_Weight_Extended_2'Img &
         "Hdsp = " & rec.Hdsp'Img &
         "Has_Full_Sample = " & rec.Has_Full_Sample'Img &
         "Receives_Informal_Care_From_Household_Member = " & rec.Receives_Informal_Care_From_Household_Member'Img &
         "Receives_Informal_Care_From_Non_Householder = " & rec.Receives_Informal_Care_From_Non_Householder'Img &
         "Hours_Of_Care_Recieved = " & rec.Hours_Of_Care_Recieved'Img &
         "Hours_Of_Care_Given = " & rec.Hours_Of_Care_Given'Img &
         "Dies_This_Period = " & rec.Dies_This_Period'Img &
         "Seperates_This_Period = " & rec.Seperates_This_Period'Img &
         "Manage_Stairs_Help = " & rec.Manage_Stairs_Help'Img &
         "Get_Around_House_Help = " & rec.Get_Around_House_Help'Img &
         "Get_In_Or_Out_Of_Bed_Help = " & rec.Get_In_Or_Out_Of_Bed_Help'Img &
         "Cut_Toenails_Help = " & rec.Cut_Toenails_Help'Img &
         "Bathing_Or_Showering_Help = " & rec.Bathing_Or_Showering_Help'Img &
         "Walk_Down_Road_Help = " & rec.Walk_Down_Road_Help'Img &
         "Manage_Stairs_Difficulty = " & rec.Manage_Stairs_Difficulty'Img &
         "Get_Around_House_Difficulty = " & rec.Get_Around_House_Difficulty'Img &
         "Get_In_Or_Out_Of_Bed_Difficulty = " & rec.Get_In_Or_Out_Of_Bed_Difficulty'Img &
         "Cut_Toenails_Difficulty = " & rec.Cut_Toenails_Difficulty'Img &
         "Bathing_Or_Showering_Difficulty = " & rec.Bathing_Or_Showering_Difficulty'Img &
         "Walk_Down_Road_Difficulty = " & rec.Walk_Down_Road_Difficulty'Img &
         "Employment_Status = " & rec.Employment_Status'Img &
         "Usual_Hours_Worked_Per_Week = " & rec.Usual_Hours_Worked_Per_Week'Img &
         "Highest_Qualification = " & rec.Highest_Qualification'Img &
         "Is_Disabled = " & rec.Is_Disabled'Img &
         "Marital_Status = " & rec.Marital_Status'Img &
         "Partner_Status = " & rec.Partner_Status'Img &
         "Health_Status = " & rec.Health_Status'Img &
         "Base_Personal_Wealth = " & rec.Base_Personal_Wealth'Img &
         "Personal_Wealth = " & rec.Personal_Wealth'Img &
         "Disability_Living_Allowance_Mobility_Level = " & rec.Disability_Living_Allowance_Mobility_Level'Img &
         "Disability_Living_Allowance_Care_Level = " & rec.Disability_Living_Allowance_Care_Level'Img &
         "Attendance_Allowance_Level = " & rec.Attendance_Allowance_Level'Img;
   end to_String;



   function To_String( rec : Household_Data ) return String is
   begin
      return  "Household_Data: " &
         "Dataset_Name = " & To_String( rec.Dataset_Name ) &
         "Iteration = " & rec.Iteration'Img &
         "Wave = " & To_String( rec.Wave ) &
         "Interview_Date = " & tio.Image( rec.Interview_Date, tio.ISO_Date ) &
         "Current_Simulated_Date = " & tio.Image( rec.Current_Simulated_Date, tio.ISO_Date ) &
         "Hid = " & rec.Hid'Img &
         "Origin_Hid = " & rec.Origin_Hid'Img &
         "Tenure = " & rec.Tenure'Img &
         "Region = " & rec.Region'Img &
         "Gross_Rent = " & rec.Gross_Rent'Img &
         "Net_Rent = " & rec.Net_Rent'Img &
         "Mortgage_Outstanding = " & rec.Mortgage_Outstanding'Img &
         "Gross_Housing_Costs = " & rec.Gross_Housing_Costs'Img &
         "Net_Housing_Costs = " & rec.Net_Housing_Costs'Img &
         "Total_Income = " & rec.Total_Income'Img &
         "House_Value = " & rec.House_Value'Img &
         "Other_Property_Value = " & rec.Other_Property_Value'Img &
         "Mortgage_Payment = " & rec.Mortgage_Payment'Img &
         "Years_Outstanding_On_Mortgage = " & rec.Years_Outstanding_On_Mortgage'Img &
         "Years_Outstanding_On_Mortgage = " & rec.Years_Outstanding_On_Mortgage'Img &
         "Weight_Basic = " & rec.Weight_Basic'Img &
         "Weight_Extended_1 = " & rec.Weight_Extended_1'Img &
         "Weight_Extended_2 = " & rec.Weight_Extended_2'Img &
         "Ct_Band = " & rec.Ct_Band'Img &
         "Has_Full_Sample = " & rec.Has_Full_Sample'Img;
   end to_String;



   function To_String( rec : Gain_Lose ) return String is
   begin
      return  "Gain_Lose: " &
         "Username = " & To_String( rec.Username ) &
         "Run_Id = " & rec.Run_Id'Img &
         "Iteration = " & rec.Iteration'Img &
         "Hid = " & rec.Hid'Img &
         "Pid = " & rec.Pid'Img &
         "Wave = " & To_String( rec.Wave ) &
         "Pre = " & rec.Pre'Img &
         "Post = " & rec.Post'Img;
   end to_String;



   function To_String( rec : Household_Capital ) return String is
   begin
      return  "Household_Capital: " &
         "Username = " & To_String( rec.Username ) &
         "Run_Id = " & rec.Run_Id'Img &
         "Sysno = " & rec.Sysno'Img &
         "Iteration = " & rec.Iteration'Img &
         "Hid = " & rec.Hid'Img &
         "Wave = " & To_String( rec.Wave ) &
         "Capital_Stock = " & rec.Capital_Stock'Img;
   end to_String;



   function To_String( rec : Table_Stats ) return String is
   begin
      return  "Table_Stats: " &
         "Run_Id = " & rec.Run_Id'Img &
         "Username = " & To_String( rec.Username ) &
         "Model_Table_Name = " & To_String( rec.Model_Table_Name ) &
         "Row_Num = " & rec.Row_Num'Img &
         "Col_Num = " & rec.Col_Num'Img &
         "Wave = " & To_String( rec.Wave ) &
         "System_Number = " & rec.System_Number'Img &
         "Nvalues = " & rec.Nvalues'Img &
         "Rmean_1 = " & rec.Rmean_1'Img &
         "Rmin_1 = " & rec.Rmin_1'Img &
         "Rmax_1 = " & rec.Rmax_1'Img &
         "Rmed_1 = " & rec.Rmed_1'Img &
         "Sddev_1 = " & rec.Sddev_1'Img &
         "Dec1_1 = " & rec.Dec1_1'Img &
         "Dec10_1 = " & rec.Dec10_1'Img &
         "Rmean_2 = " & rec.Rmean_2'Img &
         "Rmin_2 = " & rec.Rmin_2'Img &
         "Rmax_2 = " & rec.Rmax_2'Img &
         "Rmed_2 = " & rec.Rmed_2'Img &
         "Sddev_2 = " & rec.Sddev_2'Img &
         "Dec1_2 = " & rec.Dec1_2'Img &
         "Dec10_2 = " & rec.Dec10_2'Img &
         "Rmean_3 = " & rec.Rmean_3'Img &
         "Rmin_3 = " & rec.Rmin_3'Img &
         "Rmax_3 = " & rec.Rmax_3'Img &
         "Rmed_3 = " & rec.Rmed_3'Img &
         "Sddev_3 = " & rec.Sddev_3'Img &
         "Dec1_3 = " & rec.Dec1_3'Img &
         "Dec10_3 = " & rec.Dec10_3'Img &
         "Rmean_4 = " & rec.Rmean_4'Img &
         "Rmin_4 = " & rec.Rmin_4'Img &
         "Rmax_4 = " & rec.Rmax_4'Img &
         "Rmed_4 = " & rec.Rmed_4'Img &
         "Sddev_4 = " & rec.Sddev_4'Img &
         "Dec1_4 = " & rec.Dec1_4'Img &
         "Dec10_4 = " & rec.Dec10_4'Img &
         "Rmean_5 = " & rec.Rmean_5'Img &
         "Rmin_5 = " & rec.Rmin_5'Img &
         "Rmax_5 = " & rec.Rmax_5'Img &
         "Rmed_5 = " & rec.Rmed_5'Img &
         "Sddev_5 = " & rec.Sddev_5'Img &
         "Dec1_5 = " & rec.Dec1_5'Img &
         "Dec10_5 = " & rec.Dec10_5'Img &
         "Rmean_6 = " & rec.Rmean_6'Img &
         "Rmin_6 = " & rec.Rmin_6'Img &
         "Rmax_6 = " & rec.Rmax_6'Img &
         "Rmed_6 = " & rec.Rmed_6'Img &
         "Sddev_6 = " & rec.Sddev_6'Img &
         "Dec1_6 = " & rec.Dec1_6'Img &
         "Dec10_6 = " & rec.Dec10_6'Img;
   end to_String;



   function To_String( rec : Uap_Threshold ) return String is
   begin
      return  "Uap_Threshold: " &
         "Run_Id = " & rec.Run_Id'Img &
         "Username = " & To_String( rec.Username ) &
         "Sysno = " & rec.Sysno'Img &
         "Iteration = " & rec.Iteration'Img &
         "Wave = " & To_String( rec.Wave ) &
         "Uap_Level = " & rec.Uap_Level'Img &
         "Threshold = " & rec.Threshold'Img;
   end to_String;



   function To_String( rec : Maxima_And_Totals ) return String is
   begin
      return  "Maxima_And_Totals: " &
         "Lifetime_La_Contributions = " & rec.Lifetime_La_Contributions'Img &
         "Lifetime_Client_Contributions = " & rec.Lifetime_Client_Contributions'Img &
         "Lifetime_Gross_Payments = " & rec.Lifetime_Gross_Payments'Img &
         "Lifetime_Capital_Contributions = " & rec.Lifetime_Capital_Contributions'Img &
         "Highest_La_Contribution = " & rec.Highest_La_Contribution'Img;
   end to_String;



   function To_String( rec : Disaggregated_Data_Table_Cell ) return String is
   begin
      return  "Disaggregated_Data_Table_Cell: " &
         "Run_Id = " & rec.Run_Id'Img &
         "Username = " & To_String( rec.Username ) &
         "Model_Table_Name = " & To_String( rec.Model_Table_Name ) &
         "Row_Num = " & rec.Row_Num'Img &
         "Col_Num = " & rec.Col_Num'Img &
         "Wave = " & To_String( rec.Wave ) &
         "Iteration = " & rec.Iteration'Img &
         "System_Number = " & rec.System_Number'Img &
         "Value1 = " & rec.Value1'Img &
         "Value2 = " & rec.Value2'Img &
         "Value3 = " & rec.Value3'Img &
         "Value4 = " & rec.Value4'Img &
         "Value5 = " & rec.Value5'Img &
         "Value6 = " & rec.Value6'Img &
         "I = " & rec.I'Img &
         "P1 = " & rec.P1'Img &
         "P2 = " & rec.P2'Img &
         "P3 = " & rec.P3'Img;
   end to_String;



   function To_String( rec : Disaggregated_Data_Table ) return String is
   begin
      return  "Disaggregated_Data_Table: " &
         "Run_Id = " & rec.Run_Id'Img &
         "Username = " & To_String( rec.Username ) &
         "Model_Table_Name = " & To_String( rec.Model_Table_Name ) &
         "Iteration = " & rec.Iteration'Img;
   end to_String;



   function To_String( rec : Disaggregated_Data_Table_Cell_Description ) return String is
   begin
      return  "Disaggregated_Data_Table_Cell_Description: " &
         "Model_Table_Name = " & To_String( rec.Model_Table_Name ) &
         "Cell_Pos = " & rec.Cell_Pos'Img &
         "Cell_Label = " & To_String( rec.Cell_Label ) &
         "Cell_Type = " & To_String( rec.Cell_Type ) &
         "File_Pos = " & rec.File_Pos'Img;
   end to_String;



   function To_String( rec : Disaggregated_Data_Table_Description ) return String is
   begin
      return  "Disaggregated_Data_Table_Description: " &
         "Model_Table_Name = " & To_String( rec.Model_Table_Name ) &
         "Filename = " & To_String( rec.Filename ) &
         "Has_Totals = " & rec.Has_Totals'Img &
         "Table_Type = " & To_String( rec.Table_Type ) &
         "Table_Subtype = " & To_String( rec.Table_Subtype );
   end to_String;



   function To_String( rec : Personal_Income ) return String is
   begin
      return  "Personal_Income: " &
         "Username = " & To_String( rec.Username ) &
         "Run_Id = " & rec.Run_Id'Img &
         "Pid = " & rec.Pid'Img &
         "Sysno = " & rec.Sysno'Img &
         "Iteration = " & rec.Iteration'Img &
         "Wave = " & To_String( rec.Wave ) &
         "Income_Type = " & rec.Income_Type'Img &
         "Hid = " & rec.Hid'Img &
         "Buno = " & rec.Buno'Img &
         "Adno = " & rec.Adno'Img &
         "Value = " & rec.Value'Img;
   end to_String;



   function To_String( rec : Personal_Results ) return String is
   begin
      return  "Personal_Results: " &
         "Username = " & To_String( rec.Username ) &
         "Run_Id = " & rec.Run_Id'Img &
         "Sysno = " & rec.Sysno'Img &
         "Iteration = " & rec.Iteration'Img &
         "Pid = " & rec.Pid'Img &
         "Wave = " & To_String( rec.Wave ) &
         "Hid = " & rec.Hid'Img &
         "Buno = " & rec.Buno'Img &
         "Adno = " & rec.Adno'Img &
         "Passes_Non_Residential_Capital_Test = " & rec.Passes_Non_Residential_Capital_Test'Img &
         "Passes_Non_Residential_Income_Test = " & rec.Passes_Non_Residential_Income_Test'Img &
         "Passes_Residential_Capital_Test = " & rec.Passes_Residential_Capital_Test'Img &
         "Passes_Residential_Income_Test = " & rec.Passes_Residential_Income_Test'Img &
         "Passes_Residential_Means_Test = " & rec.Passes_Residential_Means_Test'Img &
         "Passes_Non_Residential_Means_Test = " & rec.Passes_Non_Residential_Means_Test'Img &
         "La_Contributions = " & rec.La_Contributions'Img &
         "Client_Contributions = " & rec.Client_Contributions'Img &
         "Gross_Care_Costs = " & rec.Gross_Care_Costs'Img &
         "Total_Payments_To_Date = " & rec.Total_Payments_To_Date'Img &
         "Disposable_Income = " & rec.Disposable_Income'Img &
         "Net_Income = " & rec.Net_Income'Img &
         "Marginal_Rate = " & rec.Marginal_Rate'Img &
         "Capital_Contribution = " & rec.Capital_Contribution'Img &
         "Minimum_Income_Guarantee = " & rec.Minimum_Income_Guarantee'Img &
         "Hours_Of_Care_La = " & rec.Hours_Of_Care_La'Img &
         "Hours_Of_Care_Private = " & rec.Hours_Of_Care_Private'Img &
         "Uap = " & rec.Uap'Img &
         "Remaining_Capital_Stock = " & rec.Remaining_Capital_Stock'Img;
   end to_String;



   function To_String( rec : Probit_Threshold ) return String is
   begin
      return  "Probit_Threshold: " &
         "Username = " & To_String( rec.Username ) &
         "Run_Id = " & rec.Run_Id'Img &
         "Element = " & rec.Element'Img &
         "Threshold = " & rec.Threshold'Img;
   end to_String;



   function To_String( rec : Uprate_Assumption ) return String is
   begin
      return  "Uprate_Assumption: " &
         "Username = " & To_String( rec.Username ) &
         "Run_Id = " & rec.Run_Id'Img &
         "Percent_Change = " & rec.Percent_Change'Img &
         "Use_Obr = " & rec.Use_Obr'Img &
         "Target = " & rec.Target'Img &
         "Element = " & rec.Element'Img;
   end to_String;



   function To_String( rec : Key_Value_Parameter ) return String is
   begin
      return  "Key_Value_Parameter: " &
         "Username = " & To_String( rec.Username ) &
         "Run_Id = " & rec.Run_Id'Img &
         "Key = " & To_String( rec.Key ) &
         "Val = " & To_String( rec.Val );
   end to_String;



   function To_String( rec : State ) return String is
   begin
      return  "State: " &
         "Username = " & To_String( rec.Username ) &
         "Run_Id = " & rec.Run_Id'Img &
         "Household = " & rec.Household'Img &
         "Other_Counter = " & rec.Other_Counter'Img &
         "Year = " & rec.Year'Img &
         "Phase = " & rec.Phase'Img &
         "Health = " & rec.Health'Img &
         "Error_Code = " & rec.Error_Code'Img &
         "Read_Error = " & rec.Read_Error'Img &
         "Session_Id = " & To_String( rec.Session_Id );
   end to_String;



   function To_String( rec : Run ) return String is
   begin
      return  "Run: " &
         "Run_Id = " & rec.Run_Id'Img &
         "Username = " & To_String( rec.Username ) &
         "Comparison_Username = " & To_String( rec.Comparison_Username ) &
         "Comparison_Run_Id = " & rec.Comparison_Run_Id'Img &
         "Title = " & To_String( rec.Title ) &
         "Use_Random_Threshold = " & rec.Use_Random_Threshold'Img &
         "Num_Iterations = " & rec.Num_Iterations'Img &
         "Interest_Rate_Pct = " & rec.Interest_Rate_Pct'Img &
         "Real_Terms = " & rec.Real_Terms'Img &
         "Is_Null_Settings = " & rec.Is_Null_Settings'Img &
         "Working_Root = " & To_String( rec.Working_Root ) &
         "Users_Directory = " & To_String( rec.Users_Directory ) &
         "Output_Directory = " & To_String( rec.Output_Directory ) &
         "Dir_Separator = " & To_String( rec.Dir_Separator ) &
         "Session_Id = " & To_String( rec.Session_Id ) &
         "Dataset_Name = " & To_String( rec.Dataset_Name ) &
         "Default_Run_Dir_Id = " & rec.Default_Run_Dir_Id'Img &
         "Start_Year = " & rec.Start_Year'Img &
         "End_Year = " & rec.End_Year'Img &
         "Weighting_Function = " & rec.Weighting_Function'Img &
         "Weighting_Lower_Bound = " & rec.Weighting_Lower_Bound'Img &
         "Weighting_Upper_Bound = " & rec.Weighting_Upper_Bound'Img &
         "Status = " & rec.Status'Img &
         "Type_Of_Run = " & rec.Type_Of_Run'Img;
   end to_String;



   function To_String( rec : Dataset ) return String is
   begin
      return  "Dataset: " &
         "Name = " & To_String( rec.Name ) &
         "Creator = " & To_String( rec.Creator ) &
         "Title = " & To_String( rec.Title ) &
         "Run_Id = " & rec.Run_Id'Img;
   end to_String;



   function To_String( rec : User_Type ) return String is
   begin
      return  "User_Type: " &
         "Username = " & To_String( rec.Username ) &
         "Password = " & To_String( rec.Password ) &
         "Title = " & To_String( rec.Title ) &
         "Description = " & To_String( rec.Description ) &
         "Email = " & To_String( rec.Email ) &
         "Work_Dir = " & To_String( rec.Work_Dir ) &
         "Utype = " & rec.Utype'Img &
         "Lang = " & rec.Lang'Img &
         "Preferences = " & To_String( rec.Preferences ) &
         "Last_Used = " & tio.Image( rec.Last_Used, tio.ISO_Date );
   end to_String;



        

end Wsc_Db_Data;
