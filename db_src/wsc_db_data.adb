--
-- Created by ada_generator.py on 2012-02-10 12:33:06.717097
-- 
with Text_Utils;

with GNAT.Calendar.Time_IO;
with GNATColl.Traces;

package body Wsc_Db_Data is

   use Text_Utils;
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "WSC_DB_DATA" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
   function To_String( which : Results_Field_Name ) return String is
      s  : constant String := Results_Field_Name'Image( which );
      sl : constant Natural := s'Length;
   begin
      return s( 1 .. sl );
   end To_String;
   
   function To_String( rec : Gain_Lose ) return String is
   begin
      return  "Gain_Lose: " &
         "Username = " & To_String( rec.Username ) &
         "Run_Id = " & rec.Run_Id'Img &
         "Iteration = " & rec.Iteration'Img &
         "Pid = " & rec.Pid'Img &
         "Wave = " & To_String( rec.Wave ) &
         "Hid = " & rec.Hid'Img &
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
   
   function Index_To_Personal_Results( raw : Personal_Index_List ) return Personal_Results_Location_List is
      use  Personal_Index_List_Package;
      l : Personal_Results_Location_List;
      
      procedure Convert( c : Cursor ) is
         pl : Personal_Results_Location;
         pi : Personal_Index := Element( c );
      begin
         pl.pid := SERNUM_Value( pi.pid );
         pl.hid := SERNUM_Value( pi.hid );
         pl.wave := Waves'Value( TS( pi.wave ));
         pl.iteration := pi.iteration;
         pl.sysno := pi.sysno;
         l.append( pl );
      end Convert;
      
   begin
      raw.Iterate( Convert'Access );
      return l;   
   end Index_To_Personal_Results;
   
   function Gain_Lose_To_Personal_Results( raw : Gain_Lose_List.Vector ) return Personal_Results_Location_List is
      use  Gain_Lose_List;
      l : Personal_Results_Location_List;
      
      procedure Convert( c : Cursor ) is
         pl : Personal_Results_Location;
         pi : Gain_Lose := Element( c );
      begin
         pl.pid := SERNUM_Value( pi.pid );
         pl.hid := SERNUM_Value( pi.hid );
         pl.wave := Waves'Value( TS( pi.wave ));
         pl.iteration := pi.iteration;
         l.append( pl );
      end Convert;
      
   begin
      raw.Iterate( Convert'Access );
      return l;   
   end  Gain_Lose_To_Personal_Results;


   
   use ada.strings.Unbounded;
   package tio renames GNAT.Calendar.Time_IO;
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
         "Table_Type = " & To_String( rec.Table_Type );
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
         "Hid = " & rec.Hid'Img &
         "Buno = " & rec.Buno'Img &
         "Adno = " & rec.Adno'Img &
         "Income_Type = " & rec.Income_Type'Img &
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
         "La_Contributions = " & rec.La_Contributions'Img &
         "Client_Contributions = " & rec.Client_Contributions'Img &
         "Gross_Care_Costs = " & rec.Gross_Care_Costs'Img &
         "Total_Payments_To_Date = " & rec.Total_Payments_To_Date'Img &
         "Disposable_Income = " & rec.Disposable_Income'Img &
         "Net_Income = " & rec.Net_Income'Img &
         "Marginal_Rate = " & rec.Marginal_Rate'Img &
         "Capital_Contribution = " & rec.Capital_Contribution'Img &
         "Minimum_Income_Guarantee = " & rec.Minimum_Income_Guarantee'Img &
         "Passes_Residential_Means_Test = " & rec.Passes_Residential_Means_Test'Img &
         "Passes_Non_Residential_Means_Test = " & rec.Passes_Non_Residential_Means_Test'Img &
         "Hours_Of_Care_La = " & rec.Hours_Of_Care_La'Img &
         "Hours_Of_Care_Private = " & rec.Hours_Of_Care_Private'Img &
         "Uap = " & rec.Uap'Img;
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
         "Read_Error = " & rec.Read_Error'Img;
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

   function To_String( rec : Key_Value_Parameter ) return String is
   begin
      return  "Key_Value_Parameter: " &
         "Username = " & To_String( rec.Username ) &
         "Run_Id = " & rec.Run_Id'Img &
         "Key = " & To_String( rec.Key ) &
         "Val = " & To_String( rec.Val );
   end to_String;

end Wsc_Db_Data;
