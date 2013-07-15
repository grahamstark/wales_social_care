--
-- Created by ada_generator.py on 2011-11-26 19:15:52.866915
-- 

with GNAT.Calendar.Time_IO;

package body Wsc_Db_Mysql_Data is

   use ada.strings.Unbounded;
   package tio renames GNAT.Calendar.Time_IO;

   function To_String( rec : Personal_Income ) return String is
   begin
      return  "Personal_Income: " &
         "Username = " & To_String( rec.Username ) &
         "Run_Id = " & To_String( rec.Run_Id ) &
         "Pid = " & To_String( rec.Pid ) &
         "Wave = " & To_String( rec.Wave ) &
         "Hid = " & To_String( rec.Hid ) &
         "Buno = " & rec.Buno'Img &
         "Adno = " & rec.Adno'Img &
         "Income_Name = " & To_String( rec.Income_Name ) &
         "Value = " & rec.Value'Img;
   end to_String;



   function To_String( rec : Personal_Results ) return String is
   begin
      return  "Personal_Results: " &
         "Username = " & To_String( rec.Username ) &
         "Run_Id = " & To_String( rec.Run_Id ) &
         "Pid = " & To_String( rec.Pid ) &
         "Wave = " & To_String( rec.Wave ) &
         "Hid = " & To_String( rec.Hid ) &
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



        

end Wsc_Db_Mysql_Data;
