--
-- Created by ada_generator.py on 2011-11-26 19:15:52.862436
-- 
with Ada.Containers.Vectors;
--
-- FIXME: may not be needed
--
with Ada.Calendar;

with base_types; use base_types;

with Ada.Strings.Unbounded;

package Wsc_Db_Mysql_Data is

   use Ada.Strings.Unbounded;

      --
      -- record modelling personal_income : Normalised Income List
      --
      type Personal_Income is record
         Username : Unbounded_String := MISSING_W_KEY;
         Run_Id : Unbounded_String := MISSING_W_KEY;
         Pid : Unbounded_String := MISSING_W_KEY;
         Wave : Unbounded_String := MISSING_W_KEY;
         Hid : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Buno : integer := 0;
         Adno : integer := 0;
         Income_Name : Unbounded_String := MISSING_W_KEY;
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
         Run_Id => MISSING_W_KEY,
         Pid => MISSING_W_KEY,
         Wave => MISSING_W_KEY,
         Hid => Ada.Strings.Unbounded.Null_Unbounded_String,
         Buno => 0,
         Adno => 0,
         Income_Name => MISSING_W_KEY,
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
         Run_Id : Unbounded_String := MISSING_W_KEY;
         Pid : Unbounded_String := MISSING_W_KEY;
         Wave : Unbounded_String := MISSING_W_KEY;
         Hid : Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         Buno : integer := 0;
         Adno : integer := 0;
         Passes_Non_Residential_Capital_Test : integer := 0;
         Passes_Non_Residential_Income_Test : integer := 0;
         Passes_Residential_Capital_Test : integer := 0;
         Passes_Residential_Income_Test : integer := 0;
         La_Contributions : Real := 0.0;
         Client_Contributions : Real := 0.0;
         Gross_Care_Costs : Real := 0.0;
         Total_Payments_To_Date : Real := 0.0;
         Disposable_Income : Real := 0.0;
         Net_Income : Real := 0.0;
         Marginal_Rate : Real := 0.0;
         Capital_Contribution : Real := 0.0;
         Minimum_Income_Guarantee : Real := 0.0;
         Passes_Residential_Means_Test : integer := 0;
         Passes_Non_Residential_Means_Test : integer := 0;
         Hours_Of_Care_La : Real := 0.0;
         Hours_Of_Care_Private : Real := 0.0;
         Uap : integer := 0;
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
         Run_Id => MISSING_W_KEY,
         Pid => MISSING_W_KEY,
         Wave => MISSING_W_KEY,
         Hid => Ada.Strings.Unbounded.Null_Unbounded_String,
         Buno => 0,
         Adno => 0,
         Passes_Non_Residential_Capital_Test => 0,
         Passes_Non_Residential_Income_Test => 0,
         Passes_Residential_Capital_Test => 0,
         Passes_Residential_Income_Test => 0,
         La_Contributions => 0.0,
         Client_Contributions => 0.0,
         Gross_Care_Costs => 0.0,
         Total_Payments_To_Date => 0.0,
         Disposable_Income => 0.0,
         Net_Income => 0.0,
         Marginal_Rate => 0.0,
         Capital_Contribution => 0.0,
         Minimum_Income_Guarantee => 0.0,
         Passes_Residential_Means_Test => 0,
         Passes_Non_Residential_Means_Test => 0,
         Hours_Of_Care_La => 0.0,
         Hours_Of_Care_Private => 0.0,
         Uap => 0
      );
      --
      -- simple print routine for personal_results : personal results
      --
      function To_String( rec : Personal_Results ) return String;

        

end Wsc_Db_Mysql_Data;
