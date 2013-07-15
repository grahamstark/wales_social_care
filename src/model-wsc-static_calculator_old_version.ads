with Model.WSC.Run_Declarations;
with Model.WSC.Parameters;
with Model.WSC.Output;
with Model.Run_Settings;
with Model.WSC.Household;
with Model.WSC.Results;
with Model.WSC.Results.IO;
with Model.WSC.Household.Database;
with WSC_Enums;

package Model.WSC.Static_Calculator is

   use Model.Run_Settings;
   use Model.WSC.Run_Declarations;
   use Model.WSC.Output;
   use Model.WSC.Parameters;
   use Model.WSC.Household;
   use Model.WSC.Results;
   use Model.WSC.Results.IO;
   use Model.WSC.Household.Database;
   use WSC_Enums;
  

   type Make_Needs_Score_Access is access function( ad : Person ) return Amount;

   function ADL_Score( ad : Person ) return Amount;
   function Rec_Care_Probit( ad : Person ) return Amount;
      
   procedure Calculate_One_Household_With_No_History(
      wave           : Waves;
      hh             : Model.WSC.Household.Household;
      params         : Parameters_Rec;
      results        : out Household_Result;
      db             : DB_Type;
      wsc_run : Run;
      uap_thresholds : UAP_Array;
      randoms        : in out M_Randoms.Random_List );
      
   procedure Calculate_One_Household(
      wave           : Waves;
      hh             : Model.WSC.Household.Household;
      params         : Parameters_Rec;
      results        : out Household_Result;
      wsc_run        : Run;
      uap_thresholds : UAP_Array;
      randoms        : in out M_Randoms.Random_List );
   
private
   
   procedure Calculate_Residential(
      hh       : Model.WSC.Household.Household;
      params   : Parameters_Rec;
      ad       : Person;
      prs      : in out Personal_Result;
      wsc_run : Run;
      randoms  : in out M_Randoms.Random_List );
   
   procedure Calculate_Non_Residential(
      hh             : Model.WSC.Household.Household;
      params         : Parameters_Rec;
      ad             : Person;
      prs            : in out Personal_Result;
      uap_thresholds : UAP_Array;
      wsc_run : Run;
      randoms        : in out M_Randoms.Random_List );
      
   procedure Preserve_For_Existing_Claimants( 
      sys        : Parameters_Rec; 
      res        : in out Household_Result );
   
   procedure Calculate_Attendance_Allowance( 
      pers            : Person;  
      aa              : out Amount;
      aa_last_period  : Amount;
      sys             : Attendance_Allowance_System;
      wsc_run : Run;
      randoms         : in out M_Randoms.Random_List );
   
   procedure Calculate_DLA( 
      pers            : Person; 
      care            : out Amount;
      mobility        : out Amount;
      dla_last_period : Amount;
      sys             : Disability_Living_Allowance_System;      
      wsc_run : Run;
      randoms         : in out M_Randoms.Random_List  );
      
   procedure Calculate_Guaranteed_Pension_Credit( 
      bu       : Benefit_Unit;  
      res      : in out Benefit_Unit_Result;
      gpcsys   : Guaranteed_Credit_System;
      pensys   : Pension_System;
      wsc_run : Run );

   procedure Calculate_Savings_Credit( 
      bu  : Benefit_Unit;  
      res : in out Benefit_Unit_Result;
      sys : Savings_Credit_System;
      wsc_run : Run );
 
    procedure Calculate_State_Pension( 
      bu       : Benefit_Unit;  
      res      : in out Benefit_Unit_Result;
      sys      : Pension_System;
      wsc_run : Run );
      
   procedure Get_Non_Residential_Care_Hours_Offer( 
      sys        : Social_Care_Needs_System; 
      num_adults_in_bu : Person_Count;
      hours      : out Amount; 
      prs        : in out Personal_Result;
      ad         : Person;
      thresholds : UAP_Array;
      rand_list  : in out M_Randoms.Random_List );
 
   procedure Apply_Income_Means_Test(
      ad             : Person;
      num_adults     : Person_Count;
      hdata          : Household_Data;
      test           : Income_Means_Test_System;
      is_sys         : Guaranteed_Credit_System;
      res            : in out Personal_Result;
      result_state   : out Means_Test_Result );
      
   procedure Apply_Asset_Means_Test( 
      ad                           : Person;
      net_housing_assets_per_adult : Amount;
      test                         : Assets_Means_Test_System;
      contribution                 : out Amount;
      result_state                 : out Means_Test_Result );

   function Get_UAP_Level( 
      ad : Person; 
      num_adults_in_bu : Person_Count;
      get_needs : Make_Needs_Score_Access;
      care_blind_degree : Amount;
      thresholds : UAP_Array ) return UAP_Level;
   
end Model.WSC.Static_Calculator;
