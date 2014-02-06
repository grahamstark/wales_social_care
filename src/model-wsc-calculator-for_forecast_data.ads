with Model.WSC.Run_Declarations;
with Model.WSC.Parameters;
with Model.WSC.Output;
with Model.WSC.Run_Settings;
with Model.WSC.Household;
with Model.WSC.Results;
with Model.WSC.Results.IO;

with WSC_Enums;

package Model.WSC.Static_Calculator is

   use Model.WSC.Run_Settings;
   use Model.WSC.Run_Declarations;
   use Model.WSC.Output;
   use Model.WSC.Parameters;
   use Model.WSC.Household;
   use Model.WSC.Results;
   use Model.WSC.Results.IO;
   use WSC_Enums;
   
   procedure Calculate_One_Household(
      hh               : Model.WSC.Household.Household;
      params           : Parameters_Rec;
      results          : in out Household_Result;
      previous_results : Individual_Results_Map;
      wsc_run : Run );
   
private
   
   function Calculate_Attendance_Allowance( 
      pers            : Person;  
      aa_last_period  : Amount;
      sys             : Attendance_Allowance_System;
      wsc_run : Run ) return Amount;
   
   procedure Calculate_DLA( 
      pers            : Person; 
      care            : out Amount;
      mobility        : out Amount;
      dla_last_period : Amount;
      sys             : Disability_Living_Allowance_System;      
      wsc_run : Run );
      
   procedure Calculate_Guaranteed_Pension_Credit( 
      bu  : Benefit_Unit;  
      res : in out Benefit_Unit_Result;
      sys : Guaranteed_Credit_System;
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

end Model.WSC.Static_Calculator;
