with WSC_Enums;
with Model.WSC.Household;
with Keyed_Text_Buffer;
with Ada.Calendar;
with T_Utils;

package Model.WSC.Parameters is
   
   use WSC_Enums;
   use Model.WSC.Household;
   
   subtype System_Number is Positive range 1 .. 2;

   PRE_SYS : constant System_Number := 1;
   POST_SYS : constant System_Number := 2;

   function Get_Default_Incomes( which : Means_Test_Type ) return Included_Incomes_Array;   
   
   type Attendance_Allowance_System is record
      low_age         : Age_Range;
      high_age        : Age_Range;
      benefit_rate    : High_Low_Array;
      test_generosity : Rate;
      preserve_for_existing_claimants : Boolean;
   end record;
   
   type DLA_Mobility_System is record
      low_age         : Age_Range;
      high_age        : Age_Range;
      benefit_rate    :  High_Low_Array := ( others => 0.0 );
      test_generosity : Rate;
      preserve_for_existing_claimants : Boolean;
   end record;
   
   type DLA_Care_System is record
      low_age                         : Age_Range;
      high_age                        : Age_Range;
      benefit_rate                    : High_Middle_Low_Array := ( others => 0.0 );
      test_generosity                 : Rate;
      preserve_for_existing_claimants : Boolean;
   end record;
   
   type Disability_Living_Allowance_System is record
      dont_pay_for_residential_claimants : Boolean := False;
      mobility                           : DLA_Mobility_System;
      care                               : DLA_Care_System;      
      preserve_for_existing_claimants    : Boolean;
   end record;
   
   type Pension_System is record
      age_men   : Age_Range;
      age_women : Age_Range;
      citizens_pension  : Boolean := False;
      class_a : Amount;
      preserve_for_existing_claimants : Boolean;
   end record;
   
   type Guaranteed_Credit_System is record
      single : Amount;
      couple : Amount;
      carer_single : Amount;
      severe_disability_single : Amount;
      severe_disability_couple : Amount;
      incomes : Included_Incomes_Array := Get_Default_Incomes( guaranteed_pension_credit );
      earnings_disregard : Amount;
      benefit_disregard  : Amount; -- need a list of benefits this applies to
      preserve_for_existing_claimants : Boolean;
   end record;
   
  type Savings_Credit_System is record
      threshold_single : Amount;
      threshold_couple : Amount;
      maximum_single : Amount;
      maximum_couple : Amount;
      withdrawal_rate : Rate;
      incomes : Included_Incomes_Array  := Get_Default_Incomes( savings_credit );
      qualifying_incomes : Included_Incomes_Array  := Get_Default_Incomes( savings_credit_qualifying_income );
      earnings_disregard : Amount;
      benefit_disregard  : Amount;
      preserve_for_existing_claimants : Boolean;
  end record;
  
   type Pension_Credit_System is record
      guaranteed_credit : Guaranteed_Credit_System;
      savings_credit : Savings_Credit_System;
   end record;
   
   
   type Social_Care_Needs_System is record   
      uap_category                   : UAP_Array;
      share_of_category_passing_test : UAP_Array;
      use_carer_blind_system         : Boolean;
      carer_blind_degree             : Rate;
   end record;

   type Assets_Means_Test_System is record   
      include_property : Boolean;
      upper_limit      : Amount;
      lower_limit      : Amount;
      taper            : Rate;
      abolish          : Boolean;
   end record;

   type Income_Means_Test_System is record   
      floor                    : Rate;
      minimum_support_level    : Amount;
      percent_costs_met        : Amount;
      incomes                  : Included_Incomes_Array := 
          Get_Default_Incomes( guaranteed_pension_credit ); -- FIXME proprer def for this!
      abolish                  : Boolean;
   end record;

   type Maximums is record
      maximum_weekly_charge_non_residential : Amount;
      maximum_weekly_charge_residential     : Amount;
      maximum_lifetime_payment              : Amount;
   end record;
   
   type Social_Care_Means_Test_System is record
      assets : Assets_Means_Test_System;
      income : Income_Means_Test_System;
   end record;

   type Means_Test_System is record
      non_residential : Social_Care_Means_Test_System;
      residential     : Social_Care_Means_Test_System;
      maxima          : Maximums;
   end record;

   type Insurance_System is record
      actuarially_fair : Boolean;
      contribution : Contribution_Type;
   end record;
   
   type Care_Options is record
      abolish                         : Boolean;
      preserve_for_existing_claimants : Boolean;
   end record;
   
   type Social_Care_System is record
      means_test                      : Means_Test_System;
      needs_assessment_rules          : Social_Care_Needs_System;
      insurance                       : Insurance_System;
      options                         : Care_Options;
   end record;

   type Benefits_System is record
      state_pension : Pension_System;
      attendance_allowance : Attendance_Allowance_System;
      pension_credit : Pension_Credit_System;
      dla : Disability_Living_Allowance_System;
   end record;
   
   type Average_Costs_Rec is record
      hour_of_care : Amount;
      residential_per_week : Amount;
   end record;
   
   type Parameters_Rec is tagged record
      benefits    : Benefits_System;
      social_care : Social_Care_System;
      av_costs    : Average_Costs_Rec;
      is_null     : Boolean := False;
   end record;
   
   type Parameters_Array is array( Simulation_Years ) of Parameters_Rec;
   
   function Get_Null_Parameters return Parameters_Array;   
   function Get_Default_Parameters return Parameters_Array;
   
   -- say, 0.12 for a 12% increase
   procedure Uprate( p : in out Parameters_Rec; v : Rate );
   procedure Uprate( p : in out Average_Costs_Rec; v : Rate );
   
   procedure Copy_Amounts_Only( to : in out Parameters_Rec; from : Parameters_Rec );
      
   --
   -- uprate all the money parameters. This copies the money amount parameters (not withdrawal rates,  ages, etc.)
   -- with an uprated copy of the start_year, overwriting any changes already there.
   -- so, to uprate by rpi plus 10% from 2011 to 2030, enter:
   -- by_which=>rpi, and m=>10.0, start_year=>2011 end_year=>2030 
   --
   procedure Uprate(
      params      : in out Parameters_Array;
      index       : Which_Uprate_Index;
      m           : Rate;
      start_year  : Simulation_Years;
      end_year    : Simulation_Years );
      
end Model.WSC.Parameters;
