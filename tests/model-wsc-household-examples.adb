with Ada.Calendar;

package body Model.WSC.Household.Examples is

   function Make_Retired_Adult( 
      hh        : Household;
      age       : Age_Range; 
      sex       : Gender_Type; 
      pid       : Sernum_Value; 
      is_couple : Boolean; 
      health    : Health_Status_Type ) return Person is
      ad : Person;
      r : Regressors_Array renames ad.regressors;
   begin      
      ad.age := age;
      ad.sex := sex;
      ad.pid := pid;
      ad.personal_wealth := 20_000.0;
      ad.respondent_weights := ( others=>1.0 );
      ad.enumeration_weights := ( others=>1.0 );
      -- ad.annual_income( ni_retir_pension ) := 6_000.0;
      ad.current_income( ni_retir_pension ) := 6_000.0/52.0;
      ad.employment_status := retired;  
      for f in Task_Type loop
         if( age < 70 )then
            ad.fitness( f ).help := by_self;
            ad.fitness( f ).diff := fairly_easy;
         elsif( age < 85 )then
            ad.fitness( f ).help := with_help_from_someone_else;
            ad.fitness( f ).diff := fairly_difficult;
         else
            ad.fitness( f ).help := not_at_all;
            ad.fitness( f ).diff := very_difficult;
         end if;
      end loop;
      if( is_couple )then      
         ad.marital_status := married;
         ad.partner_status := married_and_with_spouse;
      else
         ad.marital_status := never_married;
         ad.partner_status := neither;
      end if;         
      ad.health_status := health;
      Recalculate_Regressors( ad, hh.Num_Adults, hh.Num_Children, hh.hdata );
      ad.health_score := r( healthsc );
      ad.activities_of_daily_living_score := r( adlscore );
      return ad;
   end Make_Retired_Adult;
   
   function Get_Household( which : Example_Type ) return Household is
   use Ada.Calendar;
      hh : Household;
      bu : Benefit_Unit;
    begin
      hh.wave := r;
      hh.hdata.wave := r;
      hh.hdata.interview_date := Time_Of( 2009, 1, 1 );
      hh.hdata.region := wales;
      hh.hdata.mortgage_outstanding := 500.0;
      hh.hdata.house_value := 80_000.0;
      hh.hdata.other_property_value := 80_000.0;
      hh.hdata.ct_band := band_c;
      hh.hdata.weights := ( others => 1.0 );
      case which is
      when single_retired_person =>
         hh.hid := 1_000_000;
         hh.hdata.hid := 1_000_000;
         hh.num_benefit_units := 1;
         bu.num_people := 1;
         bu.num_adults := 1;
         bu.position_of_head := 1;
         bu.position_of_spouse := 0;
         bu.adults( 1 ) := Make_Retired_Adult( hh, 65, female, 1_000_001, False, fair );
         hh.benefit_units( 1 ) := bu;
      when old_sick_single_male =>
         hh.hid := 2_000_000;
         hh.hdata.hid := 2_000_000;
         hh.num_benefit_units := 1;
         bu.num_people := 1;
         bu.num_adults := 1;
         bu.position_of_head := 1;
         bu.position_of_spouse := 0;
         bu.adults( 1 ) := Make_Retired_Adult( hh, 90, male, 2_000_001, False, very_poor );
         hh.benefit_units( 1 ) := bu;
      when couple_bu_retired     =>
         hh.hdata.wave := r;
         hh.hid := 3_000_000;
         hh.hdata.hid := 3_000_000;
         hh.num_benefit_units := 1;
         bu.num_people := 2;
         bu.position_of_head := 1;
         bu.position_of_spouse := 2;
         bu.num_adults := 2;
         bu.adults( 1 ) := Make_Retired_Adult( hh, 65, male, 3_000_001, True, excellent );
         bu.adults( 2 ) := Make_Retired_Adult( hh, 65, female, 3_000_002, True, excellent );
         hh.benefit_units( 1 ) := bu;
      when cpag_terry_and_julie =>
         hh.hdata.wave := r;
         hh.hid := 4_000_000;
         hh.hdata.hid := 4_000_000;
         hh.num_benefit_units := 1;
         bu.num_people := 2;
         bu.position_of_head := 1;
         bu.position_of_spouse := 2;
         bu.num_adults := 2;
         bu.adults( 1 ) := Make_Retired_Adult( hh, 66, male, 4_000_001, True, excellent );
         bu.adults( 1 ).current_income := ( others => 0.0 );
         bu.adults( 1 ).current_income( ni_retir_pension ) := 163.0;
         bu.adults( 1 ).current_income( annuity_or_priv_pens ) := 30.0;         
         bu.adults( 2 ) := Make_Retired_Adult( hh, 66, female, 2_000_001, True, excellent );         
         -- bu.adults( 1 ).annual_income := bu.adults( 1 ).current_income;
         -- bu.adults( 2 ).annual_income := bu.adults( 1 ).current_income;
         hh.benefit_units( 1 ) := bu;
      when cpag_angelina_and_michael =>
         hh.hdata.wave := r;
         hh.hid := 5_000_000;
         hh.hdata.hid := 5_000_000;
         hh.num_benefit_units := 1;
         bu.num_people := 2;
         bu.position_of_head := 1;
         bu.position_of_spouse := 2;
         bu.num_adults := 2;
         bu.adults( 1 ) := Make_Retired_Adult( hh, 67, male, 5_000_001, True, excellent );
         bu.adults( 1 ).current_income := ( others => 0.0 );
         bu.adults( 1 ).current_income( ni_retir_pension ) := 102.15;
         bu.adults( 1 ).current_income( annuity_or_priv_pens ) := 60.0;         
         bu.adults( 2 ) := Make_Retired_Adult( hh, 58, female, 2_000_001, True, excellent );         
         bu.adults( 1 ).current_income( annuity_or_priv_pens ) := 60.0;         
         -- bu.adults( 1 ).annual_income := bu.adults( 1 ).current_income;
         -- bu.adults( 2 ).annual_income := bu.adults( 1 ).current_income;
         hh.benefit_units( 1 ) := bu;
      when young_single =>
         hh.hdata.wave := r;
         hh.hid := 6_000_000;
         hh.hdata.hid := 6_000_000;
         hh.num_benefit_units := 1;
         bu.num_people := 1;
         bu.position_of_head := 1;
         bu.position_of_spouse := 0;
         bu.num_adults := 1;
         bu.adults( 1 ) := Make_Retired_Adult( hh, 55, male, 6_000_001, True, excellent );
         bu.adults( 1 ).current_income := ( others => 0.0 );
         bu.adults( 1 ).current_income( ni_retir_pension ) := 0.0;
         bu.adults( 1 ).current_income( annuity_or_priv_pens ) := 0.0;         
         bu.adults( 1 ).employment_status := in_paid_employ;
         hh.benefit_units( 1 ) := bu;
         -- ... and so on    
      when age_uk_indira =>
         hh.hdata.wave := r;
         hh.hid := 7_000_000;
         hh.hdata.hid := 7_000_000;
         hh.num_benefit_units := 1;
         bu.num_people := 1;
         bu.position_of_head := 1;
         bu.position_of_spouse := 0;
         bu.num_adults := 1;
         bu.adults( 1 ) := Make_Retired_Adult( hh, 70, female, 7_000_001, True, excellent );
         bu.adults( 1 ).current_income := ( others => 0.0 );
         bu.adults( 1 ).current_income( ni_retir_pension ) := 105.25;
         bu.adults( 1 ).current_income( annuity_or_priv_pens ) := 40.0;  
         bu.adults( 1 ).current_income( maint_or_alimony ) := 20.0;  
         bu.adults( 1 ).employment_status := retired;
         hh.benefit_units( 1 ) := bu;
      when age_uk_sarah =>   
         hh.hdata.wave := r;
         hh.hid := 8_000_000;
         hh.hdata.hid := 8_000_000;
         hh.num_benefit_units := 1;
         bu.num_people := 1;
         bu.position_of_head := 1;
         bu.position_of_spouse := 0;
         bu.num_adults := 1;
         bu.adults( 1 ) := Make_Retired_Adult( hh, 62, female, 8_000_001, True, excellent );
         bu.adults( 1 ).current_income := ( others => 0.0 );
         bu.adults( 1 ).current_income( ni_retir_pension ) := 115.25;
         bu.adults( 1 ).current_income( annuity_or_priv_pens ) := 0.0;  
         bu.adults( 1 ).current_income( maint_or_alimony ) := 0.0;  
         bu.adults( 1 ).current_income( attendance_allow ) := 1.0;  
                  
         bu.adults( 1 ).employment_status := retired;
         hh.benefit_units( 1 ) := bu;
      when zero_income =>   
         hh.hdata.wave := r;
         hh.hid := 9_000_000;
         hh.hdata.hid := 9_000_000;
         hh.num_benefit_units := 1;
         bu.num_people := 1;
         bu.position_of_head := 1;
         bu.position_of_spouse := 0;
         bu.num_adults := 1;
         bu.adults( 1 ) := Make_Retired_Adult( hh, 69, female, 9_000_001, True, excellent );
         bu.adults( 1 ).current_income := ( others => 0.0 );
         bu.adults( 1 ).current_income( ni_retir_pension ) := 0.0;
         bu.adults( 1 ).current_income( annuity_or_priv_pens ) := 0.0;  
         bu.adults( 1 ).current_income( maint_or_alimony ) := 0.0;  
         bu.adults( 1 ).current_income( attendance_allow ) := 0.0;  
                  
         bu.adults( 1 ).employment_status := retired;
         hh.benefit_units( 1 ) := bu;
      end case;
      return hh;
   end Get_Household;

end Model.WSC.Household.Examples;
