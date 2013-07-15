with Model.WSC.Uprate;
with Ada.Text_IO;

package body Model.WSC.Parameters is

   use Ada.Text_IO;   
   
   UPRATE_ROUND_UP : constant Amount := 0.01;
   --
   -- FIXME this ignores disregards !!!!!!!!
   --
   function Get_Default_Incomes( which : Means_Test_Type ) return Included_Incomes_Array is
      inc : Included_Incomes_Array := ( others => 0.0 );
   begin
      case which is
         when guaranteed_pension_credit | savings_credit =>    
            inc( pension_credit ) := 0.0;
            inc( incapacity_benefit ) := 1.0;
            inc( family_credit ) := 1.0;
            inc( job_seekers_allow ) := 1.0;
            inc( sev_disabl_allow ) := 1.0;
            inc( maternity_allow ) := 1.0;
            inc( maint_or_alimony ) := 1.0;
         when savings_credit_qualifying_income =>
            inc( pension_credit ) := 0.0;
            inc( incapacity_benefit ) := 0.0;
            inc( family_credit ) := 0.0;
            inc( job_seekers_allow ) := 0.0;
            inc( sev_disabl_allow ) := 0.0;
            inc( maternity_allow ) := 0.0;
            inc( maint_or_alimony ) := 0.0;
      end case;
      inc( gross_wage ) := 1.0;
      inc( gross_self_employment ) := 1.0;
      inc( net_wage ) := 0.0;
      inc( net_self_employment ) := 0.0;
      inc( income_tax ) := -1.0;
      inc( ni ) := -1.0;
      inc( ni_retir_pension ) := 1.0;
      inc( invalidity_pens ) := 1.0;
      inc( ind_injury_allow ) := 1.0;
      inc( attendance_allow ) := 0.0;
      inc( pension_prev_emp ) := 1.0;
      inc( mobility_allow ) := 1.0;
      inc( inv_care_allow ) := 1.0;
      inc( war_disabl_pens ) := 1.0;
      inc( disab_livng_allwnce ) := 0.0;
      inc( disab_wrkng_allwnce ) := 1.0;
      inc( disab_liv_allowcare ) := 0.0;
      inc( disab_liv_allowmob ) := 0.0;
      inc( disab_liv_allow_dk ) := 0.0;
      inc( pens_spse_prev_emp ) := 1.0;
      inc( unempl_or_incme_supt ) := 1.0;
      inc( income_support ) := 1.0;
      inc( unempl_benefit ) := 1.0;
      inc( ni_sick_benefit ) := 1.0;
      inc( child_benefit ) := 0.0;
      inc( one_parent_benefit ) := 1.0;
      inc( housing_benefit ) := 1.0;
      inc( comm_charge_bene ) := 1.0;
      inc( other_state_bene ) := 1.0;
      inc( child_tax_credit ) := 0.0;
      inc( return_to_work_credit ) := 1.0;
      inc( widow_or_war_pens ) := 1.0;
      inc( foster_allowance ) := 1.0;
      inc( sick_or_acci_insurance ) := 1.0;
      inc( any_other_payment ) := 1.0;
      inc( wid_mothr_allow ) := 1.0;
            
      inc( educa_grant ) := 1.0;
                  -- 
      inc( t_u_or_friendly_soc_payt ) := 1.0;
      inc( payment_abs_relative ) := 1.0;
      inc( rent_or_boarders_or_lodgers ) := 1.0;
      inc( rent_other_prop ) := 1.0;
      inc( annuity_or_priv_pens ) := 1.0;
      return inc;
   end Get_Default_Incomes;
   
   function Get_Null_Parameters return Parameters_Array is   
      p : Parameters_Array;
   begin
      for y in Simulation_Years loop
         p( y ).is_null := True;
      end loop;
      return p;
   end Get_Null_Parameters;
   
   function Get_Default_Parameters return Parameters_Array is
      p : Parameters_Array;
   begin
      for y in Simulation_Years loop
         p( y ).is_null := False;
      end loop;
      return p;
   end Get_Default_Parameters;

   procedure Uprate( p : in out Pension_System; v : Rate ) is
   begin
      Uprate( p.class_a, v, UPRATE_ROUND_UP );
   end Uprate;
   
   procedure Uprate( p : in out Guaranteed_Credit_System; v : Rate ) is
   begin
      Uprate( p.single, v, UPRATE_ROUND_UP );
      Uprate( p.couple, v, UPRATE_ROUND_UP );
      Uprate( p.carer_single, v, UPRATE_ROUND_UP );
      Uprate( p.severe_disability_single, v, UPRATE_ROUND_UP );
      Uprate( p.severe_disability_couple, v, UPRATE_ROUND_UP );
   end Uprate;
   
  procedure Uprate( p : in out Savings_Credit_System; v : Rate ) is
   begin
      Uprate( p.threshold_single, v, UPRATE_ROUND_UP );
      Uprate( p.threshold_couple, v, UPRATE_ROUND_UP );
      Uprate( p.maximum_single, v, UPRATE_ROUND_UP );
      Uprate( p.maximum_couple, v, UPRATE_ROUND_UP );
  end Uprate;
  
   procedure Uprate( p : in out Pension_Credit_System; v : Rate ) is
   begin
      Uprate( p.guaranteed_credit, v );
      Uprate( p.savings_credit, v );
   end Uprate;
   
   procedure Uprate( p : in out Attendance_Allowance_System; v : Rate ) is
   begin
      Uprate( p.benefit_rate( high ), v, UPRATE_ROUND_UP );
      Uprate( p.benefit_rate( low ) , v, UPRATE_ROUND_UP );
   end Uprate;
   
   procedure Uprate( p : in out DLA_Mobility_System; v : Rate ) is
   begin
      Uprate( p.benefit_rate( high ), v, UPRATE_ROUND_UP );
      Uprate( p.benefit_rate( low ) , v, UPRATE_ROUND_UP );
   end Uprate;
   
   procedure Uprate( p : in out DLA_Care_System; v : Rate ) is
   begin
      Uprate( p.benefit_rate( high ), v, UPRATE_ROUND_UP );
      Uprate( p.benefit_rate( middle ), v, UPRATE_ROUND_UP );
      Uprate( p.benefit_rate( low ) , v, UPRATE_ROUND_UP );
   end Uprate;
   
   procedure Uprate( p : in out Disability_Living_Allowance_System; v : Rate ) is
   begin
      Uprate( p.mobility, v );
      Uprate( p.care, v );      
   end Uprate;
   
   
   procedure Uprate( p : in out Assets_Means_Test_System; v : Rate ) is   
   begin
      Uprate( p.upper_limit, v, 100.0 );
      Uprate( p.lower_limit, v, 100.0 );
   end Uprate;

   
   procedure Uprate( p : in out Maximums; v : Rate ) is   
   begin
      Put_Line( "uprating " & Format( p.maximum_weekly_charge_residential ) & " by " & Format( v ));
      Uprate( p.maximum_weekly_charge_residential, v, UPRATE_ROUND_UP );
      Uprate( p.maximum_weekly_charge_non_residential, v, UPRATE_ROUND_UP );
      Uprate( p.maximum_lifetime_payment, v, UPRATE_ROUND_UP );
   end Uprate;
   
   procedure Uprate( p : in out Income_Means_Test_System; v : Rate ) is   
   begin
      Uprate( p.minimum_support_level, v, UPRATE_ROUND_UP );
      Uprate( p.percent_costs_met, v, UPRATE_ROUND_UP );
   end Uprate;

   procedure Uprate( p : in out Social_Care_Means_Test_System; v : Rate ) is
   begin
      Uprate( p.assets, v );
      Uprate( p.income, v );
   end Uprate;

   procedure Uprate( p : in out Means_Test_System; v : Rate ) is
   begin
      Uprate( p.residential, v );
      Uprate( p.non_residential, v );
      Uprate( p.maxima, v );
   end Uprate;
   
   procedure Uprate( p : in out Social_Care_System; v : Rate ) is
   begin
      Uprate( p.means_test, v );
   end Uprate;

   procedure Uprate( p : in out Benefits_System; v : Rate ) is
   begin
      Uprate( p.state_pension, v );
      Uprate( p.attendance_allowance, v );
      Uprate( p.pension_credit, v );
      Uprate( p.dla, v );
   end Uprate;

   procedure Uprate( p : in out Average_Costs_Rec; v : Rate ) is
   begin
      p.hour_of_care := p.hour_of_care * (1.0 + v);
      p.residential_per_week := p.residential_per_week * (1.0 + v);
   end Uprate;
   
   procedure Uprate( p : in out Parameters_Rec; v : Rate ) is
   begin
      Uprate( p.benefits, v );
      Uprate( p.social_care, v );
      Uprate( p.av_costs, v );
   end Uprate;
   

   procedure Copy_Amounts_Only( to : in out Parameters_Rec; from : Parameters_Rec ) is 
   begin
      to.benefits.state_pension.class_a := from.benefits.state_pension.class_a;
      to.benefits.pension_credit.guaranteed_credit.single := from.benefits.pension_credit.guaranteed_credit.single;
      to.benefits.pension_credit.guaranteed_credit.couple := from.benefits.pension_credit.guaranteed_credit.couple;
      to.benefits.pension_credit.guaranteed_credit.carer_single := from.benefits.pension_credit.guaranteed_credit.carer_single;
      to.benefits.pension_credit.guaranteed_credit.severe_disability_single := from.benefits.pension_credit.guaranteed_credit.severe_disability_single;
      to.benefits.pension_credit.guaranteed_credit.severe_disability_couple := from.benefits.pension_credit.guaranteed_credit.severe_disability_couple;
      to.benefits.pension_credit.savings_credit.threshold_single := from.benefits.pension_credit.savings_credit.threshold_single;
      to.benefits.pension_credit.savings_credit.threshold_couple := from.benefits.pension_credit.savings_credit.threshold_couple;
      to.benefits.pension_credit.savings_credit.maximum_single := from.benefits.pension_credit.savings_credit.maximum_single;
      to.benefits.pension_credit.savings_credit.maximum_couple := from.benefits.pension_credit.savings_credit.maximum_couple;
      to.benefits.attendance_allowance.benefit_rate( high ) := from.benefits.attendance_allowance.benefit_rate( high );
      to.benefits.attendance_allowance.benefit_rate( low ) := from.benefits.attendance_allowance.benefit_rate( low );
      to.benefits.dla.mobility.benefit_rate( high ) := from.benefits.dla.mobility.benefit_rate( high );
      to.benefits.dla.mobility.benefit_rate( low ) := from.benefits.dla.mobility.benefit_rate( low );
      to.benefits.dla.care.benefit_rate( high ) := from.benefits.dla.care.benefit_rate( high );
      to.benefits.dla.care.benefit_rate( middle ) := from.benefits.dla.care.benefit_rate( middle );
      to.benefits.dla.care.benefit_rate( low ) := from.benefits.dla.care.benefit_rate( low );
      to.social_care.means_test.residential.assets.upper_limit := from.social_care.means_test.residential.assets.upper_limit;
      to.social_care.means_test.residential.income.floor := from.social_care.means_test.residential.income.floor;
      to.social_care.means_test.residential.income.minimum_support_level := from.social_care.means_test.residential.income.minimum_support_level;
      to.social_care.means_test.non_residential.income.floor := from.social_care.means_test.non_residential.income.floor;
      to.social_care.means_test.non_residential.income.minimum_support_level := from.social_care.means_test.non_residential.income.minimum_support_level;
      to.social_care.means_test.maxima.maximum_weekly_charge_residential := from.social_care.means_test.maxima.maximum_weekly_charge_residential;
      to.social_care.means_test.maxima.maximum_weekly_charge_non_residential := from.social_care.means_test.maxima.maximum_weekly_charge_non_residential;
      to.social_care.means_test.maxima.maximum_lifetime_payment := from.social_care.means_test.maxima.maximum_lifetime_payment;
      to.av_costs.hour_of_care := from.av_costs.hour_of_care;
   end Copy_Amounts_Only;
   
   procedure Uprate(
      params      : in out Parameters_Array;
      index       : Which_Uprate_Index;
      m           : Rate;
      start_year  : Simulation_Years;
      end_year    : Simulation_Years ) is
      upr      : Rate := 1.0;
      p        : Natural := 0;
      by_which : Forecast_Element; 
   begin
      case index is
         when uprate_none => null;
         when uprate_rpi => by_which := rpi;
         when uprate_rpix => by_which := rpix;
         when uprate_earnings => by_which := obr_earnings;
         when uprate_cpi => by_which := cpi;
         when uprate_gdp => by_which := obr_earnings;   
      end case;
      for y in start_year .. end_year loop
          if( index /= uprate_none )then
             upr := Model.WSC.Uprate.Get_Ratio_Between( by_which, start_year-1, 3, y, 3 );
             upr := upr + m/100.0;
          else
             p := p + 1;
             upr := ( 1.0 + ( m / 100.0 )) ** p;
          end if;
          Put_Line( "upr for year " & y'Img & " => " & Format( upr ) & " m = " & Format( m ));
          Copy_Amounts_Only( to => params( y ), from => params( start_year ));
          Uprate( params( y ), upr - 1.0 );
      end loop;
   end Uprate;
   
end Model.WSC.Parameters;
