--
-- Created by ada_generator.py on 2012-07-24 19:03:55.560105
-- 
with Ada.Text_IO; 
with Ada.Strings.Bounded; 
with Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Characters.Conversions;
with Ada.Characters.Latin_1;
with Ada.Calendar;


package Base_Types is

   use Ada.Strings.Bounded;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   --
   --  standard types we use everywhere
   --
   package wconv renames Ada.Characters.Conversions;
   package stda renames Ada.Characters.Latin_1;
   
   type Real is new Long_Float;
   type Decimal is delta 0.01 digits 10;
   type Big_Integer is range -9223372036854775808 .. 9223372036854775807;
   --
   --
   --
   DOS_NEW_LINE  : constant String (1 .. 2) := (stda.CR, stda.LF);
   UNIX_NEW_LINE : constant String (1 .. 1) := (1 => stda.LF);

   
   type personal_results_passes_residential_means_test_Enum is ( not_applicable, not_entitled, partially_entitled, fully_entitled ) ;
   type personal_income_income_type_Enum is ( gross_wage, gross_self_employment, net_wage, net_self_employment, income_tax, ni, ni_retir_pension, sev_disabl_allow, invalidity_pens, ind_injury_allow, attendance_allow, pension_prev_emp, mobility_allow, inv_care_allow, war_disabl_pens, disab_livng_allwnce, disab_wrkng_allwnce, incapacity_benefit, disab_liv_allowcare, disab_liv_allowmob, disab_liv_allow_dk, pens_spse_prev_emp, unempl_or_incme_supt, income_support, unempl_benefit, ni_sick_benefit, child_benefit, one_parent_benefit, family_credit, maternity_allow, housing_benefit, comm_charge_bene, other_state_bene, job_seekers_allow, child_tax_credit, return_to_work_credit, widow_or_war_pens, foster_allowance, sick_or_acci_insurance, any_other_payment, wid_mothr_allow, pension_credit, educa_grant, t_u_or_friendly_soc_payt, maint_or_alimony, payment_abs_relative, rent_or_boarders_or_lodgers, rent_other_prop, annuity_or_priv_pens ) ;
   type personal_results_uap_Enum is ( critical, substantial, moderate, low, none ) ;
   type personal_results_passes_residential_income_test_Enum is ( not_applicable, not_entitled, partially_entitled, fully_entitled ) ;
   type run_weighting_function_Enum is ( chi_square, constrained_chi_square, d_and_s_type_a, d_and_s_type_b, d_and_s_constrained ) ;
   type state_health_Enum is ( normal, aborted, in_error, queued ) ;
   type run_status_Enum is ( edited, displayed, neither ) ;
   type run_type_of_run_Enum is ( simulation, data_creation ) ;
   type uprate_assumption_target_Enum is ( upr_housing_costs, upr_wealth, upr_state_benefits, upr_earnings, upr_other_income, upr_care_costs_1, upr_care_costs_2, upr_care_costs_3 ) ;
   type uap_threshold_uap_level_Enum is ( critical, substantial, moderate, low, none ) ;
   type probit_threshold_element_Enum is ( dies_this_period, has_wealth, informal_care_from_household_member, informal_care_from_non_householder, hh_split, health_better, health_worse, adl_improve, adl_worsen, retire, to_care, receiving_aa, receiving_dla ) ;
   type personal_results_passes_non_residential_income_test_Enum is ( not_applicable, not_entitled, partially_entitled, fully_entitled ) ;
   type personal_results_passes_non_residential_means_test_Enum is ( not_applicable, not_entitled, partially_entitled, fully_entitled ) ;
   type user_type_utype_Enum is ( anon, registered, expert, admin, invalid, deleted ) ;
   type state_phase_Enum is ( not_started, queued, run_starting, pre_calculations, running, generating_output, complete ) ;
   type personal_results_passes_non_residential_capital_test_Enum is ( not_applicable, not_entitled, partially_entitled, fully_entitled ) ;
   type user_type_lang_Enum is ( en ) ;
   type personal_results_passes_residential_capital_test_Enum is ( not_applicable, not_entitled, partially_entitled, fully_entitled ) ;
   type uprate_assumption_element_Enum is ( obr_earnings, rpi, rpix, cpi, producer_output_prices, government_consumption, fixed_investment, business_investment, private_dwellings, general_government, net_acquisition_of_valuables, change_in_inventories, exports, total_final_expenditure, imports, statistical_discrepancy, gdp_at_market_prices, private_consumption_nominal, government_consumption_nominal, fixed_investment_nominal, net_acquisition_of_valuables_nominal, change_in_inventories_nominal, exports_nominal, total_final_expenditure_nominal, imports_nominal, statistical_discrepancy_nominal, gdp_at_market_prices_nominal, gross_national_income_nominal, employment, employment_rate, ilo_unemployment, ilo_unemployment_rate, participation_rate, claimant_count, trend_employment_rate, other1, other2, other3, other4, other5, other6 ) ;
   
   MISSING_I_KEY : constant := -12345678;
   MISSING_S_KEY : constant String := "-12345678";
   MISSING_W_KEY : constant Unbounded_String := To_Unbounded_String("-12345678");
   MISSING_R_KEY : constant := -12345678.0;
   MISSING_T_KEY : constant Ada.Calendar.Time := Ada.Calendar.Time_Of( 2099, 11, 11, 9.0 );
   
   FIRST_DATE    : constant Ada.Calendar.Time := Ada.Calendar.Time_Of( 1901, 1, 1, 0.0 );
   
   --
   -- used for declaring strings as fixed sets of characters
   --
   PADDING_CHAR : constant Character := '@';
   --
   -- used for trim calls on retrieved strings
   --
   PADDING_CHAR_SET : constant Ada.Strings.Maps.Character_Set := 
      Ada.Strings.Maps.To_Set ( PADDING_CHAR );

   Null_Wide : constant Unbounded_String := Null_Unbounded_String;
   
   function Slice_To_Unbounded( s : String; start : Positive; stop : Natural ) return Unbounded_String;
   
   -- 
   --  some standard io packages typed for the above
   --
   package fix_io is new Ada.Text_IO.Decimal_IO (Decimal);
   package real_io is new Ada.Text_IO.Float_IO (real);
   package std_io is new Ada.Text_IO.Integer_IO (Integer);
   package string_io renames Ada.Text_IO;
   package Str80 is new Ada.Strings.Bounded.Generic_Bounded_Length(80);

end Base_Types;
