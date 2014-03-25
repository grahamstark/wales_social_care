--
-- copyright(c) 2011 Graham Stark/ Virtual Worlds (graham.stark@virtual-worlds.biz)/ Howard Reed, Landman Economics (howard@landman-economics.co.uk)
--
-- ////////////////////////////////
--
-- This is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 3, or (at your option)
-- any later version.
-- 
-- It is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this software; see the file docs/gpl_v3.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street,
-- Boston, MA 02110-1301, USA.
-- 
-- /////////////////////////////
pragma License( Modified_GPL );

with T_Utils;
with Base_Model_Types;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
--
-- This holds some basic enumerated types for the model dataset, 
-- based on the BHPS ones with the various missing/no-response codes
-- amalgamated or deleted.
-- This is in a seperated package to the Household itself to cut away some nasty cross-dependencies. 
--
package WSC_Enums is

   use Base_Model_Types;
   use Ada.Calendar;
   use Ada.Strings.Unbounded;

   BHPS_START_YEAR : constant := 1991;

   --
   -- FIXME: move this somewgere common with BHPS? 
   --
   -- subtype Sernum_Value is Big_Integer range -9 .. 1_000_000_000; -- the variable 'gechphid' has -8s sometimes for missing values
   -- type Sernum_Value is range -9 .. 1_000_000_000; -- the variable 'gechphid' has -8s sometimes for missing values
                             --  9223372036854775807
   -- MISSING_SERNUM : constant Sernum_Value := Sernum_Value'First;
   
   function To_Unbounded_String( s : Sernum_Value ) return Unbounded_String;
   
   subtype Decile_Number is Integer range 1 .. 10;
   
   subtype Iteration_Number is Integer range 1 .. 2_000;
   
   package Decile_Package is new T_Utils( 
      T => Decile_Number,
      Amount_Type=>Amount, 
      Rate_Type=>Rate, 
      Counter_Type=>Counter_Type );
   subtype Deciles_Array is  Decile_Package.Amount_Array;      
   
   type Costs_Type is (
      social_care_residential,
      social_care_non_residential,
      attendance_allowance,
      state_pension,
      savings_credit,
      guaranteed_pension_credit,
      dla_mobility,
      dla_care
   );
   
   package Costs_Type_Package is new T_Utils( 
      T => Costs_Type,
      Amount_Type=>Amount, 
      Rate_Type=>Rate, 
      Counter_Type=>Counter_Type );
   subtype Costs_Array is  Costs_Type_Package.Amount_Array;      
   
   type Summary_Statistics_Type is (
      no_statistic,
      mean,
      minimum,
      maximum,
      median,
      standard_deviation,
      decile_1,
      decile_10  -- etc.
   );

   type Incomes_Type is (
      gross_wage,
      gross_self_employment,
      net_wage,
      net_self_employment,
      
      income_tax,
      ni,

      ni_retir_pension,
      sev_disabl_allow,
      invalidity_pens,
      ind_injury_allow,
      attendance_allow,
      pension_prev_emp,
      mobility_allow,
      inv_care_allow,
      war_disabl_pens,
      disab_livng_allwnce,
      disab_wrkng_allwnce,
      incapacity_benefit,
      disab_liv_allowcare,
      disab_liv_allowmob,
      disab_liv_allow_dk,
      pens_spse_prev_emp,
      unempl_or_incme_supt,
      income_support,
      unempl_benefit,
      ni_sick_benefit,
      child_benefit,
      one_parent_benefit,
      family_credit,
      maternity_allow,
      housing_benefit,
      comm_charge_bene,
      other_state_bene,
      job_seekers_allow,
      child_tax_credit,
      return_to_work_credit,
      widow_or_war_pens,
      foster_allowance,
      sick_or_acci_insurance,
      any_other_payment,
      wid_mothr_allow,
      pension_credit,

      educa_grant,
      -- 
      t_u_or_friendly_soc_payt,
      maint_or_alimony,
      payment_abs_relative,
      rent_or_boarders_or_lodgers,
      rent_other_prop,
      annuity_or_priv_pens );
      
   subtype Calculated_Incomes is Incomes_Type range income_tax .. pension_credit;
   subtype State_Benefits is Incomes_Type range ni_retir_pension .. pension_credit;
   subtype Other_Income is Incomes_Type range t_u_or_friendly_soc_payt .. annuity_or_priv_pens;
   subtype Earnings is Incomes_Type range gross_wage .. net_self_employment;
   package Income_Package is new T_Utils( 
      T => Incomes_Type,
      Amount_Type=>Amount, 
      Rate_Type=>Rate, 
      Counter_Type=>Counter_Type );
   subtype Incomes_Array is  Income_Package.Amount_Array;        
   subtype Included_Incomes_Array is  Income_Package.Rate_Array;        

   package Calculated_Incomes_Package is new T_Utils( 
      T => Calculated_Incomes,
      Amount_Type=>Amount, 
      Rate_Type=>Rate, 
      Counter_Type=>Counter_Type );
   subtype Calculated_Incomes_Array is  Calculated_Incomes_Package.Amount_Array;        
   
   NET_INCOME_LIST : constant Included_Incomes_Array := ( 
      gross_wage=> 1.0,
      gross_self_employment=> 1.0,
      net_wage=> 0.0,
      net_self_employment=> 0.0,

      income_tax=> -1.0,
      ni=> -1.0,

      ni_retir_pension=> 1.0,
      sev_disabl_allow=> 1.0,
      invalidity_pens=> 1.0,
      ind_injury_allow=> 1.0,
      attendance_allow=> 1.0,
      pension_prev_emp=> 1.0,
      mobility_allow=> 1.0,
      inv_care_allow=> 1.0,
      war_disabl_pens=> 1.0,
      disab_livng_allwnce=> 1.0,
      disab_wrkng_allwnce=> 1.0,
      incapacity_benefit=> 1.0,
      disab_liv_allowcare=> 1.0,
      disab_liv_allowmob=> 1.0,
      disab_liv_allow_dk=> 1.0,
      pens_spse_prev_emp=> 1.0,
      unempl_or_incme_supt=> 1.0,
      income_support=> 1.0,
      unempl_benefit=> 1.0,
      ni_sick_benefit=> 1.0,
      child_benefit=> 1.0,
      one_parent_benefit=> 1.0,
      family_credit=> 1.0,
      maternity_allow=> 1.0,
      housing_benefit=> 1.0,
      comm_charge_bene=> 1.0,
      other_state_bene=> 1.0,
      job_seekers_allow=> 1.0,
      child_tax_credit=> 1.0,
      return_to_work_credit=> 1.0,
      widow_or_war_pens=> 1.0,
      foster_allowance=> 1.0,
      sick_or_acci_insurance=> 1.0,
      any_other_payment=> 1.0,
      wid_mothr_allow=> 1.0,
      pension_credit=> 1.0,

      educa_grant=> 0.0,
      -- 
      t_u_or_friendly_soc_payt=> 1.0,
      maint_or_alimony=> 1.0,
      payment_abs_relative=> 1.0,
      rent_or_boarders_or_lodgers=> 1.0,
      rent_other_prop=> 1.0,
      annuity_or_priv_pens => 1.0 );
   
  function To_String( i : Incomes_Type ) return String;

  type UAP_Level is ( critical, substantial, moderate, low, none );

   package UAP_Level_Package is new T_Utils( 
      T => UAP_Level,
      Amount_Type=>Amount, 
      Rate_Type=>Rate, 
      Counter_Type=>Counter_Type );
   subtype UAP_Array is  UAP_Level_Package.Amount_Array;   

   type Means_Test_Result is ( not_applicable, not_entitled, partially_entitled, fully_entitled );
   function Combine_Results( result_1, result_2 :  Means_Test_Result ) return Means_Test_Result;

   package Means_Test_Result_Package is new T_Utils( 
      T => Means_Test_Result,
      Amount_Type=>Amount, 
      Rate_Type=>Rate, 
      Counter_Type=>Counter_Type );
   subtype Means_Test_Result_Array is Means_Test_Result_Package.Amount_Array;   
   
   type Contribution_Type is ( none, voluntary, manditory );
 
         
   type Tenure_Type is (  
      owned_outright,
      owned_with_mortgage,
      local_authority_rented,
      housing_assoc_rented,
      rented_from_employer,
      rented_private_unfurnished,
      rented_private_furnished,
      other_rented );
   package Tenure_Type_Package is new T_Utils( 
      T => Tenure_Type,
      Amount_Type=>Amount, 
      Rate_Type=>Rate, 
      Counter_Type=>Counter_Type );
   subtype Tenure_Array is  Tenure_Type_Package.Amount_Array;        

   function Pretty_Print( i : Tenure_Type ) return String;
   
   
   type Gender_Type is ( male, female );
   
   package Gender_Type_Package is new T_Utils( 
      T => Gender_Type,
      Amount_Type=>Amount, 
      Rate_Type=>Rate, 
      Counter_Type=>Counter_Type );
   subtype Gender_Type_Array is  Gender_Type_Package.Amount_Array;        


   type Head_Or_Spouse is ( neither, head, spouse );
   subtype Head_Or_Spouse_Id is Head_Or_Spouse range head .. spouse;

   type Council_Tax_Band is (
      band_a,
      band_b,
      band_c,
      band_d,
      band_e,
      band_f,
      band_g,
      band_h,
      not_valued_separately,
      northern_ireland );
      
   -- type Region_Type is (  
      -- north_east,
      -- wales,
      -- scotland,
      -- northern_ireland,
      -- channel_islands,
      -- north_west,
      -- yorkshire_and_the_humber,
      -- east_midlands,
      -- west_midlands,
      -- east_of_england,
      -- london,
      -- south_east,
      -- south_west );
   type Region_Type is (   
      inner_london,
      outer_london,
      r_of_south_east,
      south_west,
      east_anglia,
      east_midlands,
      west_midlands_conurbation,
      r_of_west_midlands,
      greater_manchester,
      merseyside,
      r_of_north_west,
      south_yorkshire,
      west_yorkshire,
      r_of_yorks_and_humberside,
      tyne_and_wear,
      r_of_north,
      wales,
      scotland,
      northern_ireland );
     
   type Help_Needed_Type is (
      by_self,
      with_help_from_someone_else,
      not_at_all );

      
   type Difficulty_Type is (   
      very_easy,
      fairly_easy,
      fairly_difficult,
      very_difficult );
      
      
   type Task_Type is (
      manage_stairs,
      get_around_house,
      get_in_or_out_of_bed,
      cut_toenails,
      bathing_or_showering,
      walk_down_road );
   
   type Employment_Status_Type is (
      na,
      in_paid_employ,
      self_employed,
      unemployed,
      retired,
      family_care,
      ft_student,
      long_term_sick_or_disabled,
      on_matern_leave,
      govt_trng_scheme,
      something_else );

   type Change_Direction_Type is ( worsening, improvement );
  
   type Qualification_Type is (  -- Qfedhi_Type
      na,
      higher_degree,
      first_degree,
      teaching_qf,
      other_higher_qf,
      nursing_qf,
      gce_a_levels,
      gce_o_levels_or_equiv,
      commercial_qf_no_o_levels,
      cse_grade_2_5_scot_grade_4_5,
      apprenticeship,
      other_qf,
      no_qf,
      still_at_school_no_qf );
      
      
   type Marital_Status_Type is (  
      na,
      married,
      separated,
      divorced,
      widowed,
      never_married,
      in_a_civil_partnership,
      have_a_dissolved_civil_partnership,
      separated_from_a_civil_partnership,
      surviving_partner_of_a_civil_partnership );
      
      
   package Marital_Status_Type_Package is new T_Utils( 
      T => Marital_Status_Type,
      Amount_Type=>Amount, 
      Rate_Type=>Rate, 
      Counter_Type=>Counter_Type );
   subtype Marital_Status_Type_Array is  Marital_Status_Type_Package.Amount_Array;        


   type Partner_Status_Type is (
      married_and_with_spouse,
      cohabiting,
      neither,
      in_civil_partnership );
      
   type Health_Status_Type is (  
      na,
      excellent,
      good,
      fair,
      poor,
      very_poor );
     
   type Delete_Reason_Type is (
      death,
      separation );
      
   type Care_Type is (
      health_visitor,
      home_help,
      meals_on_wheels,
      social_worker,
      physiotherapist
   );
   
   type Weights_Type is
      ( basic,
        extended_1,
        extended_2 );
        
   package Weights_Package is new T_Utils( 
      T => Weights_Type,
      Amount_Type=>Amount, 
      Rate_Type=>Rate, 
      Counter_Type=>Counter_Type );  
   subtype Weights_Array is Weights_Package.Amount_Array;

   subtype Year_Range          is Year_Number range Year_Number'First .. 2200;
   
   type Waves is ( a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq );
   subtype Waves_With_Data is Waves range b .. r;
   subtype Estimated_Data_Waves is Waves range s .. Waves'Last;
   package Waves_Package is new T_Utils( 
      T => Waves,
      Amount_Type=>Amount, 
      Rate_Type=>Rate, 
      Counter_Type=>Counter_Type );
   
   function To_String( wave : Waves ) return String;
      
   subtype Abs_Waves_Array is Waves_Package.Abs_Amount_Array;
   subtype Waves_Array is Waves_Package.Amount_Array;
      
   function Year_From_Wave( wave : Waves ) return Year_Range;
   function Wave_From_Year( year : Year_Range ) return Waves;
   
   subtype Simulation_Waves is Waves range r .. am;
   
   subtype Data_And_Forecast_Years is Year_Number range 1991 .. 2030;
   subtype Simulation_Years is Data_And_Forecast_Years range 2010 .. Data_And_Forecast_Years'Last;

   type Forecast_Element is (
         obr_earnings, -- obr_earnings : OBR earnings measure
         rpi, -- chaw : RPI: All items retail prices index (January 1987=100)
         rpix, -- chmk : RPI: All items excluding mortgage interest (RPIX) (Jan 1987=100)
         cpi, -- d7bt : CPI INDEX 00 : ALL ITEMS- estimated pre-97 2005=100
         producer_output_prices, -- jvz8 : PPI:7200700010:Net Sector Output - All Manufacturing excluding duty
         government_consumption, -- nmrp : General Government: Final consumption expenditure (P3): CPSA
         fixed_investment, -- npqt : Total Gross Fixed Capital Formation CVM SA �m
         business_investment, -- npel : Gross Fixed Capital Formation: Business Investment: CVM SA: �m
         private_dwellings, -- dfea : Gross Fixed Capital Formation:Private Sector:New Dwellings, Excl Land CVM SA
         general_government, -- dlwf : Gross Fixed Capital Formation:Total General Government P51:CVM SA: �m
         net_acquisition_of_valuables, -- npjr : Acquisitions less disposals of valuables: total economy CVM SA, �m
         change_in_inventories, -- cafu : Changes in inventories including alignment adjustment - CVM NAYear SA
         exports, -- ikbk : Balance of Payments: Trade in Goods & Services: Total exports: CVM SA
         total_final_expenditure, -- abmg : Total gross final expenditure (aligned) - P.3+P.5+P.6 : CVM SA
         imports, -- ikbl : Balance of Payments: Imports: Total Trade in Goods & Services: CVM SA
         statistical_discrepancy, -- gixs : Difference between GDP(Expenditure) and GDP(Average) MP(CVM SA)
         gdp_at_market_prices, -- abmi : Gross Domestic Product: chained volume measures: Seasonally adjusted
         private_consumption_nominal, -- rpqm : Household Expenditure: Total.Hhld & NPISH Final Consumption Expenditure CP SA: �
         government_consumption_nominal, -- nmrp : General Government: Final consumption expenditure (P3): CPSA
         fixed_investment_nominal, -- npqs : Total Gross Fixed Capital Formation CP SA �m
         net_acquisition_of_valuables_nominal, -- npjq : Acquisitions less disposals of valuables: total economy CP SA, �m
         change_in_inventories_nominal, -- caex : Changes in inventories (CP SA) including alignment adjustment
         exports_nominal, -- ikbh : Balance of Payments: Trade in Goods & Services: Total exports: CP SA
         total_final_expenditure_nominal, -- abmf : Total gross final expenditure (aligned) - P.3+P.5+P.6 : CP SA
         imports_nominal, -- ikbi : Balance of Payments: Trade in Goods & Services: Total imports: CP SA
         statistical_discrepancy_nominal, -- gixm : Difference between GDP(E) and GDP(A) MP (SA CP)
         gdp_at_market_prices_nominal, -- ybha : Gross Domestic Product at market prices: Current price: Seasonally adjusted
         gross_national_income_nominal, -- abmz : Gross National Income: Current price: Seasonally adjusted
         employment, -- mgrz : LFS: In employment: UK: All: Aged 16+: 000s:SA: Annual = 4 quarter average
         employment_rate, -- mgsc : LFS: Unemployed: UK: All: Aged 16+: 000s: SA: Annual = 4 quarter average
         ilo_unemployment, -- mgwg : LFS: Economic activity rate: UK: All: All aged 16 and over: %: SA
         ilo_unemployment_rate, -- bcjd : Total Claimant count SA (UK) - thousands
         participation_rate, -- mgrz : LFS: In employment: UK: All: Aged 16+: 000s:SA: Annual = 4 quarter average
         claimant_count, -- ybuv : LFS: Avg actual weekly hours of work: UK: All workers in main & 2nd job: SA
         trend_employment_rate -- ybus : LFS: Total actual weekly hours worked (millions): UK: All: SA
   );
   
   type Uprate_Targets is (
      upr_housing_costs,
      upr_wealth,
      upr_state_benefits,
      upr_earnings,
      upr_other_income,
      upr_care_costs_1,
      upr_care_costs_2,
      upr_care_costs_3
   );
   
   type Which_Uprate_Index is (                                       
      uprate_none,
      uprate_rpi,
      uprate_rpix,
      uprate_earnings,
      uprate_cpi,
      uprate_gdp );
      
   
   type Age_Band_Type is (
      age_0_15, 
      age_16_18, 
      age_19_21, 
      age_22_39, 
      age_40_44,
      age_45_49, 
      age_50_54, 
      age_55_59, 
      age_60_64,     
      age_65_69,
      age_70_74, 
      age_75_79, 
      age_80_84, 
      age_85_89, 
      age_90_and_over );

   
   subtype Adult_Age_Band is Age_Band_Type range age_16_18 .. Age_Band_Type'Last;
   package Age_Band_Package is new T_Utils( 
      T => Age_Band_Type,
      Amount_Type=>Amount, 
      Rate_Type=>Rate, 
      Counter_Type=>Counter_Type );
   subtype Age_Band_Array is  Age_Band_Package.Amount_Array;        

   type Summary_Items_Type is (
      population,      
      
      male_60_plus_population,
      female_60_plus_population,
      
      age60_plus_population,
      age65_plus_population,
      age70_plus_population,
      age80_plus_population,
      
      
      social_care_clients,
      
      residential_clients,
      non_residential_clients,
      
      residential_fully_publicly_funded_clients,
      residential_partially_publicly_funded_clients,
      residential_privately_funded_clients,
      
      non_residential_fully_publicly_funded_clients,
      non_residential_partially_publicly_funded_clients,
      non_residential_privately_funded_clients,
      
      residential_private_funding,
      residential_public_funding,
      
      non_residential_private_funding,
      non_residential_public_funding,
      
      dla_care_recipients,
      dla_mob_recipients,
      aa_recipients,
      dla_mob_cost,
      dla_care_cost,
      pension_recipients,
      pension_cost,
      pc_recipients,
      pc_cost,
      aa_cost,
      
      
      public_expenditure_net_of_user_charges_and_other_income,
      private_spend_on_social_care,
      
      net_income,
      disposable_income,
      
      combined_benefit_spend,
      combined_benefit_recipients,
      health_poor_or_very_poor,
      health_poor,
      health_very_poor
       );
      
   function Pretty_Print( t : Summary_Items_Type ) return String;    
       
   package Summary_Items_Package is new T_Utils(  
      Rate_Type    => Rate, 
      Amount_Type  => Amount, 
      Counter_Type => Counter_Type, 
      T            => Summary_Items_Type );
      
   subtype Summary_Items_Array is Summary_Items_Package.Amount_Array;  
   
   type Summary_Items_Group is (
      population,           
      health,         
      care_clients,    
      care_costs,      
      benefits,      
      net_expenditure,
      incomes );
      
   function Group_From_Item( item : Summary_Items_Type ) return Summary_Items_Group;
   
   type High_Middle_Low_Nil_Type is ( high, middle, low, nil );
   subtype High_Middle_Low_Type is High_Middle_Low_Nil_Type range high .. low;
   
   type Means_Test_Type is (
      guaranteed_pension_credit,
      savings_credit,
      savings_credit_qualifying_income );
   
   package High_Middle_Low_Package is new T_Utils(  
      Rate_Type    => Rate, 
      Amount_Type  => Amount, 
      Counter_Type => Counter_Type, 
      T            => High_Middle_Low_Nil_Type );
      
   subtype High_Middle_Low_Array is High_Middle_Low_Package.Amount_Array;  
   
   type High_Low_Nil_Type is ( high, low, nil );

   subtype High_Low_Type is High_Low_Nil_Type range high .. low;

   package High_Low_Package is new T_Utils(  
      Rate_Type    => Rate, 
      Amount_Type  => Amount, 
      Counter_Type => Counter_Type, 
      T            => High_Low_Nil_Type );
      
   subtype High_Low_Array is High_Low_Package.Amount_Array;  
   
  
   type Probit_Threshold_Type is (
      dies_this_period,                    --  0
      has_wealth,                          --  1
      informal_care_from_household_member, --  2
      informal_care_from_non_householder,  --  3
      hh_split,                            --  4
      health_better,                       --  5
      health_worse,                        --  6
      adl_improve,                         --  7
      adl_worsen,                          --  8
      retire,                              --  9
      to_care,                             -- 10
      receiving_aa,                        -- 11
      receiving_dla,                       -- 12
      private_care_demand );               -- 13
      
   type Probit_Thresholds_Array is array( Probit_Threshold_Type ) of Probability;    
      
   type Languages is ( en ); -- needed for front end
   
   type Personal_Results_Location is record
      hid : Sernum_Value;
      pid : Sernum_Value;
      wave : Waves;
      iteration : Positive;
      sysno : Positive;
   end record;
   
   package Personal_Results_Location_List_Package is new Ada.Containers.Vectors
      (Element_Type => Personal_Results_Location,
      Index_Type => Positive );
   subtype Personal_Results_Location_List is Personal_Results_Location_List_Package.Vector;
   
   type Type_Of_Uprating is ( no_uprating, annual_increment, uprate_from_base_data );
   
end WSC_Enums;
