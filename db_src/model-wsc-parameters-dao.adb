with Ada.Text_IO;
with Line_Extractor;
with Text_Utils;
with Ada.Strings.Unbounded;
with Key_Value_DAO;
with GNATColl.Traces;

package body Model.WSC.Parameters.DAO is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Text_Utils;
   
   function MK( year : Simulation_Years; key : String ) return Unbounded_String is
   begin
      return TuS( Line_Extractor.Make_Key_With_Year( "y", year, key ));
   end MK;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.PARAMETERS.DAO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
   procedure Write( r : Run; start_year : Year_Number; end_year : Year_Number; sys : Parameters_Array ) is
   begin
      for year in start_year .. end_year loop
         Key_Value_DAO.Write( r,  MK( year, "benefits.state_pension.age_men" ), sys( year ).benefits.state_pension.age_men );
         Key_Value_DAO.Write( r,  MK( year, "benefits.state_pension.age_women" ), sys( year ).benefits.state_pension.age_women );
         Key_Value_DAO.Write( r,  MK( year, "benefits.state_pension.citizens_pension" ), sys( year ).benefits.state_pension.citizens_pension );
         Key_Value_DAO.Write( r,  MK( year, "benefits.state_pension.class_a" ), sys( year ).benefits.state_pension.class_a );
         Key_Value_DAO.Write( r,  MK( year, "benefits.state_pension.preserve_for_existing_claimants" ), sys( year ).benefits.state_pension.preserve_for_existing_claimants );
         
         Key_Value_DAO.Write( r,  MK( year, "benefits.pension_credit.guaranteed_credit.single" ), sys( year ).benefits.pension_credit.guaranteed_credit.single );
         Key_Value_DAO.Write( r,  MK( year, "benefits.pension_credit.guaranteed_credit.couple" ), sys( year ).benefits.pension_credit.guaranteed_credit.couple );
         Key_Value_DAO.Write( r,  MK( year, "benefits.pension_credit.guaranteed_credit.carer_single" ), sys( year ).benefits.pension_credit.guaranteed_credit.carer_single );
         Key_Value_DAO.Write( r,  MK( year, "benefits.pension_credit.guaranteed_credit.severe_disability_single" ), sys( year ).benefits.pension_credit.guaranteed_credit.severe_disability_single );
         Key_Value_DAO.Write( r,  MK( year, "benefits.pension_credit.guaranteed_credit.severe_disability_couple" ), sys( year ).benefits.pension_credit.guaranteed_credit.severe_disability_couple );
         Key_Value_DAO.Write( r,  MK( year, "benefits.pension_credit.guaranteed_credit.preserve_for_existing_claimants" ), sys( year ).benefits.pension_credit.guaranteed_credit.preserve_for_existing_claimants );

         Key_Value_DAO.Write( r,  MK( year, "benefits.pension_credit.savings_credit.threshold_single" ), sys( year ).benefits.pension_credit.savings_credit.threshold_single );
         Key_Value_DAO.Write( r,  MK( year, "benefits.pension_credit.savings_credit.threshold_couple" ), sys( year ).benefits.pension_credit.savings_credit.threshold_couple );
         Key_Value_DAO.Write( r,  MK( year, "benefits.pension_credit.savings_credit.maximum_single" ), sys( year ).benefits.pension_credit.savings_credit.maximum_single );
         Key_Value_DAO.Write( r,  MK( year, "benefits.pension_credit.savings_credit.maximum_couple" ), sys( year ).benefits.pension_credit.savings_credit.maximum_couple );
         Key_Value_DAO.Write( r,  MK( year, "benefits.pension_credit.savings_credit.withdrawal_rate" ), sys( year ).benefits.pension_credit.savings_credit.withdrawal_rate );
         Key_Value_DAO.Write( r,  MK( year, "benefits.pension_credit.savings_credit.preserve_for_existing_claimants" ), sys( year ).benefits.pension_credit.savings_credit.preserve_for_existing_claimants );
         -- FIXME ADD DISREGARDS, CAPITAL FOR PC
         
         Key_Value_DAO.Write( r,  MK( year, "benefits.attendance_allowance.low_age" ), sys( year ).benefits.attendance_allowance.low_age );
         Key_Value_DAO.Write( r,  MK( year, "benefits.attendance_allowance.high_age" ), sys( year ).benefits.attendance_allowance.high_age );
         Key_Value_DAO.Write( r,  MK( year, "benefits.attendance_allowance.high_rate" ), sys( year ).benefits.attendance_allowance.benefit_rate( high ));
         Key_Value_DAO.Write( r,  MK( year, "benefits.attendance_allowance.low_rate" ), sys( year ).benefits.attendance_allowance.benefit_rate( low ) );
         Key_Value_DAO.Write( r,  MK( year, "benefits.attendance_allowance.test_generosity" ), sys( year ).benefits.attendance_allowance.test_generosity );
         Key_Value_DAO.Write( r,  MK( year, "benefits.attendance_allowance.preserve_for_existing_claimants" ), sys( year ).benefits.attendance_allowance.preserve_for_existing_claimants );
         
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.dont_pay_for_residential_claimants" ), sys( year ).benefits.dla.dont_pay_for_residential_claimants );
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.mobility.low_age" ), sys( year ).benefits.dla.mobility.low_age );
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.mobility.high_age" ), sys( year ).benefits.dla.mobility.high_age );
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.mobility.high_rate" ), sys( year ).benefits.dla.mobility.benefit_rate( high ));
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.mobility.low_rate" ), sys( year ).benefits.dla.mobility.benefit_rate( low ) );
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.mobility.test_generosity" ), sys( year ).benefits.dla.mobility.test_generosity );
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.mobility.preserve_for_existing_claimants" ), sys( year ).benefits.dla.mobility.preserve_for_existing_claimants );
         
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.care.low_age" ), sys( year ).benefits.dla.care.low_age );
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.care.high_age" ), sys( year ).benefits.dla.care.high_age );
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.care.high_rate" ), sys( year ).benefits.dla.care.benefit_rate( high ));
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.care.middle_rate" ), sys( year ).benefits.dla.care.benefit_rate( middle ));
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.care.low_rate" ), sys( year ).benefits.dla.care.benefit_rate( low ));
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.care.test_generosity" ), sys( year ).benefits.dla.care.test_generosity );
         Key_Value_DAO.Write( r,  MK( year, "benefits.dla.care.preserve_for_existing_claimants" ), sys( year ).benefits.dla.care.preserve_for_existing_claimants );
         
         Key_Value_DAO.Write( r,  MK( year, "social_care.needs_assessment_rules.uap_category_critical" ), sys( year ).social_care.needs_assessment_rules.uap_category( critical ));
         Key_Value_DAO.Write( r,  MK( year, "social_care.needs_assessment_rules.uap_category_substantial" ), sys( year ).social_care.needs_assessment_rules.uap_category( substantial ));
         Key_Value_DAO.Write( r,  MK( year, "social_care.needs_assessment_rules.uap_category_moderate" ), sys( year ).social_care.needs_assessment_rules.uap_category( moderate ));
         Key_Value_DAO.Write( r,  MK( year, "social_care.needs_assessment_rules.uap_category_low" ), sys( year ).social_care.needs_assessment_rules.uap_category( low ));
         
         Key_Value_DAO.Write( r,  MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_critical" ), sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( critical ));
         Key_Value_DAO.Write( r,  MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_substantial" ), sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( substantial ));
         Key_Value_DAO.Write( r,  MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_moderate" ), sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( moderate ));
         Key_Value_DAO.Write( r,  MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_low" ), sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( low ));
         
         Key_Value_DAO.Write( r,  MK( year, "social_care.needs_assessment_rules.use_carer_blind_system" ), sys( year ).social_care.needs_assessment_rules.use_carer_blind_system );
         
         Key_Value_DAO.Write( r,  MK( year, "social_care.needs_assessment_rules.carer_blind_degree" ), sys( year ).social_care.needs_assessment_rules.carer_blind_degree );
         
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.residential.assets.include_property" ), sys( year ).social_care.means_test.residential.assets.include_property );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.residential.assets.upper_limit" ), sys( year ).social_care.means_test.residential.assets.upper_limit );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.residential.assets.lower_limit" ), sys( year ).social_care.means_test.residential.assets.lower_limit );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.residential.assets.taper" ), sys( year ).social_care.means_test.residential.assets.taper );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.residential.assets.abolish" ), sys( year ).social_care.means_test.residential.assets.abolish );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.residential.income.floor" ), sys( year ).social_care.means_test.residential.income.floor );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.residential.income.minimum_support_level" ), sys( year ).social_care.means_test.residential.income.minimum_support_level );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.residential.income.percent_costs_met" ), sys( year ).social_care.means_test.residential.income.percent_costs_met );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.residential.income.abolish" ), sys( year ).social_care.means_test.residential.income.abolish );
         
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.non_residential.assets.include_property" ), sys( year ).social_care.means_test.non_residential.assets.include_property );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.non_residential.assets.upper_limit" ), sys( year ).social_care.means_test.non_residential.assets.upper_limit );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.non_residential.assets.lower_limit" ), sys( year ).social_care.means_test.non_residential.assets.lower_limit );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.non_residential.assets.taper" ), sys( year ).social_care.means_test.non_residential.assets.taper );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.non_residential.assets.abolish" ), sys( year ).social_care.means_test.non_residential.assets.abolish );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.non_residential.income.floor" ), sys( year ).social_care.means_test.non_residential.income.floor );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.non_residential.income.minimum_support_level" ), sys( year ).social_care.means_test.non_residential.income.minimum_support_level );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.non_residential.income.percent_costs_met" ), sys( year ).social_care.means_test.non_residential.income.percent_costs_met );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.non_residential.income.abolish" ), sys( year ).social_care.means_test.non_residential.income.abolish );

         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.maxima.maximum_weekly_charge_residential" ), sys( year ).social_care.means_test.maxima.maximum_weekly_charge_residential );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.maxima.maximum_weekly_charge_non_residential" ), sys( year ).social_care.means_test.maxima.maximum_weekly_charge_non_residential );
         Key_Value_DAO.Write( r,  MK( year, "social_care.means_test.maxima.maximum_lifetime_payment" ), sys( year ).social_care.means_test.maxima.maximum_lifetime_payment );
         
         Key_Value_DAO.Write( r,  MK( year, "social_care.options.abolish" ), sys( year ).social_care.options.abolish );
         Key_Value_DAO.Write( r,  MK( year, "social_care.options.preserve_for_existing_claimants" ), sys( year ).social_care.options.preserve_for_existing_claimants );
         
         Key_Value_DAO.Write( r,  MK( year, "social_care.insurance.actuarially_fair" ), sys( year ).social_care.insurance.actuarially_fair );
         Key_Value_DAO.Write( r,  MK( year, "social_care.insurance.contribution" ), Censor_String( Contribution_Type'Image( sys( year ).social_care.insurance.contribution )));
         Key_Value_DAO.Write( r,  MK( year, "av_costs.hour_of_care" ), sys( year ).av_costs.hour_of_care );
         Key_Value_DAO.Write( r,  MK( year, "av_costs.residential_per_week" ), sys( year ).av_costs.residential_per_week );
      end loop;      
   end Write;
   
   function Read( r : Run; start_year : Year_Number; end_year : Year_Number ) return Parameters_Array is
      sys : Parameters_Array;
   begin
      Log( "Parameters.DAO : Read opening; run_id = " & r.run_id'Img );
      for year in start_year .. end_year loop
         Log( "on year " & year'Img );
         sys( year ).benefits.state_pension.age_men  := Key_Value_DAO.Read( r,  MK( year, "benefits.state_pension.age_men" ));
         Log( "got benefits.state_pension.age_men as " & sys( year ).benefits.state_pension.age_men'Img );
         sys( year ).benefits.state_pension.age_women  := Key_Value_DAO.Read( r,  MK( year, "benefits.state_pension.age_women" ));
         Log( "got benefits.state_pension.age_women as " & sys( year ).benefits.state_pension.age_women'Img );
         sys( year ).benefits.state_pension.citizens_pension  := Key_Value_DAO.Read( r,  MK( year, "benefits.state_pension.citizens_pension" ));
         sys( year ).benefits.state_pension.class_a  := Key_Value_DAO.Read( r,  MK( year, "benefits.state_pension.class_a" ));
         sys( year ).benefits.state_pension.preserve_for_existing_claimants  := Key_Value_DAO.Read( r,  MK( year, "benefits.state_pension.preserve_for_existing_claimants" ));
         Log( "Pension OK" );
         sys( year ).benefits.pension_credit.guaranteed_credit.single  := Key_Value_DAO.Read( r,  MK( year, "benefits.pension_credit.guaranteed_credit.single" ));
         sys( year ).benefits.pension_credit.guaranteed_credit.couple  := Key_Value_DAO.Read( r,  MK( year, "benefits.pension_credit.guaranteed_credit.couple" ));
         sys( year ).benefits.pension_credit.guaranteed_credit.carer_single  := Key_Value_DAO.Read( r,  MK( year, "benefits.pension_credit.guaranteed_credit.carer_single" ));
         sys( year ).benefits.pension_credit.guaranteed_credit.severe_disability_single  := Key_Value_DAO.Read( r,  MK( year, "benefits.pension_credit.guaranteed_credit.severe_disability_single" ));
         sys( year ).benefits.pension_credit.guaranteed_credit.severe_disability_couple  := Key_Value_DAO.Read( r,  MK( year, "benefits.pension_credit.guaranteed_credit.severe_disability_couple" ));
         sys( year ).benefits.pension_credit.guaranteed_credit.preserve_for_existing_claimants  := Key_Value_DAO.Read( r,  MK( year, "benefits.pension_credit.guaranteed_credit.preserve_for_existing_claimants" ));
         Log( "Pension Credit OK" );
         
         sys( year ).benefits.pension_credit.savings_credit.threshold_single  := Key_Value_DAO.Read( r,  MK( year, "benefits.pension_credit.savings_credit.threshold_single" ));
         sys( year ).benefits.pension_credit.savings_credit.threshold_couple  := Key_Value_DAO.Read( r,  MK( year, "benefits.pension_credit.savings_credit.threshold_couple" ));
         sys( year ).benefits.pension_credit.savings_credit.maximum_single  := Key_Value_DAO.Read( r,  MK( year, "benefits.pension_credit.savings_credit.maximum_single" ));
         sys( year ).benefits.pension_credit.savings_credit.maximum_couple  := Key_Value_DAO.Read( r,  MK( year, "benefits.pension_credit.savings_credit.maximum_couple" ));
         sys( year ).benefits.pension_credit.savings_credit.withdrawal_rate  := Key_Value_DAO.Read( r,  MK( year, "benefits.pension_credit.savings_credit.withdrawal_rate" ));
         sys( year ).benefits.pension_credit.savings_credit.preserve_for_existing_claimants  := Key_Value_DAO.Read( r,  MK( year, "benefits.pension_credit.savings_credit.preserve_for_existing_claimants" ));
         Log( "Pension Credit Savings OK" );

         sys( year ).benefits.attendance_allowance.low_age  := Key_Value_DAO.Read( r,  MK( year, "benefits.attendance_allowance.low_age" ));
         sys( year ).benefits.attendance_allowance.high_age  := Key_Value_DAO.Read( r,  MK( year, "benefits.attendance_allowance.high_age" ));
         sys( year ).benefits.attendance_allowance.benefit_rate( high )  := Key_Value_DAO.Read( r,  MK( year, "benefits.attendance_allowance.high_rate" ));
         sys( year ).benefits.attendance_allowance.benefit_rate( low )  := Key_Value_DAO.Read( r,  MK( year, "benefits.attendance_allowance.low_rate" ));
         sys( year ).benefits.attendance_allowance.test_generosity  := Key_Value_DAO.Read( r,  MK( year, "benefits.attendance_allowance.test_generosity" ));
         sys( year ).benefits.attendance_allowance.preserve_for_existing_claimants  := Key_Value_DAO.Read( r,  MK( year, "benefits.attendance_allowance.preserve_for_existing_claimants" ));
         Log( "AA OK" );

         sys( year ).benefits.dla.dont_pay_for_residential_claimants  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.dont_pay_for_residential_claimants" ));
         sys( year ).benefits.dla.mobility.low_age  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.mobility.low_age" ));
         sys( year ).benefits.dla.mobility.high_age  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.mobility.high_age" ));
         sys( year ).benefits.dla.mobility.benefit_rate( high )  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.mobility.high_rate" ));
         sys( year ).benefits.dla.mobility.benefit_rate( low )  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.mobility.low_rate" ));
         sys( year ).benefits.dla.mobility.test_generosity  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.mobility.test_generosity" ));
         sys( year ).benefits.dla.mobility.preserve_for_existing_claimants  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.mobility.preserve_for_existing_claimants" ));
         Log( "DLA Mob OK" );

         sys( year ).benefits.dla.care.low_age  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.care.low_age" ));
         sys( year ).benefits.dla.care.high_age  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.care.high_age" ));
         sys( year ).benefits.dla.care.benefit_rate( high )  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.care.high_rate" ));
         sys( year ).benefits.dla.care.benefit_rate( middle )  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.care.middle_rate" ));
         sys( year ).benefits.dla.care.benefit_rate( low )  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.care.low_rate" ));
         sys( year ).benefits.dla.care.test_generosity  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.care.test_generosity" ));
         sys( year ).benefits.dla.care.preserve_for_existing_claimants  := Key_Value_DAO.Read( r,  MK( year, "benefits.dla.care.preserve_for_existing_claimants" ));
         Log( "DLA Care OK" );

         sys( year ).social_care.needs_assessment_rules.uap_category( critical ) := Key_Value_DAO.Read( r,  MK( year, "social_care.needs_assessment_rules.uap_category_critical" ));
         sys( year ).social_care.needs_assessment_rules.uap_category( substantial ) := Key_Value_DAO.Read( r,  MK( year, "social_care.needs_assessment_rules.uap_category_substantial" ));
         sys( year ).social_care.needs_assessment_rules.uap_category( moderate ) := Key_Value_DAO.Read( r,  MK( year, "social_care.needs_assessment_rules.uap_category_moderate" ));
         sys( year ).social_care.needs_assessment_rules.uap_category( low ) := Key_Value_DAO.Read( r,  MK( year, "social_care.needs_assessment_rules.uap_category_low" ));
         sys( year ).social_care.needs_assessment_rules.uap_category( none ) := 100.0 -
            sys( year ).social_care.needs_assessment_rules.uap_category( critical ) -
            sys( year ).social_care.needs_assessment_rules.uap_category( substantial ) -
            sys( year ).social_care.needs_assessment_rules.uap_category( moderate ) -
            sys( year ).social_care.needs_assessment_rules.uap_category( low );
         
         sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( critical ) := Key_Value_DAO.Read( r,  MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_critical" ));
         sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( substantial ) := Key_Value_DAO.Read( r,  MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_substantial" ));
         sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( moderate ) := Key_Value_DAO.Read( r,  MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_moderate" ));
         sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( low ) := Key_Value_DAO.Read( r,  MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_low" ));
         sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( none ) := 0.0;

         sys( year ).social_care.needs_assessment_rules.use_carer_blind_system  := Key_Value_DAO.Read( r,  MK( year, "social_care.needs_assessment_rules.use_carer_blind_system" ));
         sys( year ).social_care.needs_assessment_rules.carer_blind_degree  := Key_Value_DAO.Read( r,  MK( year, "social_care.needs_assessment_rules.carer_blind_degree" ));
         Log( "Social Care Needs OK" );

         sys( year ).social_care.means_test.residential.assets.include_property  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.residential.assets.include_property" ));
         sys( year ).social_care.means_test.residential.assets.upper_limit  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.residential.assets.upper_limit" ));
         sys( year ).social_care.means_test.residential.assets.lower_limit  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.residential.assets.lower_limit" ));
         sys( year ).social_care.means_test.residential.assets.taper  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.residential.assets.taper" ));
         sys( year ).social_care.means_test.residential.assets.abolish  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.residential.assets.abolish" ));
         sys( year ).social_care.means_test.residential.income.floor  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.residential.income.floor" ));
         sys( year ).social_care.means_test.residential.income.minimum_support_level  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.residential.income.minimum_support_level" ));
         sys( year ).social_care.means_test.residential.income.percent_costs_met  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.residential.income.percent_costs_met" ));
         sys( year ).social_care.means_test.residential.income.abolish  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.residential.income.abolish" ));
         Log( "Social Residential MT OK" );
         
         sys( year ).social_care.means_test.non_residential.assets.include_property  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.non_residential.assets.include_property" ));
         sys( year ).social_care.means_test.non_residential.assets.upper_limit  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.non_residential.assets.upper_limit" ));
         sys( year ).social_care.means_test.non_residential.assets.lower_limit  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.non_residential.assets.lower_limit" ));
         sys( year ).social_care.means_test.non_residential.assets.taper  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.non_residential.assets.taper" ));
         sys( year ).social_care.means_test.non_residential.assets.abolish  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.non_residential.assets.abolish" ));
         sys( year ).social_care.means_test.non_residential.income.floor  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.non_residential.income.floor" ));
         sys( year ).social_care.means_test.non_residential.income.minimum_support_level  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.non_residential.income.minimum_support_level" ));
         sys( year ).social_care.means_test.non_residential.income.percent_costs_met  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.non_residential.income.percent_costs_met" ));
         sys( year ).social_care.means_test.non_residential.income.abolish  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.non_residential.income.abolish" ));

         sys( year ).social_care.means_test.maxima.maximum_weekly_charge_residential  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.maxima.maximum_weekly_charge_residential" ));
         sys( year ).social_care.means_test.maxima.maximum_weekly_charge_non_residential  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.maxima.maximum_weekly_charge_non_residential" ));
         sys( year ).social_care.means_test.maxima.maximum_lifetime_payment  := Key_Value_DAO.Read( r,  MK( year, "social_care.means_test.maxima.maximum_lifetime_payment" ));
         Log( "Social Non Residential MT OK" );
         sys( year ).social_care.options.abolish  := Key_Value_DAO.Read( r,  MK( year, "social_care.options.abolish" ));
         sys( year ).social_care.options.preserve_for_existing_claimants  := Key_Value_DAO.Read( r,  MK( year, "social_care.options.preserve_for_existing_claimants" ));
         
         sys( year ).social_care.insurance.actuarially_fair  := Key_Value_DAO.Read( r,  MK( year, "social_care.insurance.actuarially_fair" ));
         sys( year ).social_care.insurance.contribution := Contribution_Type'Value( Key_Value_DAO.Read( r,  MK( year, "social_care.insurance.contribution" )));

         sys( year ).av_costs.hour_of_care  := Key_Value_DAO.Read( r,  MK( year, "av_costs.hour_of_care" ));
         sys( year ).av_costs.residential_per_week :=  Key_Value_DAO.Read( r,  MK( year, "av_costs.residential_per_week" ));
      end loop;      
      Log( "Parameters.IO : Read OK" );
      return sys;
   end Read;
   
end Model.WSC.Parameters.DAO;
