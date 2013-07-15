with Ada.Text_IO;
with Line_Extractor;
with Text_Utils;
with Keyed_Text_Buffer;

package body Model.WSC.Parameters.IO is
   
   function MK( year : Simulation_Years; key : String ) return String is
   begin
      return Line_Extractor.Make_Key_With_Year( "y", year, key );
   end MK;
   
   function To_Keyed_Buffer( sys : Parameters_Array; start_year, end_year : Simulation_Years ) return Keyed_Text_Buffer.Text_Buffer is
      kb : Keyed_Text_Buffer.Text_Buffer;
   begin
      return kb;
   end To_Keyed_Buffer;
   
   procedure Write( sys : Parameters_Array; filename : String; start_year, end_year : Simulation_Years ) is
   use UK_Key_Value_IO;
   use Ada.Text_IO;
   use Text_Utils;
      f : UK_Key_Value_IO.File_Type;
   begin
      Create( f, Out_File, filename );
      for year in start_year .. end_year loop
         Write( f, MK( year, "benefits.state_pension.age_men" ), sys( year ).benefits.state_pension.age_men );
         Write( f, MK( year, "benefits.state_pension.age_women" ), sys( year ).benefits.state_pension.age_women );
         Write( f, MK( year, "benefits.state_pension.citizens_pension" ), sys( year ).benefits.state_pension.citizens_pension );
         Write( f, MK( year, "benefits.state_pension.class_a" ), sys( year ).benefits.state_pension.class_a );
         Write( f, MK( year, "benefits.state_pension.preserve_for_existing_claimants" ), sys( year ).benefits.state_pension.preserve_for_existing_claimants );
         
         Write( f, MK( year, "benefits.pension_credit.guaranteed_credit.single" ), sys( year ).benefits.pension_credit.guaranteed_credit.single );
         Write( f, MK( year, "benefits.pension_credit.guaranteed_credit.couple" ), sys( year ).benefits.pension_credit.guaranteed_credit.couple );
         Write( f, MK( year, "benefits.pension_credit.guaranteed_credit.carer_single" ), sys( year ).benefits.pension_credit.guaranteed_credit.carer_single );
         Write( f, MK( year, "benefits.pension_credit.guaranteed_credit.severe_disability_single" ), sys( year ).benefits.pension_credit.guaranteed_credit.severe_disability_single );
         Write( f, MK( year, "benefits.pension_credit.guaranteed_credit.severe_disability_couple" ), sys( year ).benefits.pension_credit.guaranteed_credit.severe_disability_couple );
         Write( f, MK( year, "benefits.pension_credit.guaranteed_credit.preserve_for_existing_claimants" ), sys( year ).benefits.pension_credit.guaranteed_credit.preserve_for_existing_claimants );

         Write( f, MK( year, "benefits.pension_credit.savings_credit.threshold_single" ), sys( year ).benefits.pension_credit.savings_credit.threshold_single );
         Write( f, MK( year, "benefits.pension_credit.savings_credit.threshold_couple" ), sys( year ).benefits.pension_credit.savings_credit.threshold_couple );
         Write( f, MK( year, "benefits.pension_credit.savings_credit.maximum_single" ), sys( year ).benefits.pension_credit.savings_credit.maximum_single );
         Write( f, MK( year, "benefits.pension_credit.savings_credit.maximum_couple" ), sys( year ).benefits.pension_credit.savings_credit.maximum_couple );
         Write( f, MK( year, "benefits.pension_credit.savings_credit.withdrawal_rate" ), sys( year ).benefits.pension_credit.savings_credit.withdrawal_rate );
         Write( f, MK( year, "benefits.pension_credit.savings_credit.preserve_for_existing_claimants" ), sys( year ).benefits.pension_credit.savings_credit.preserve_for_existing_claimants );
         -- FIXME ADD DISREGARDS, CAPITAL FOR PC
         
         Write( f, MK( year, "benefits.attendance_allowance.low_age" ), sys( year ).benefits.attendance_allowance.low_age );
         Write( f, MK( year, "benefits.attendance_allowance.high_age" ), sys( year ).benefits.attendance_allowance.high_age );
         Write( f, MK( year, "benefits.attendance_allowance.high_rate" ), sys( year ).benefits.attendance_allowance.benefit_rate( high ));
         Write( f, MK( year, "benefits.attendance_allowance.low_rate" ), sys( year ).benefits.attendance_allowance.benefit_rate( low ) );
         Write( f, MK( year, "benefits.attendance_allowance.test_generosity" ), sys( year ).benefits.attendance_allowance.test_generosity );
         Write( f, MK( year, "benefits.attendance_allowance.preserve_for_existing_claimants" ), sys( year ).benefits.attendance_allowance.preserve_for_existing_claimants );
         
         Write( f, MK( year, "benefits.dla.dont_pay_for_residential_claimants" ), sys( year ).benefits.dla.dont_pay_for_residential_claimants );
         Write( f, MK( year, "benefits.dla.mobility.low_age" ), sys( year ).benefits.dla.mobility.low_age );
         Write( f, MK( year, "benefits.dla.mobility.high_age" ), sys( year ).benefits.dla.mobility.high_age );
         Write( f, MK( year, "benefits.dla.mobility.high_rate" ), sys( year ).benefits.dla.mobility.benefit_rate( high ));
         Write( f, MK( year, "benefits.dla.mobility.low_rate" ), sys( year ).benefits.dla.mobility.benefit_rate( low ) );
         Write( f, MK( year, "benefits.dla.mobility.test_generosity" ), sys( year ).benefits.dla.mobility.test_generosity );
         Write( f, MK( year, "benefits.dla.mobility.preserve_for_existing_claimants" ), sys( year ).benefits.dla.mobility.preserve_for_existing_claimants );
         
         Write( f, MK( year, "benefits.dla.care.low_age" ), sys( year ).benefits.dla.care.low_age );
         Write( f, MK( year, "benefits.dla.care.high_age" ), sys( year ).benefits.dla.care.high_age );
         Write( f, MK( year, "benefits.dla.care.high_rate" ), sys( year ).benefits.dla.care.benefit_rate( high ));
         Write( f, MK( year, "benefits.dla.care.middle_rate" ), sys( year ).benefits.dla.care.benefit_rate( middle ));
         Write( f, MK( year, "benefits.dla.care.low_rate" ), sys( year ).benefits.dla.care.benefit_rate( low ));
         Write( f, MK( year, "benefits.dla.care.test_generosity" ), sys( year ).benefits.dla.care.test_generosity );
         Write( f, MK( year, "benefits.dla.care.preserve_for_existing_claimants" ), sys( year ).benefits.dla.care.preserve_for_existing_claimants );
         
         Write( f, MK( year, "social_care.needs_assessment_rules.uap_category_critical" ), sys( year ).social_care.needs_assessment_rules.uap_category( critical ));
         Write( f, MK( year, "social_care.needs_assessment_rules.uap_category_substantial" ), sys( year ).social_care.needs_assessment_rules.uap_category( substantial ));
         Write( f, MK( year, "social_care.needs_assessment_rules.uap_category_moderate" ), sys( year ).social_care.needs_assessment_rules.uap_category( moderate ));
         Write( f, MK( year, "social_care.needs_assessment_rules.uap_category_low" ), sys( year ).social_care.needs_assessment_rules.uap_category( low ));
         
         Write( f, MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_critical" ), sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( critical ));
         Write( f, MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_substantial" ), sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( substantial ));
         Write( f, MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_moderate" ), sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( moderate ));
         Write( f, MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_low" ), sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( low ));
         
         Write( f, MK( year, "social_care.needs_assessment_rules.use_carer_blind_system" ), sys( year ).social_care.needs_assessment_rules.use_carer_blind_system );
         
         Write( f, MK( year, "social_care.needs_assessment_rules.carer_blind_degree" ), sys( year ).social_care.needs_assessment_rules.carer_blind_degree );
         
         Write( f, MK( year, "social_care.means_test.residential.assets.include_property" ), sys( year ).social_care.means_test.residential.assets.include_property );
         Write( f, MK( year, "social_care.means_test.residential.assets.upper_limit" ), sys( year ).social_care.means_test.residential.assets.upper_limit );
         Write( f, MK( year, "social_care.means_test.residential.assets.lower_limit" ), sys( year ).social_care.means_test.residential.assets.lower_limit );
         Write( f, MK( year, "social_care.means_test.residential.assets.taper" ), sys( year ).social_care.means_test.residential.assets.taper );
         Write( f, MK( year, "social_care.means_test.residential.assets.abolish" ), sys( year ).social_care.means_test.residential.assets.abolish );
         Write( f, MK( year, "social_care.means_test.residential.income.floor" ), sys( year ).social_care.means_test.residential.income.floor );
         Write( f, MK( year, "social_care.means_test.residential.income.minimum_support_level" ), sys( year ).social_care.means_test.residential.income.minimum_support_level );
         Write( f, MK( year, "social_care.means_test.residential.income.percent_costs_met" ), sys( year ).social_care.means_test.residential.income.percent_costs_met );
         Write( f, MK( year, "social_care.means_test.residential.income.abolish" ), sys( year ).social_care.means_test.residential.income.abolish );
         
         Write( f, MK( year, "social_care.means_test.non_residential.assets.include_property" ), sys( year ).social_care.means_test.non_residential.assets.include_property );
         Write( f, MK( year, "social_care.means_test.non_residential.assets.upper_limit" ), sys( year ).social_care.means_test.non_residential.assets.upper_limit );
         Write( f, MK( year, "social_care.means_test.non_residential.assets.lower_limit" ), sys( year ).social_care.means_test.non_residential.assets.lower_limit );
         Write( f, MK( year, "social_care.means_test.non_residential.assets.taper" ), sys( year ).social_care.means_test.non_residential.assets.taper );
         Write( f, MK( year, "social_care.means_test.non_residential.assets.abolish" ), sys( year ).social_care.means_test.non_residential.assets.abolish );
         Write( f, MK( year, "social_care.means_test.non_residential.income.floor" ), sys( year ).social_care.means_test.non_residential.income.floor );
         Write( f, MK( year, "social_care.means_test.non_residential.income.minimum_support_level" ), sys( year ).social_care.means_test.non_residential.income.minimum_support_level );
         Write( f, MK( year, "social_care.means_test.non_residential.income.percent_costs_met" ), sys( year ).social_care.means_test.non_residential.income.percent_costs_met );
         Write( f, MK( year, "social_care.means_test.non_residential.income.abolish" ), sys( year ).social_care.means_test.non_residential.income.abolish );

         Write( f, MK( year, "social_care.means_test.maxima.maximum_weekly_charge_residential" ), sys( year ).social_care.means_test.maxima.maximum_weekly_charge_residential );
         Write( f, MK( year, "social_care.means_test.maxima.maximum_weekly_charge_non_residential" ), sys( year ).social_care.means_test.maxima.maximum_weekly_charge_non_residential );
         Write( f, MK( year, "social_care.means_test.maxima.maximum_lifetime_payment" ), sys( year ).social_care.means_test.maxima.maximum_lifetime_payment );
         
         Write( f, MK( year, "social_care.options.abolish" ), sys( year ).social_care.options.abolish );
         Write( f, MK( year, "social_care.options.preserve_for_existing_claimants" ), sys( year ).social_care.options.preserve_for_existing_claimants );
         
         Write( f, MK( year, "social_care.insurance.actuarially_fair" ), sys( year ).social_care.insurance.actuarially_fair );
         Write( f, MK( year, "social_care.insurance.contribution" ), Censor_String( Contribution_Type'Image( sys( year ).social_care.insurance.contribution )));
         Write( f, MK( year, "av_costs.hour_of_care" ), sys( year ).av_costs.hour_of_care );
         Write( f, MK( year, "av_costs.residential_per_week" ), sys( year ).av_costs.residential_per_week );
      end loop;      
      Close( f );
   end Write;
   
   function Read( filename : String; start_year, end_year : Simulation_Years ) return Parameters_Array is
   use Ada.Text_IO;
      sys : Parameters_Array;
      buff : Text_Buffer;
   begin
      Put_Line( "Parameters.IO : Read opening " & filename );
      buff := Keyed_Text_Buffer.Load( filename, single_line_delimited );
      Read( buff, start_year, end_year, sys );
      return sys;
   end Read;
   
   procedure Read( buff : Text_Buffer; start_year, end_year : Simulation_Years; sys : in out Parameters_Array ) is
   use UK_Key_Value_IO;
   use Ada.Text_IO;
   begin
      for year in start_year .. end_year loop
         Put_Line( "on year " & year'Img );
         sys( year ).benefits.state_pension.age_men  := Read( buff, MK( year, "benefits.state_pension.age_men" ));
         sys( year ).benefits.state_pension.age_women  := Read( buff, MK( year, "benefits.state_pension.age_women" ));
         sys( year ).benefits.state_pension.citizens_pension  := Read( buff, MK( year, "benefits.state_pension.citizens_pension" ));
         sys( year ).benefits.state_pension.class_a  := Read( buff, MK( year, "benefits.state_pension.class_a" ));
         sys( year ).benefits.state_pension.preserve_for_existing_claimants  := Read( buff, MK( year, "benefits.state_pension.preserve_for_existing_claimants" ));
         Put_Line( "Pension OK" );
         sys( year ).benefits.pension_credit.guaranteed_credit.single  := Read( buff, MK( year, "benefits.pension_credit.guaranteed_credit.single" ));
         sys( year ).benefits.pension_credit.guaranteed_credit.couple  := Read( buff, MK( year, "benefits.pension_credit.guaranteed_credit.couple" ));
         sys( year ).benefits.pension_credit.guaranteed_credit.carer_single  := Read( buff, MK( year, "benefits.pension_credit.guaranteed_credit.carer_single" ));
         sys( year ).benefits.pension_credit.guaranteed_credit.severe_disability_single  := Read( buff, MK( year, "benefits.pension_credit.guaranteed_credit.severe_disability_single" ));
         sys( year ).benefits.pension_credit.guaranteed_credit.severe_disability_couple  := Read( buff, MK( year, "benefits.pension_credit.guaranteed_credit.severe_disability_couple" ));
         sys( year ).benefits.pension_credit.guaranteed_credit.preserve_for_existing_claimants  := Read( buff, MK( year, "benefits.pension_credit.guaranteed_credit.preserve_for_existing_claimants" ));
         Put_Line( "Pension Credit OK" );
         
         sys( year ).benefits.pension_credit.savings_credit.threshold_single  := Read( buff, MK( year, "benefits.pension_credit.savings_credit.threshold_single" ));
         sys( year ).benefits.pension_credit.savings_credit.threshold_couple  := Read( buff, MK( year, "benefits.pension_credit.savings_credit.threshold_couple" ));
         sys( year ).benefits.pension_credit.savings_credit.maximum_single  := Read( buff, MK( year, "benefits.pension_credit.savings_credit.maximum_single" ));
         sys( year ).benefits.pension_credit.savings_credit.maximum_couple  := Read( buff, MK( year, "benefits.pension_credit.savings_credit.maximum_couple" ));
         sys( year ).benefits.pension_credit.savings_credit.withdrawal_rate  := Read( buff, MK( year, "benefits.pension_credit.savings_credit.withdrawal_rate" ));
         sys( year ).benefits.pension_credit.savings_credit.preserve_for_existing_claimants  := Read( buff, MK( year, "benefits.pension_credit.savings_credit.preserve_for_existing_claimants" ));
         Put_Line( "Pension Credit Savings OK" );

         sys( year ).benefits.attendance_allowance.low_age  := Read( buff, MK( year, "benefits.attendance_allowance.low_age" ));
         sys( year ).benefits.attendance_allowance.high_age  := Read( buff, MK( year, "benefits.attendance_allowance.high_age" ));
         sys( year ).benefits.attendance_allowance.benefit_rate( high )  := Read( buff, MK( year, "benefits.attendance_allowance.high_rate" ));
         sys( year ).benefits.attendance_allowance.benefit_rate( low )  := Read( buff, MK( year, "benefits.attendance_allowance.low_rate" ));
         sys( year ).benefits.attendance_allowance.test_generosity  := Read( buff, MK( year, "benefits.attendance_allowance.test_generosity" ));
         sys( year ).benefits.attendance_allowance.preserve_for_existing_claimants  := Read( buff, MK( year, "benefits.attendance_allowance.preserve_for_existing_claimants" ));
         Put_Line( "AA OK" );

         sys( year ).benefits.dla.dont_pay_for_residential_claimants  := Read( buff, MK( year, "benefits.dla.dont_pay_for_residential_claimants" ));
         sys( year ).benefits.dla.mobility.low_age  := Read( buff, MK( year, "benefits.dla.mobility.low_age" ));
         sys( year ).benefits.dla.mobility.high_age  := Read( buff, MK( year, "benefits.dla.mobility.high_age" ));
         sys( year ).benefits.dla.mobility.benefit_rate( high )  := Read( buff, MK( year, "benefits.dla.mobility.high_rate" ));
         sys( year ).benefits.dla.mobility.benefit_rate( low )  := Read( buff, MK( year, "benefits.dla.mobility.low_rate" ));
         sys( year ).benefits.dla.mobility.test_generosity  := Read( buff, MK( year, "benefits.dla.mobility.test_generosity" ));
         sys( year ).benefits.dla.mobility.preserve_for_existing_claimants  := Read( buff, MK( year, "benefits.dla.mobility.preserve_for_existing_claimants" ));
         Put_Line( "DLA Mob OK" );

         sys( year ).benefits.dla.care.low_age  := Read( buff, MK( year, "benefits.dla.care.low_age" ));
         sys( year ).benefits.dla.care.high_age  := Read( buff, MK( year, "benefits.dla.care.high_age" ));
         sys( year ).benefits.dla.care.benefit_rate( high )  := Read( buff, MK( year, "benefits.dla.care.high_rate" ));
         sys( year ).benefits.dla.care.benefit_rate( middle )  := Read( buff, MK( year, "benefits.dla.care.middle_rate" ));
         sys( year ).benefits.dla.care.benefit_rate( low )  := Read( buff, MK( year, "benefits.dla.care.low_rate" ));
         sys( year ).benefits.dla.care.test_generosity  := Read( buff, MK( year, "benefits.dla.care.test_generosity" ));
         sys( year ).benefits.dla.care.preserve_for_existing_claimants  := Read( buff, MK( year, "benefits.dla.care.preserve_for_existing_claimants" ));
         Put_Line( "DLA Care OK" );

         sys( year ).social_care.needs_assessment_rules.uap_category( critical ) := Read( buff, MK( year, "social_care.needs_assessment_rules.uap_category_critical" ));
         sys( year ).social_care.needs_assessment_rules.uap_category( substantial ) := Read( buff, MK( year, "social_care.needs_assessment_rules.uap_category_substantial" ));
         sys( year ).social_care.needs_assessment_rules.uap_category( moderate ) := Read( buff, MK( year, "social_care.needs_assessment_rules.uap_category_moderate" ));
         sys( year ).social_care.needs_assessment_rules.uap_category( low ) := Read( buff, MK( year, "social_care.needs_assessment_rules.uap_category_low" ));
         sys( year ).social_care.needs_assessment_rules.uap_category( none ) := 100.0 -
            sys( year ).social_care.needs_assessment_rules.uap_category( critical ) -
            sys( year ).social_care.needs_assessment_rules.uap_category( substantial ) -
            sys( year ).social_care.needs_assessment_rules.uap_category( moderate ) -
            sys( year ).social_care.needs_assessment_rules.uap_category( low );
         
         sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( critical ) := Read( buff, MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_critical" ));
         sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( substantial ) := Read( buff, MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_substantial" ));
         sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( moderate ) := Read( buff, MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_moderate" ));
         sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( low ) := Read( buff, MK( year, "social_care.needs_assessment_rules.share_of_category_passing_test_low" ));
         sys( year ).social_care.needs_assessment_rules.share_of_category_passing_test( none ) := 0.0;

         sys( year ).social_care.needs_assessment_rules.use_carer_blind_system  := Read( buff, MK( year, "social_care.needs_assessment_rules.use_carer_blind_system" ));
         sys( year ).social_care.needs_assessment_rules.carer_blind_degree  := Read( buff, MK( year, "social_care.needs_assessment_rules.carer_blind_degree" ));
         Put_Line( "Social Care Needs OK" );

         sys( year ).social_care.means_test.residential.assets.include_property  := Read( buff, MK( year, "social_care.means_test.residential.assets.include_property" ));
         sys( year ).social_care.means_test.residential.assets.upper_limit  := Read( buff, MK( year, "social_care.means_test.residential.assets.upper_limit" ));
         sys( year ).social_care.means_test.residential.assets.lower_limit  := Read( buff, MK( year, "social_care.means_test.residential.assets.lower_limit" ));
         sys( year ).social_care.means_test.residential.assets.taper  := Read( buff, MK( year, "social_care.means_test.residential.assets.taper" ));
         sys( year ).social_care.means_test.residential.assets.abolish  := Read( buff, MK( year, "social_care.means_test.residential.assets.abolish" ));
         sys( year ).social_care.means_test.residential.income.floor  := Read( buff, MK( year, "social_care.means_test.residential.income.floor" ));
         sys( year ).social_care.means_test.residential.income.minimum_support_level  := Read( buff, MK( year, "social_care.means_test.residential.income.minimum_support_level" ));
         sys( year ).social_care.means_test.residential.income.percent_costs_met  := Read( buff, MK( year, "social_care.means_test.residential.income.percent_costs_met" ));
         sys( year ).social_care.means_test.residential.income.abolish  := Read( buff, MK( year, "social_care.means_test.residential.income.abolish" ));
         Put_Line( "Social Residential MT OK" );
         
         sys( year ).social_care.means_test.non_residential.assets.include_property  := Read( buff, MK( year, "social_care.means_test.non_residential.assets.include_property" ));
         sys( year ).social_care.means_test.non_residential.assets.upper_limit  := Read( buff, MK( year, "social_care.means_test.non_residential.assets.upper_limit" ));
         sys( year ).social_care.means_test.non_residential.assets.lower_limit  := Read( buff, MK( year, "social_care.means_test.non_residential.assets.lower_limit" ));
         sys( year ).social_care.means_test.non_residential.assets.taper  := Read( buff, MK( year, "social_care.means_test.non_residential.assets.taper" ));
         sys( year ).social_care.means_test.non_residential.assets.abolish  := Read( buff, MK( year, "social_care.means_test.non_residential.assets.abolish" ));
         sys( year ).social_care.means_test.non_residential.income.floor  := Read( buff, MK( year, "social_care.means_test.non_residential.income.floor" ));
         sys( year ).social_care.means_test.non_residential.income.minimum_support_level  := Read( buff, MK( year, "social_care.means_test.non_residential.income.minimum_support_level" ));
         sys( year ).social_care.means_test.non_residential.income.percent_costs_met  := Read( buff, MK( year, "social_care.means_test.non_residential.income.percent_costs_met" ));
         sys( year ).social_care.means_test.non_residential.income.abolish  := Read( buff, MK( year, "social_care.means_test.non_residential.income.abolish" ));

         sys( year ).social_care.means_test.maxima.maximum_weekly_charge_residential  := Read( buff, MK( year, "social_care.means_test.maxima.maximum_weekly_charge_residential" ));
         sys( year ).social_care.means_test.maxima.maximum_weekly_charge_non_residential  := Read( buff, MK( year, "social_care.means_test.maxima.maximum_weekly_charge_non_residential" ));
         sys( year ).social_care.means_test.maxima.maximum_lifetime_payment  := Read( buff, MK( year, "social_care.means_test.maxima.maximum_lifetime_payment" ));
         Put_Line( "Social Non Residential MT OK" );
         sys( year ).social_care.options.abolish  := Read( buff, MK( year, "social_care.options.abolish" ));
         sys( year ).social_care.options.preserve_for_existing_claimants  := Read( buff, MK( year, "social_care.options.preserve_for_existing_claimants" ));
         
         sys( year ).social_care.insurance.actuarially_fair  := Read( buff, MK( year, "social_care.insurance.actuarially_fair" ));
         sys( year ).social_care.insurance.contribution := Contribution_Type'Value( Read( buff, MK( year, "social_care.insurance.contribution" )));

         sys( year ).av_costs.hour_of_care  := Read( buff, MK( year, "av_costs.hour_of_care" ));
         sys( year ).av_costs.residential_per_week :=  Read( buff, MK( year, "av_costs.residential_per_week" ));
      end loop;      
      Put_Line( "Parameters.IO : Read OK" );
   end Read;
   
end Model.WSC.Parameters.IO;
