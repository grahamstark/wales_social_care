--
--  $Author: graham_s $
--  $Date: 2012-04-06 15:56:14 +0100 (Wed, 06 Apr 2012) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );

with Ada.Command_Line;
with Ada.Direct_IO;

with AUnit.Assertions;   
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Ada.Calendar;

with GNATColl.Traces;

with Base_Model_Types;

with Model.WSC.Global_Settings;
with Model.WSC.Household.Database;
with Model.WSC.Household.Examples;
with Model.WSC.Household.IO;
with Model.WSC.Household.Regressions;
with Model.WSC.Household;
with Model.WSC.Parameters.IO;
with Model.WSC.Results.IO;
with Model.WSC.Results;
with Model.WSC.Run_Declarations;
with Model.WSC.Uprate;
with Model.WSC.Household.Transitions;
with Run_IO;
with UAP_Threshold_IO;
with WSC_Enums;


with Maths_Functions;
with Maths_Functions.Simple_Statistics;
with Statistics_Commons;
with Transition_Events;
with Model.Parameters;
with Model.Parameters.Means_Tested_Benefits;
with Model.Parameters.Non_Means_Tested_Benefits;

with T_Utils;
with Text_Utils;

package body Model.WSC.Static_Calculator.Tests is
   
   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;
   use Ada.Text_IO;
   use WSC_Enums;
   use Text_Utils;
   
   default_wsc_run : Model.WSC.Run_Declarations.Run;
   params : Parameters_Array;
   rand_list : M_Randoms.Random_List;
      
   package MF is new Maths_Functions( Rate );
   
   package MFStats is new MF.Simple_Statistics;
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.DYNAMIC_DRIVER.WEB_RUNNER" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   function Rec_Care_Probit( ad : Person ) return Amount is
   begin
      -- if( ad.age >= 90 )then
         -- return 1.0;
      -- elsif( ad.age >= 80 )then
         -- return -0.5;
      -- elsif( ad.age >= 65 )then
         -- return -2.0;
      -- end if;
      -- return -1.0;
      return Model.WSC.Household.Regressions.Recieving_Any_Care_Probit( 
         ad                    => ad, 
         receiving_last_period => False, 
         use_lags              => False );
   end Rec_Care_Probit;
   
   procedure Test_Basic_Stats( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use MFStats;
      use Statistics_Commons;
      d1,d2,d3 : MFStats.Dataset;
      s1,s2,s3 : Measures_Array;
   begin
      for i in 1 .. 4_000 loop
         d1.Add( Rate( i ));
      end loop;
      s1 := d1.Generate;
      for s in Simple_Measures loop
         Put_Line( s'Img & " " & Format( s1( s )));
      end loop;
   end Test_Basic_Stats;
   
   
   procedure Test_Get_UAP_Level( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Model.WSC.Household.Examples;
      hh                : Model.WSC.Household.Household := Get_Household( single_retired_person );
      level             : UAP_Level;
      thresholds        : UAP_Array;
      needs_score       : Amount;
      care_blind_degree : Amount := 0.0;
      ad                : Model.WSC.Household.Person renames hh.benefit_units( 1 ).adults( 1 );
   begin
      -- default wave AM thresholds
      thresholds( critical )    :=  -6.93338562029997E-01;
      thresholds( substantial ) :=  -8.65252817280005E-01;
      thresholds( moderate )    :=  -9.57856156020007E-01;
      thresholds( low )         :=  -1.21617584943999E+00;
      thresholds( none )        :=  Amount'Last;
      for age in 1 .. 3 loop
         care_blind_degree := 0.0;
         for cc in 1 .. 3 loop            
            case age is
               when 1 => ad.age := 65;
               when 2 => ad.age := 80;
               when 3 => ad.age :=100;
            end case;
            for n_adults in 1 .. 2 loop
               ad.Recalculate_Regressors( n_adults, 0, hh.hdata );                  
               needs_score := Rec_Care_Probit( ad );
               level := Get_UAP_Level( 
                  ad, 
                  n_adults, 
                  Rec_Care_Probit'Access, 
                  care_blind_degree, 
                  thresholds );
               if( n_adults = 2 ) and ( cc = 1 )then
                  Assert( level = none, "UAP Level for 2 adult care 0 blind systam was " & level'Img );
               end if;
               Put_Line( 
                  " Num Adults = " & n_adults'Img & 
                  " Age = " &  ad.age'Img & 
                  " needs score " & Format( needs_score ) & 
                  " care blind degree " & Format( care_blind_degree ) & 
                  " => UAP Level " & level'Img );
            end loop;
            care_blind_degree := care_blind_degree + 0.50;
         end loop;
      end loop;
   end Test_Get_UAP_Level;
   
   procedure Test_Apply_Income_Means_Test( test_case : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Model.WSC.Household.Examples;
      use Ada.Calendar;
      hh : Model.WSC.Household.Household := Get_Household( single_retired_person );
   begin
      null;
   end Test_Apply_Income_Means_Test;   
   
   procedure Test_Get_Non_Residential_Care_Hours_Offer( test_case : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Model.WSC.Household.Examples;
      use Ada.Calendar;
      hh                : Model.WSC.Household.Household := Get_Household( single_retired_person );
      level             : UAP_Level;
      thresholds        : UAP_Array;
      needs_score       : Amount;
      year              : Year_Number;
      pers_results      : Personal_Result;
      needs_sys         : Social_Care_Needs_System; 
      hours_offered_pre_means_test : Amount := 0.0;
      ad                : Model.WSC.Household.Person renames hh.benefit_units( 1 ).adults( 1 );
   begin
      thresholds( critical )    :=  -6.93338562029997E-01;
      thresholds( substantial ) :=  -8.65252817280005E-01;
      thresholds( moderate )    :=  -9.57856156020007E-01;
      thresholds( low )         :=  -1.21617584943999E+00;
      thresholds( none )        :=  Amount'Last;
      -- default wave AM thresholds
      each_wave:
      for wave in t .. am loop
         Put_Line( "Test_Get_Non_Residential_Care_Hours_Offer for wave " & Waves'Image( wave ));
         year := Year_From_Wave( wave );
         needs_sys := params( year ).social_care.needs_assessment_rules;
         needs_sys.use_carer_blind_system := True;
         for aup in UAP_Level loop
            Put_Line( "needs_sys.share_of_category_passing_test(" & aup'Img & ") = " & Format( needs_sys.share_of_category_passing_test( aup )));   
         end loop;
         ages:
         for age in 1 .. 3 loop
            case age is
               when 1 => ad.age := 65;
               when 2 => ad.age := 80;
               when 3 => ad.age :=100;
            end case;
            needs_score := Rec_Care_Probit( ad );
            needs_sys.carer_blind_degree := 0.0;
            care_blinds:
            for cc in 1 .. 3 loop
               adult_count:
               for n_adults in 1 .. 2 loop
                  ad.Recalculate_Regressors( n_adults, 0, hh.hdata );
                  needs_score := Rec_Care_Probit( ad );
                  rand_list.Reset_Pos;
                  Get_Non_Residential_Care_Hours_Offer( 
                     sys              => needs_sys,
                     num_adults_in_bu => n_adults,
                     prs              => pers_results,
                     hours            => hours_offered_pre_means_test, 
                     ad               => ad,
                     thresholds       => thresholds,
                     rand_list        => rand_list );
                  Put_Line( 
                     " Num Adults = " & n_adults'Img & 
                     " Age = " &  ad.age'Img & 
                     " needs score " & Format( needs_score ) & 
                     " care blind degree " & Format( needs_sys.carer_blind_degree ) & 
                     " => hours_offered_pre_means_test " & Format( hours_offered_pre_means_test ) & 
                     " AUP Level " & pers_results.uap'Img );
                     
               end loop adult_count;
               needs_sys.carer_blind_degree := needs_sys.carer_blind_degree + 0.50;
            end loop care_blinds;
         end loop ages;
      end loop each_wave;
   end Test_Get_Non_Residential_Care_Hours_Offer;
   
   procedure Test_Calculate_Non_Residential( test_case : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Model.WSC.Household.Examples;
      use Ada.Calendar;
      hh                : Model.WSC.Household.Household := Get_Household( single_retired_person );
      level             : UAP_Level;
      thresholds        : UAP_Array;
      needs_score       : Amount;
      year              : Year_Number;
      pers_results      : Personal_Result;
      ad                : Model.WSC.Household.Person renames hh.benefit_units( 1 ).adults( 1 );
   begin
      thresholds( critical )    :=  -6.93338562029997E-01;
      thresholds( substantial ) :=  -8.65252817280005E-01;
      thresholds( moderate )    :=  -9.57856156020007E-01;
      thresholds( low )         :=  -1.21617584943999E+00;
      thresholds( none )        :=  Amount'Last;
      -- default wave AM thresholds
      each_wave:
      for wave in t .. am loop
         Put_Line( "Test_Calculate_Non_Residential for wave " & Waves'Image( wave ));
         year := Year_From_Wave( wave );
         params( year ).social_care.needs_assessment_rules.use_carer_blind_system := True;
         for aup in UAP_Level loop
            Put_Line( "needs_sys.share_of_category_passing_test(" & aup'Img & ") = " & Format( params( year ).social_care.needs_assessment_rules.share_of_category_passing_test( aup )));   
         end loop;
         Put_Line( "social_care.non_residential.income.incomes " & Income_Package.To_String( params( year ).social_care.means_test.non_residential.income.incomes ));
         
         ages:
         for age in 1 .. 3 loop
            case age is
               when 1 => ad.age := 65;
               when 2 => ad.age := 80;
               when 3 => ad.age :=100;
            end case;
            needs_score := Rec_Care_Probit( ad );
            params( year ).social_care.needs_assessment_rules.carer_blind_degree := 0.0;
            care_blinds:
            for cc in 1 .. 3 loop
               adult_count:
               for n_adults in 1 .. 2 loop
                  incomes:
                  for inc in 1 .. 4 loop
                     case inc is
                        when 1 => ad.current_income( pension_prev_emp ) := 0.0;
                        when 2 => ad.current_income( pension_prev_emp ) := 100.0;
                        when 3 => ad.current_income( pension_prev_emp ) := 500.0;
                        when 4 => ad.current_income( pension_prev_emp ) := 1_000.0;
                     end case;
                     capitals:
                     for cap in 1 .. 5 loop
                        case cap is
                           when 1 => ad.personal_wealth := 0.0;
                           when 2 => ad.personal_wealth := 1_000.0;
                           when 3 => ad.personal_wealth := 10_000.0;
                           when 4 => ad.personal_wealth := 20_000.0;
                           when 5 => ad.personal_wealth := 25_000.0;
                        end case;

                        Zero( pers_results, clear_historical => True );

                        for it in Calculated_Incomes loop
                           pers_results.income( it ) := ad.current_income( it ); 
                        end loop;
                        
                        ad.Recalculate_Regressors( n_adults, 0, hh.hdata );
                        needs_score := Rec_Care_Probit( ad );
                        rand_list.Reset_Pos;
                        Calculate_Non_Residential(
                           hh,
                           params( year ),
                           ad,
                           pers_results,
                           thresholds,
                           default_wsc_run,
                           rand_list );
                        Put_Line( 
                           " Num Adults = " & n_adults'Img & 
                           " Age = " &  ad.age'Img & 
                           " ad.current_income( pension_prev_emp ) " & Format( ad.current_income( pension_prev_emp )) &
                           " ad.personal_wealth = " & Format( ad.personal_wealth ) & 
                           " needs score " & Format( needs_score ) & 
                           " care blind degree " & Format( params( year ).social_care.needs_assessment_rules.carer_blind_degree ));
                        Put_Line( 
                           " passes_non_residential_capital_test = " & pers_results.passes_non_residential_capital_test'Img &
                           " passes_non_residential_income_test = " & pers_results.passes_non_residential_income_test'Img &
                           " passes_non_residential_means_test = " & pers_results.passes_non_residential_means_test'Img &
                           " gross_care_costs = " & Format( pers_results.gross_care_costs ) &
                           " client_contributions = " & Format( pers_results.client_contributions ) &
                           " passes_non_residential_means_test = " & pers_results.passes_non_residential_means_test'Img &
                           " hours_of_care_la = " & Format( pers_results.hours_of_care_la ) &
                           " la_contributions  = " & Format( pers_results.la_contributions  ) &
                           " hours_of_care_private = " & Format( pers_results.hours_of_care_private ) &
                           " client_contributions = " & Format( pers_results.client_contributions ) &
                           " AUP Level " & pers_results.uap'Img );
                        Put_Line( Model.WSC.Results.To_String( pers_results.intermediate,  "  " ));
                     end loop capitals;
                  end loop incomes;
               end loop adult_count;
               params( year ).social_care.needs_assessment_rules.carer_blind_degree := params( year ).social_care.needs_assessment_rules.carer_blind_degree + 0.50;
            end loop care_blinds;
         end loop ages;
      end loop each_wave;
   end Test_Calculate_Non_Residential;
   
   procedure Test_Calculate_Residential( test_case : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Model.WSC.Household.Examples;
      use Ada.Calendar;
      hh                : Model.WSC.Household.Household := Get_Household( single_retired_person );
      level             : UAP_Level;
      needs_score       : Amount;
      year              : Year_Number;
      pers_results      : Personal_Result;
      ad                : Model.WSC.Household.Person renames hh.benefit_units( 1 ).adults( 1 );
   begin
      ad.years_in_residential_care := 1;
      hh.hdata.house_value := 0.0;
      hh.hdata.other_property_value := 0.0;
      each_wave:
      for wave in t .. am loop
         ad.years_in_residential_care := ad.years_in_residential_care + 1;
         Put_Line( "Test_Calculate_Residential for wave " & Waves'Image( wave ));
         year := Year_From_Wave( wave );
         params( year ).social_care.needs_assessment_rules.use_carer_blind_system := True;
         for aup in UAP_Level loop
            Put_Line( "needs_sys.share_of_category_passing_test(" & aup'Img & ") = " & Format( params( year ).social_care.needs_assessment_rules.share_of_category_passing_test( aup )));   
         end loop;
         Put_Line( "social_care.non_residential.income.incomes " & Income_Package.To_String( params( year ).social_care.means_test.non_residential.income.incomes ));
         
         ages:
         for age in 1 .. 3 loop
            case age is
               when 1 => ad.age := 65;
               when 2 => ad.age := 80;
               when 3 => ad.age :=100;
            end case;
            needs_score := Rec_Care_Probit( ad );
            params( year ).social_care.needs_assessment_rules.carer_blind_degree := 0.0;
            incomes:
            for inc in 1 .. 4 loop
               case inc is
                  when 1 => ad.current_income( pension_prev_emp ) := 0.0;
                  when 2 => ad.current_income( pension_prev_emp ) := 100.0;
                  when 3 => ad.current_income( pension_prev_emp ) := 500.0;
                  when 4 => ad.current_income( pension_prev_emp ) := 1_000.0;
               end case;
               capitals:
               for cap in 1 .. 5 loop
                  case cap is
                     when 1 => ad.personal_wealth := 0.0;
                     when 2 => ad.personal_wealth := 1_000.0;
                     when 3 => ad.personal_wealth := 10_000.0;
                     when 4 => ad.personal_wealth := 20_000.0;
                     when 5 => ad.personal_wealth := 25_000.0;
                  end case;

                  Zero( pers_results, clear_historical => True );

                  for it in Calculated_Incomes loop
                     pers_results.income( it ) := ad.current_income( it ); 
                  end loop;
                  
                  ad.Recalculate_Regressors( 1, 0, hh.hdata );
                  rand_list.Reset_Pos;
                  Calculate_Residential(
                     hh,
                     params( year ),
                     ad,
                     pers_results,
                     default_wsc_run,
                     rand_list );
                  Put_Line( 
                     " Age = " &  ad.age'Img & 
                     " ad.current_income( pension_prev_emp ) " & Format( ad.current_income( pension_prev_emp )) &
                     " ad.personal_wealth = " & Format( ad.personal_wealth ));
                  Put_Line( 
                     " passes_residential_capital_test = " & pers_results.passes_residential_capital_test'Img &
                     " passes_residential_income_test = " & pers_results.passes_residential_income_test'Img &
                     " passes_residential_means_test = " & pers_results.passes_residential_means_test'Img &
                  
                     " gross_care_costs = " & Format( pers_results.gross_care_costs ) &
                     " tarriff income = " & Format( pers_results.tarriff_income ) &
                     " client_contributions = " & Format( pers_results.client_contributions ) &
                     " passes_non_residential_means_test = " & pers_results.passes_non_residential_means_test'Img &
                     " hours_of_care_la = " & Format( pers_results.hours_of_care_la ) &
                     " la_contributions  = " & Format( pers_results.la_contributions  ) &
                     " client_contributions = " & Format( pers_results.client_contributions ));
                  Put_Line( Model.WSC.Results.To_String( pers_results.intermediate,  "  " ));
               end loop capitals;
            end loop incomes;
            params( year ).social_care.needs_assessment_rules.carer_blind_degree := params( year ).social_care.needs_assessment_rules.carer_blind_degree + 0.50;
         end loop ages;
      end loop each_wave;
   end Test_Calculate_Residential;
   
   procedure Set_Up( tc : in out Test_Case) is
      username : Unbounded_String;
   begin
      Put_Line( "Model.WSC.Static_Calculator.Tests:: SET UP" );
      -- rand_list.Reset;
      rand_list.Load( Model.WSC.Global_Settings.Physical_Root & "data/randoms/randoms_1.txt" );
      Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 1 ));
      default_wsc_run := Model.WSC.Run_Declarations.Read_Settings( Ada.Command_Line.Argument( 2 ));
      Put_Line( "GLOBAL SETTINGS READ OK" );
      Put_Line( "LOG OUTPUT SET" );
      Model.WSC.Global_Settings.Initialise_Logging;
      Put_Line( "LOG TARGETS SET" );
      Put_Line( "Set_Up: opening parameters from |" & Model.WSC.Global_Settings.Default_Text_Parameter_File_Name );
      params := Model.WSC.Parameters.IO.Read( 
         Model.WSC.Global_Settings.Default_Text_Parameter_File_Name,
         default_wsc_run.Start_Year,
         default_wsc_run.End_Year );
   end Set_Up;

   procedure Shut_Down( tc : in out Test_Case) is
   begin
      null;
   end Shut_Down;
   
   procedure Test_One_HH_Calculation( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.WSC.Household.Examples;
   use Model.WSC.Household.Transitions;
   use Ada.Calendar;


      procedure Load_Last( results : in out Household_Result ) is
      begin
         for buno in 1 .. results.res.num_benefit_units loop
            for adno in 1 .. results.benefit_units( buno ).res.num_people loop
               declare 
                  ad :Personal_Result renames results.benefit_units( buno ).people( adno );
               begin
                  results.benefit_units( buno ).people_last_period( adno ) := ad;
                  ad.lifetime_gross_payments := ad.lifetime_gross_payments + ( ad.gross_care_costs * 52.0 );      
                  ad.lifetime_client_contributions := ad.total_payments_to_date + ( ad.client_contributions * 52.0 );
                  ad.total_payments_to_date := ad.lifetime_client_contributions;
                  ad.lifetime_la_contributions := ad.lifetime_la_contributions + ( ad.la_contributions * 52.0 );
                  for i in Calculated_Incomes loop
                     if( ad.income( i ) > ad.highest_previous_income( i ))then
                        ad.highest_previous_income( i ) := ad.income( i );
                     end if;
                  end loop;
               end;
            end loop;
         end loop;
      end Load_Last;

   
      hh             : Model.WSC.Household.Household := Get_Household( single_retired_person );
      results        : Household_Result;
      evc            : Transition_Events.Transition_Events_Counter.Recorder;
      rand_list      : M_Randoms.Random_List;
      year           : Year_Number;
      uap_thresholds : UAP_Array;
      f              : File_Type;
   begin
      hh.wave := u;
      Create( f, Out_File, "tmp/Test_One_HH_Calculation.txt" );
      rand_list.Load( Model.WSC.Global_Settings.Physical_Root & "data/randoms/randoms_2.txt" );
      Zero( results, clear_historical => True );
      Put_Line( "TEST ONE HOUSEHOLD- START" );
      for wave in u .. aa loop
         Zero( results, clear_historical => False );
         year := Year_From_Wave( wave );
         Put_Line( f, "On Year " & year'Img );
         Put_Line( "On Year " & year'Img );
         Calculate_One_Household(
            wave,
            hh,
            params( year ),
            results,
            default_wsc_run,
            uap_thresholds,
            rand_list );
         Put_Line( f,  "======== WAVE ========= " & wave'Img );
         Put_Line( f,  hh.To_String );
         Put_Line( f,  To_String( results ));
         if( wave < Simulation_Waves'Last )then
            Age( hh, default_wsc_run, evc );
         end if;
         Load_Last( results );
      end loop;
      Put_Line( "TEST ONE HOUSEHOLD - DONE" );      
      Close( f );
   end Test_One_HH_Calculation;
 
   procedure Test_Results_IO ( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.WSC.Results.IO;
   use Model.WSC.Results;
      inm, outm : Individual_Results_Map;
      res1, res2 : Personal_Result;
      s : Sernum_Value;
   begin
      for l in 1 .. 1000 loop
         res1.sernum := 123456 + Sernum_Value( l );
         res1.is_residential := False;       
         res1.receives_social_care := True;
         for i in Calculated_Incomes loop
            res1.income( i ):= 99887766.78 + Amount( l );
         end loop;      
         res1.la_contributions := 199887766.78 + Amount( l );
         res1.client_contributions := 299887766.78 + Amount( l );
         res1.gross_care_costs := 399887766.78 + Amount( l );
      
         res1.disposable_income := 499887766.78 + Amount( l );
         res1.net_income := 599887766.78 + Amount( l );
         res1.marginal_rate := 699887766.78 + Amount( l );
         res1.capital_contribution := 799887766.78 + Amount( l );
         res1.minimum_income_guarantee := 899887766.78 + Amount( l );
         for s in Summary_Items_Type loop
            res1.summary( s ):= 999887766.78 + Amount( l );
         end loop;
         for c in Costs_Type loop
            res1.costs_summary( c ) := 1099887766.78 + Amount( l );
         end loop;
         inm.Insert( res1.sernum, res1 );
      end loop;
      Dump( "tmp/test_results_1.txt", inm );
      Restore( "tmp/test_results_1.txt", outm );
      for l in 1 .. 1000 loop
         s := 123456 + Sernum_Value( l );
         res2 := outm.Element( s );
         res1 := inm.Element( s );
         if( res1 /= res2 )then
            Put_Line( "res1 " & To_String( res1 ));
            Put_Line( "res2 " & To_String( res2 ));
         end if;
         Assert( res1 = res2, " unequal results for " & l'Img );
      end loop;
      Dump( "tmp/test_results_2.txt", outm );  
   end Test_Results_IO;
      
   procedure Test_Rounding( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      f : Amount := 73.60;
   begin
      Put_Line( Counter_Type'Image( Counter_Type( f )));
      Put_Line( Counter_Type'Image( Counter_Type( Amount'Rounding( f*100.0 ) / 100.0 )));
      Put_Line( Counter_Type'Image( Counter_Type( Amount'Rounding( f*100.0 )) / 100.0 ));
      Put_Line( Format( f ));
      Put_Line( Format_With_Commas( f ));
      Put_Line( Format( Counter_Type( f )));
      Put_Line( Format_With_Commas( Counter_Type( f )));
   end Test_Rounding;

   procedure Test_Means_Tests( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.WSC.Household.Examples;
      hh : Model.WSC.Household.Household;
      res : Household_Result;
      passes_income_test       : Means_Test_Result := not_applicable;
   begin
      for h in age_uk_indira .. zero_income loop
         Zero( res, clear_historical => True );
         hh := Get_Household( h );
         res.res.num_benefit_units := hh.num_benefit_units;
         res.benefit_units( 1 ).res.num_people := hh.benefit_units( 1 ).num_adults;
         for i in Calculated_Incomes loop
            res.benefit_units( 1 ).people( 1 ).income( i ) := 
                  hh.benefit_units( 1 ).adults( 1 ).current_income( i ); 
         end loop;
         res.benefit_units( 1 ).people( 1 ).gross_care_costs := 81.0;
         res.benefit_units( 1 ).people( 1 ).tarriff_income := 8.0;
               
         for adno in 1 .. hh.benefit_units( 1 ).num_adults loop
            Apply_Income_Means_Test(
               ad             => hh.benefit_units( 1 ).adults( 1 ),
               num_adults     => 1,
               hdata          => hh.hdata,
               test           => params( 2012 ).social_care.means_test.residential.income,
               is_sys         => params( 2012 ).benefits.pension_credit.guaranteed_credit,
               res            => res.benefit_units( 1 ).people( 1 ),
               result_state   => passes_income_test );
         end loop;
         Accumulate( hh, res, default_wsc_run );
         Put_Line( "MEANS TEST == HHLD " & h'Img );
         Put_Line( To_String( hh.benefit_units( 1 )));
         Put_Line( To_String( res ));

      end loop;
   end Test_Means_Tests;
   
   
   procedure Test_Run_Settings_IO( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.WSC.Run_Declarations;
      rs1, rs2 : Model.WSC.Run_Declarations.Run;
   begin
      rs1 := default_wsc_run;
      Write_Settings( "tmp/wsc_run.txt", rs1 );
      rs2 := Read_Settings( "tmp/wsc_run.txt" );
      Assert( rs1 = rs2, "settings should match " );
   end Test_Run_Settings_IO;

   procedure Test_Rand_List( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      r, rn : Rate;
      type RAT is array( 1 .. 10, 1 .. 100 ) of Rate;
      ra, ran : RAT;
   begin
      for j in 1 .. 10 loop
         if( j <= 5 )then
            rand_list.Reset_Pos;
            Put_Line( " index reset; iteration " & j'Img );
         else
            Put_Line( " complete reset; iteration " & j'Img );
            rand_list.Reset;
         end if;
         
         for i in 1 .. 100 loop
            rand_list.Next( r );
            rand_list.Next_Normal( rn );
            ra( j, i ) := r;
            ran( j, i ) := rn;
            Put_Line( " r[" & i'Img & "] = " & r'Img & "  " & rn'Img );
         end loop;
      end loop;
      for j in 1 .. 10 loop
         for i in 1 .. 100 loop
            if( j > 1 and j <= 5 )then
               Assert( ra( j, i ) = ra( j-1, i ), "R mismatch " & ra( j, i )'Img & " j " & j'Img & " " & ra( j-1, i )'Img );
               Assert( ran( j, i ) = ran( j-1, i ), "RN mismatch " & ran( j, i )'Img & " j " & j'Img & " " & ran( j-1, i )'Img );
            elsif( j > 7 )then
               Assert( ra( j, i ) /= ra( j-1, i ), "R match " & ra( j, i )'Img & " j " & j'Img & " " & ra( j-1, i )'Img );
               Assert( ran( j, i ) /= ran( j-1, i ), "RN match " & ran( j, i )'Img & " j "& j'Img & " "  & ran( j-1, i )'Img );
            end if;
         end loop;
      end loop;
   end Test_Rand_List;
   
   procedure Test_Pension_Credit( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.WSC.Household.Examples;
      hh : Model.WSC.Household.Household;
      res : Household_Result;
   begin
      for h in age_uk_indira .. zero_income loop
         Zero( res, clear_historical => True );
         hh := Get_Household( h );
         res.res.num_benefit_units := hh.num_benefit_units;
         res.benefit_units( 1 ).res.num_people := hh.benefit_units( 1 ).num_adults;
         for i in Calculated_Incomes loop
            res.benefit_units( 1 ).people( 1 ).income( i ) := 
                  hh.benefit_units( 1 ).adults( 1 ).current_income( i ); 
         end loop;
         
         Calculate_Guaranteed_Pension_Credit( 
            hh.benefit_units( 1 ),  
            res.benefit_units( 1 ),
            params( 2012 ).benefits.pension_credit.guaranteed_credit,
            params( 2012 ).benefits.state_pension,
            default_wsc_run );
         Calculate_Savings_Credit( 
            hh.benefit_units( 1 ),  
            res.benefit_units( 1 ),
            params( 2012 ).benefits.pension_credit.savings_credit,
            default_wsc_run );
         Accumulate( hh, res, default_wsc_run );
         Put_Line( "HHLD " & h'Img );
         Put_Line( To_String( hh.benefit_units( 1 )));
         Put_Line( To_String( res ));
         if( h = age_uk_indira )then
            Assert( Nearly_Equal( 9.36, res.benefit_units( 1 ).people( 1 ).income( pension_credit )),
               " pension credit should be 9.36 was " & 
               Format( res.benefit_units( 1 ).people( 1 ).income( pension_credit )));
         elsif ( h = zero_income )then
            Assert( Nearly_Equal( 137.35, res.benefit_units( 1 ).people( 1 ).income( pension_credit )),
               " pension credit should be 137.35 was " & 
               Format( res.benefit_units( 1 ).people( 1 ).income( pension_credit )));
            
         end if;
         
      end loop;
   end Test_Pension_Credit;
   
   procedure Test_AA( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.WSC.Household.Examples;
      hh : Model.WSC.Household.Household := Get_Household( old_sick_single_male );
      aa : Amount:= 1.0;
   begin
      for age in 65 .. 100 loop
         hh.benefit_units( 1 ).adults( 1 ) := Make_Retired_Adult( hh, age, male, 0, False, very_poor );
         Recalculate_Regressors( hh.benefit_units( 1 ).adults( 1 ), hh.Num_Adults, hh.Num_Children, hh.hdata );
         if( age = 65 or age = 100 )then
            Put_Line( To_String( hh ));
         end if;
         Calculate_Attendance_Allowance( 
            hh.benefit_units( 1 ).adults( 1 ),
            aa,
            aa,
            params( 2012 ).benefits.attendance_allowance,
            default_wsc_run, 
            rand_list );
      end loop;
   end Test_AA;
   
   procedure Test_DLA( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.WSC.Household.Examples;
      hh : Model.WSC.Household.Household := Get_Household( old_sick_single_male );
      care, mob : Amount:= 0.0;
      last_care, last_mob : Amount:= 0.0;
   begin
      Put_Line( "Static_Tests: DLA" );
      for age in 40 .. 65 loop
         hh.benefit_units( 1 ).adults( 1 ) := Make_Retired_Adult( hh, age, male, 0, False, very_poor );
         for f in Task_Type loop
            hh.benefit_units( 1 ).adults( 1 ).fitness( f ).help := not_at_all;
            hh.benefit_units( 1 ).adults( 1 ).fitness( f ).diff := very_difficult;
         end loop;
         Recalculate_Regressors( hh.benefit_units( 1 ).adults( 1 ), hh.Num_Adults, hh.Num_Children, hh.hdata );
         
         if( age = 40 or age = 65 )then
            Put_Line( To_String( hh ));
         end if;
         
         Calculate_DLA( 
            hh.benefit_units( 1 ).adults( 1 ),
            care,
            mob,
            last_care + last_mob,
            params( 2012 ).benefits.dla,
            default_wsc_run, 
            rand_list );
        Put_Line( "Test_DLA; got care as " & Format( care ) & " mob " & Format( mob ));
        last_care := care + 1.0;
        last_mob := mob;
      end loop;
   end Test_DLA;

   --------------------
   -- Register_Tests --
   --------------------
   procedure Register_Tests( tc : in out Test_Case ) is
      use AUnit.Test_Cases.Registration;
   begin
      Put_Line( "Static_Tests: register tests" );
      Register_Routine( tc, Test_Calculate_Residential'Access, "Test_Calc_Residential" );
      Register_Routine( tc, Test_Basic_Stats'Access, "Test_Basic_Stats" );
      Register_Routine( tc, Test_Get_UAP_Level'Access, "Test UAP Level" );
      Register_Routine( tc, Test_Apply_Income_Means_Test'Access, "Test_Apply_Income_Means_Test" ); 
      Register_Routine( tc, Test_Get_Non_Residential_Care_Hours_Offer'Access, " Test_Get_Non_Residential_Care_Hours_Offer" );
      Register_Routine( tc, Test_Calculate_Non_Residential'Access, "Test_Calculate_Non_Residential" ); 
      Register_Routine( tc, Test_One_HH_Calculation'Access, "Test_One_HH_Calculation" );
      Register_Routine( tc, Test_AA'Access, "Test_AA" );
      Register_Routine( tc, Test_DLA'Access, "Test_DLA" );
      Register_Routine( tc, Test_Pension_Credit'Access , "Pension Credit " );
      Register_Routine( tc, Test_Means_Tests'Access, "Test_Means_Tests" );
      -- Register_Routine( tc, Test_Run_Settings_IO'Access, "Test_Run_Settings_IO" );
      -- Register_Routine( tc,Test_Results_IO'Access, "Test_Results_IO" );
      -- Register_Routine( tc,Test_Rounding'Access, "Test Rounding" );
      -- Register_Routine( t, Test_Rand_List'Access, "Test_Rand_List" );
      -- Register_Routine( t, Test_Years_In_Home'Access, "Test Years In Home" );
   end Register_Tests;

   ----------
   -- Name --
   ----------
   function Name ( tc : Test_Case ) return Message_String is
   begin
      return Format( "Model.WSC.Static_Calculator.Tests" );
   end Name;

end Model.WSC.Static_Calculator.Tests;
