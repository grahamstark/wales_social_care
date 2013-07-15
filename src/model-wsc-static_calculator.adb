with Model.WSC.Household.Regressions;
with Ada.Assertions;
with Ada.Numerics.Float_Random;
with GNATColl.Traces;
with Ada.Text_IO;
with WSC_Enums;
with Model.WSC.Regressors;
with Model.WSC.Household.Regressions;

package body Model.WSC.Static_Calculator is
   
   use Ada.Text_IO;
   use WSC_Enums;
   use Ada.Assertions;
    
   -- AA split from http://83.244.183.180/100pc/aa/ccaaawd/ccgor/a_carate_r_ccaaawd_c_ccgor_nov10.html
   AA_Counts_Wales : constant High_Low_Array := ( high => 80.85, low => 33.57, nil => 0.0 );
   AA_Prop_Low     : constant Rate := AA_Counts_Wales( low ) / ( AA_Counts_Wales( high ) + AA_Counts_Wales( low ));
   
   type H_low_By_H_ML_Array is array( High_Middle_Low_Nil_Type,  High_Low_Nil_Type ) of Amount;
   DLA_Counts_Wales : constant H_low_By_H_ML_Array := (
      ( 46.90, 14.35, 3.25 ),
      ( 35.24, 27.54, 6.85 ),
      ( 38.79, 13.77, 20.05 ),
      ( 33.34, 4.46, 0.0 ));
   DLA_TOTAL_WALES : constant Amount := 244.54;
   -- from statswales item 024768
   type Hours_Split is ( lt_5, s5_9, s10_19, s20_plus );
   type Hours_Array is array( Hours_Split ) of Amount;
   type UAP_By_Hours_Array is array( UAP_Level, Hours_Split ) of Amount;
   
   -- see my notes for where this comes from. Probability of each band of
   -- hours by PRS.UAP level (so 100% change of < 5 hours for low PRS.UAP offers
   -- 15% chance of > 20 hours for critical cases
   HOURS_PROB_BY_UAP : constant UAP_By_Hours_Array := (
      none        => ( 0.0, 0.0, 0.0, 0.0 ), 
      low         => ( 1.0, 0.0, 0.0, 0.0 ), 
      moderate    => ( 1.0, 0.0, 0.0, 0.0 ),
      substantial => ( 1.0, 0.58, 0.0, 0.0 ),
      critical    => ( 0.0, 1.0, 0.52, 0.15 )
   ); -- note the last possible ones are always 1, to guarantee we
      -- always pick something
   
   --
   -- from 200/10 from statwales series 024768 (staff hours rather than client hours)
   --
   AVERAGE_STAFF_HOURS : constant Hours_Array := (
      lt_5     => 1.9,
      s5_9     => 5.4,
      s10_19   => 12.5,
      s20_plus => 21.4
   );
   

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.STATIC_CALCULATOR" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
    
   procedure Evaluate_Random( 
      rand_list : in out M_Randoms.Random_List;
      thresh    : Amount;
      go        : out Boolean ) is
      r : Rate := Rand_List.Next;
   begin
      go := r <= thresh;   
   end Evaluate_Random;
   
   function Get_UAP_Level( 
      ad : Person; 
      num_adults_in_bu : Person_Count;
      get_needs : Make_Needs_Score_Access;
      care_blind_degree : Amount;
      thresholds : UAP_Array ) return UAP_Level is
      score : Amount := get_needs( ad );
      uap  : UAP_Level := UAP_Level'Last;
   begin
      for u in UAP_Level loop
         if( score > thresholds( u ))then
            UAP := u;
            exit;
         end if;
      end loop;
      if( ad.receives_informal_care_from_household_member ) or ( num_adults_in_bu > 1 ) then
         if( care_blind_degree = 0.0 )then
            UAP := none;
            --
            -- fixme check other person's UAP level
            --
         elsif( care_blind_degree < 0.5 )then
            if( UAP < low )then
               -- towards first is higher needs
               -- so move up 2 levels reduces need if carer present
               UAP := UAP_Level'Succ( UAP_Level'Succ( uap ));
            else
               uap := none;
            end if;
         elsif( care_blind_degree < 1.0 )then
            if( uap <= low )then
               -- less severe by 1 level
               uap := UAP_Level'Succ( uap );
            end if;
         end if;
         -- so 1.0 means 'don't change' 
      end if;
      return uap;
   end Get_UAP_Level;
   
   procedure Get_Non_Residential_Care_Hours_Offer( 
      sys        : Social_Care_Needs_System; 
      num_adults_in_bu : Person_Count;
      hours      : out Amount; 
      prs        : in out Personal_Result;
      ad         : Person;
      thresholds : UAP_Array;
      rand_list  : in out M_Randoms.Random_List ) is
         
      care_blindness   : Amount := 0.0;
      passes           : Boolean;
      use_this_package : Boolean;
      which_hr         : Hours_Split := Hours_Split'First;
      random_hours : Hours_Array;            
   begin
      for h in reverse Hours_Split loop
         random_hours( h ) := rand_list.Next;
      end loop;
      hours := 0.0;
      if( sys.use_carer_blind_system )then
         care_blindness := sys.carer_blind_degree;
      end if;
      prs.uap := Get_UAP_Level( ad, num_adults_in_bu, Rec_Care_Probit'Access, care_blindness, thresholds );
      Evaluate_Random( rand_list, sys.share_of_category_passing_test( prs.uap ) / 100.0, passes );
      find_hours:
      for h in reverse Hours_Split loop
         use_this_package := random_hours( h ) <= HOURS_PROB_BY_UAP( prs.uap, h );
         -- Evaluate_Random( 
            -- rand_list, 
            -- HOURS_PROB_BY_UAP( prs.uap, h ), 
            -- use_this_package );
         if use_this_package then
            which_hr := h;
            exit find_hours;
         end if;
      end loop find_hours;
      if( passes )then
          hours := AVERAGE_STAFF_HOURS( which_hr );
      end if;
      Log( "Get_Non_Residential_Care_Hours_Offer hours offer " & Format( hours ) & "passes prob " & passes'Img);
   end  Get_Non_Residential_Care_Hours_Offer;
   
   function ADL_Score( ad : Person ) return Amount is
      use Model.WSC.Regressors;
   begin
      if( ad.receives_informal_care_from_household_member )then
         return 0.0;
      end if;
      return ad.regressors( adlscore );
   end ADL_Score;

   function Rec_Care_Probit( ad : Person ) return Amount is
      use Model.WSC.Household.Regressions;
   begin
      -- if( ad.receives_informal_care_from_household_member )then
      --    return Amount'First;
      -- end if;
      return Recieving_Any_Care_Probit( 
         ad                    => ad, 
         receiving_last_period => False, 
         use_lags              => False );

   end Rec_Care_Probit;


   procedure Calculate_Attendance_Allowance( 
      pers           : Person; 
      aa             : out Amount;
      aa_last_period : Amount;
      sys            : Attendance_Allowance_System;
      wsc_run        : Run;
      randoms        : in out M_Randoms.Random_List  ) is
   use Model.WSC.Household.Regressions;
      pr               : Amount;
      qualifies_for_aa : Boolean;
      rand             : Rate := randoms.Next_Normal;
      r                : Rate := randoms.Next;
   begin
      aa := 0.0;
      if(( pers.age < sys.low_age ) or ( pers.age > sys.high_age ) or ( pers.years_in_residential_care > 0 ))then
         return;
      end if;
      pr := Receives_AA_Probit( pers, aa_last_period );
      qualifies_for_aa := Evaluate_Probit( 
         v                    =>  pr + rand, 
         add_random_component => False,
         threshold            => sys.test_generosity * wsc_run.probit_thresholds( receiving_aa ));
         
      Log( " Calculate_Attendance_Allowance got pr as " & Format( pr ) & " => qualifies_for_aa " & qualifies_for_aa'Img & " last period " & Format( aa_last_period ));
      if( qualifies_for_aa )then
         if( r < AA_Prop_Low )then
            aa := sys.benefit_rate( low );
         else
            aa := sys.benefit_rate( high );
         end if;
      end if;
    end Calculate_Attendance_Allowance;

    
   procedure Calculate_DLA( 
      pers            : Person; 
      care            : out Amount;
      mobility        : out Amount;
      dla_last_period : Amount;
      sys             : Disability_Living_Allowance_System;     
      wsc_run         : Run;
      randoms         : in out M_Randoms.Random_List ) is
   use Model.WSC.Household.Regressions;
      probit_pr         : Amount;
      qualifies_for_dla : Boolean; 
      p                 : Rate := randoms.Next;
      rand              : Rate := randoms.Next_Normal;
      cum               : Amount := 0.0;
   begin
      care := 0.0;
      mobility := 0.0;
      Log( "Calculate_DLA; got age limits as " & sys.care.low_age'Img & " : " & sys.care.high_age'Img );
      if( dla_last_period = 0.0 ) and then -- allow to stay on at 65+ if already on
       (( pers.age < sys.care.low_age ) or ( pers.age > sys.care.high_age ))then
         return;
      end if;
      probit_pr := Receives_DLA_Probit( pers, dla_last_period );
      qualifies_for_dla := Evaluate_Probit( 
         v                    => probit_pr+rand, 
         threshold            => sys.mobility.test_generosity * wsc_run.probit_thresholds( receiving_dla ),
         add_random_component => False );
      Log( "Calculate_DLA Last Period" & Format( dla_last_period ) & " qualifies_for_dla " & qualifies_for_dla'Img );
      if( dla_last_period > 0.0 )then
         Log( "DLA LAST PERIOD POSITIVE" );
      end if;
      allocate_to_dla_level:
      for c in High_Middle_Low_Nil_Type loop
         for m in High_Low_Nil_Type loop
            cum := cum + ( DLA_Counts_Wales( c, m ) / DLA_TOTAL_WALES );
            if( cum > p )then
               Log( "Calculate_DLA; got prob as " & Format( p ) & " => care state " & c'Img & " mob state " & m'Img );
               care := sys.care.benefit_rate( c );
               mobility := sys.mobility.benefit_rate( m );
               exit allocate_to_dla_level;
            end if;
         end loop;            
      end loop allocate_to_dla_level;
      if( not qualifies_for_dla )then
         mobility := 0.0;
         care := 0.0;
      end if;
      if( pers.years_in_residential_care > 0 )then
         care := 0.0;
      end if;
   end Calculate_DLA;

   function Get_Disabled_Benefits return Income_Package.Set is
      s : Income_Package.Set;
   begin
      s.Include( sev_disabl_allow );
      s.Include( attendance_allow );
      s.Include( disab_liv_allowcare );
      s.Include( disab_liv_allow_dk );
      return s;      
   end Get_Disabled_Benefits;
   
   function Get_Care_Benefits return Income_Package.Set is
      s : Income_Package.Set;
   begin
      s.Include( inv_care_allow );
      return s;      
   end Get_Care_Benefits;
   
   DISAB_SET : constant Income_Package.Set := Get_Disabled_Benefits;
   CARE_SET : constant Income_Package.Set := Get_Care_Benefits;
   
   function Has_Disabled_Benefit( ad : Person; res : Personal_Result ) return Boolean is
      incset : Income_Package.Set := ad.Which_Incomes_Received( True );
      benset : Income_Package.Set := Which_Incomes_Received( res.income );
   begin
      incset.Union( benset );
      return not incset.Intersection( DISAB_SET ).Is_Empty;      
   end Has_Disabled_Benefit;

   function Test_Ages( 
      bu : Benefit_Unit;
      age_men : Age_Range;
      age_women : Age_Range ) return Boolean is
   begin
      for adno in 1 .. bu.num_adults loop
         if( bu.adults( adno ).sex = male ) then
            if( bu.adults( adno ).age >= age_men )then
               return True;
            end if;
         else
            if( bu.adults( adno ).age >= age_women )then
               return True;
            end if;
         end if;
      end loop;
      return False;
   end Test_Ages;
   
   procedure Calculate_Guaranteed_Pension_Credit( 
      bu       : Benefit_Unit;  
      res      : in out Benefit_Unit_Result;
      gpcsys   : Guaranteed_Credit_System;
      pensys   : Pension_System;
      wsc_run : Run ) is
      is_couple    : Boolean := bu.Is_Couple;
      income       : Amount := 0.0;
      num_carers   : Person_Count := 0;
      num_disabled : Person_Count := 0;
      additional_amounts : Amount := 0.0;
      mig                : Amount := 0.0;
      standard_guarantee : Amount;
      has_non_dependent_adult : Boolean := False; -- FIXME we have to look at all other BUs for this, too.
   begin
      if( not Test_Ages( bu, pensys.age_men, pensys.age_women ))then
         return;
      end if;
      if( is_couple )then
         standard_guarantee := gpcsys.couple;
      else
         standard_guarantee := gpcsys.single;
      end if;
      -- assume all other adults in the BU are non-dependents
      -- this is NOT RIGHT because we're counting extra adults here
      for adno in 1 .. bu.num_adults loop
         declare
            ad : Person renames bu.adults( adno );
            incset : Income_Package.Set := ad.Which_Incomes_Received( True );
            benset : Income_Package.Set := Which_Incomes_Received( res.people( adno ).income );
         begin
            incset.Union( benset );
            if not incset.Intersection( DISAB_SET ).Is_Empty then
               num_disabled := num_disabled + 1;
            end if;
            if not incset.Intersection( CARE_SET ).Is_Empty then
               num_carers := num_carers + 1;
            end if;
         end;
      end loop;
      if( num_disabled >= 2 ) then
         additional_amounts := gpcsys.severe_disability_couple;
      elsif( num_disabled = 1 )then
         additional_amounts := gpcsys.severe_disability_single;
      end if;
      if( num_carers >= 1 )then
         additional_amounts := additional_amounts + gpcsys.carer_single;
      end if;
      mig := standard_guarantee + additional_amounts;
      Assert( mig > 0.0, "MIG Must be positive; was " & Format( mig ));
      income := Calculate_Income( bu=>bu, res=>res, which_to_include=>gpcsys.incomes );
      Assert( income >= 0.0, " income must be positive " & Format( income ));
      -- FIXME not person 1? bu.head_id ??
      res.people( 1 ).minimum_income_guarantee := mig;
      res.res.minimum_income_guarantee := mig;
      
      res.people( 1 ).income( pension_credit ) := Amount'Max( 0.0, mig - income );
      --
      -- in case there's an actual value stored in there 
      --
      res.people( 2 ).income( pension_credit ) := 0.0;
      Add_To_Map( res.res.intermediate, "GPC: MIG", mig );
      Add_To_Map( res.res.intermediate, "GPC: Additional Amounts", additional_amounts );
      Add_To_Map( res.res.intermediate, "GPC: Standard Guarantee", standard_guarantee );
      Add_To_Map( res.res.intermediate, "GPC: Income", income );
      Add_To_Map( res.res.intermediate, "GPC: amount = ", res.people( 1 ).income( pension_credit ));
   end Calculate_Guaranteed_Pension_Credit;

   procedure Calculate_Savings_Credit( 
      bu       : Benefit_Unit;  
      res      : in out Benefit_Unit_Result;
      sys      : Savings_Credit_System;
      wsc_run : Run ) is
      qualifying_income : Amount := 0.0;
      threshold         : Amount := 0.0;
      income            : Amount := 0.0;
      maximum           : Amount := 0.0;
      excess_income     : Amount := 0.0;
      credit            : Amount := 0.0;
      income_over_mig   : Amount := 0.0;
      is_couple         : constant Boolean := bu.Is_Couple;
   begin
      if( not Test_Ages( bu, 60, 60 ))then
         return;
      end if;
      qualifying_income := Calculate_Income( bu, res, sys.qualifying_incomes );
      income := Calculate_Income( bu, res, sys.incomes );
      if( is_couple )then
         maximum := sys.maximum_couple;
         threshold := sys.threshold_couple;
      else
         maximum := sys.maximum_single;
         threshold := sys.threshold_single;
      end if;
      excess_income := Amount'Max( 0.0, qualifying_income - threshold );
      if( excess_income <= 0.0 )then
         credit := 0.0;
      else
         excess_income := excess_income * sys.withdrawal_rate/100.0;
         if( excess_income < maximum )then
            credit := excess_income;
         else
            income_over_mig := Amount'Max( 0.0, income - res.res.minimum_income_guarantee );
            credit := maximum - (( 1.0 - sys.withdrawal_rate/100.0 ) * income_over_mig );
         end if;
         credit := Amount'Min( credit, maximum );
         credit := Amount'Max( 0.0, credit );
      end if;
      res.people( 1 ).income( pension_credit ) := res.people( 1 ).income( pension_credit ) + credit;
      res.people( 2 ).income( pension_credit ) := 0.0;
      Add_To_Map( res.res.intermediate, "CS: Excess Income", excess_income );
      Add_To_Map( res.res.intermediate, "CS: Maximum", maximum );
      Add_To_Map( res.res.intermediate, "CS: Income", income );
      Add_To_Map( res.res.intermediate, "CS: Qualifying Income", qualifying_income );
      Add_To_Map( res.res.intermediate, "CS: Threshold", threshold );
      Add_To_Map( res.res.intermediate, "CS: Income over MIG ", income_over_mig );
      Add_To_Map( res.res.intermediate, "CS: Withdrawal Rate ", sys.withdrawal_rate );
      Add_To_Map( res.res.intermediate, "CS: Savings Credit", credit );
   end Calculate_Savings_Credit;

   procedure Calculate_State_Pension( 
      bu       : Benefit_Unit;  
      res      : in out Benefit_Unit_Result;
      sys      : Pension_System;
      wsc_run : Run ) is
      p : Amount := 0.0;
      is_couple  : constant Boolean := bu.Is_Couple;
   begin
      -- FIXME add a contribution ratio like taxben used to have
      -- FIXME Child additions, spouse additions
      -- contributions ???
      for adno in 1 .. bu.num_adults loop
         if( bu.adults( adno ).sex = male ) then
            -- deferred pensions, incomplete contributions, child & spouse adds for 
            -- people 
            if( bu.adults( adno ).age >= sys.age_men )then
               res.people( adno ).income( ni_retir_pension ) := sys.class_a;
            end if;
         else
            if( bu.adults( adno ).age >= sys.age_women )then
               res.people( adno ).income( ni_retir_pension ) := sys.class_a;
            end if;
         end if;
      end loop;
   end Calculate_State_Pension;

   procedure Apply_Income_Means_Test(
      ad             : Person;
      num_adults     : Person_Count;
      hdata          : Household_Data;
      test           : Income_Means_Test_System;
      is_sys         : Guaranteed_Credit_System;
      res            : in out Personal_Result;
      result_state   : out Means_Test_Result ) is
         
      income_available_for_charging  : Amount;
      income : Amount := Calculate_Income( ad.current_income, res.income, test.incomes );
      safety_level : Amount;
      extras       : Amount;
      net_costs    : Amount := res.gross_care_costs - res.tarriff_income;
   begin
      if( test.abolish or net_costs <= 0.0 )then
         result_state := not_applicable;
         return;
      end if;
      extras := hdata.net_housing_costs/Amount( Num_Adults ); -- plus something for disabilty costs
      -- check period for net_housing_costs (monthly?)
      res.minimum_income_guarantee := is_sys.single;
      if( Has_Disabled_Benefit( ad, res ))then
         res.minimum_income_guarantee := res.minimum_income_guarantee + is_sys.severe_disability_single;
      end if;
      safety_level := ( 1.0 + (test.floor/100.0 )) * ( res.minimum_income_guarantee + extras );
      -- mortgage, other costs
      income_available_for_charging := income - safety_level;
      income_available_for_charging := Amount'Max( 0.0, income_available_for_charging );
      res.la_contributions := net_costs - income_available_for_charging; 
      if( res.la_contributions < test.minimum_support_level )then
         res.la_contributions := Amount'Min( test.minimum_support_level, net_costs );
      end if;
      
      if( test.percent_costs_met > 0.0 )then
         declare
            min_share : Amount := (test.percent_costs_met/100.0) * net_costs;
         begin           
            res.la_contributions := Amount'Max( min_share, res.la_contributions );
         end;
      end if;
      res.la_contributions := Amount'Max( 0.0, res.la_contributions );
      res.client_contributions := net_costs - res.la_contributions; 
      if( res.la_contributions = 0.0 )then
         result_state := not_entitled;
      elsif( res.client_contributions > 0.0 )then
         result_state := partially_entitled;
      else
         result_state := fully_entitled;
      end if;
      Assert( res.la_contributions >= 0.0 and res.la_contributions <= net_costs,
         "la contributions out of range; value is " & Format( res.la_contributions ) & 
         " gross care costs were " & Format( net_costs ));
      Assert( res.client_contributions >= 0.0 and res.client_contributions <= net_costs,
         "client contributions out of range; value is " & Format( res.client_contributions ) & 
         " net care costs were " & Format( net_costs ));
         
      Add_To_Map( res.intermediate, "Care Income: Income", income );
      Add_To_Map( res.intermediate, "Care Income: Safety Level", safety_level );
      Add_To_Map( res.intermediate, "Care Income: Floor", test.floor );
      Add_To_Map( res.intermediate, "Care Income: Extras", extras );
      Add_To_Map( res.intermediate, "Care Income: Income Available for Charging", income_available_for_charging );
   end Apply_Income_Means_Test;
   
   procedure Apply_Asset_Means_Test( 
      ad                           : Person;
      net_housing_assets_per_adult : Amount;
      test                         : Assets_Means_Test_System;
      contribution                 : out Amount;
      result_state                 : out Means_Test_Result ) is
      cap : Amount := ad.personal_wealth;
   begin
      contribution := 0.0;
      if( test.abolish )then
         result_state := not_applicable;
         return;
      end if;
      if( test.include_property )then
         cap := cap + net_housing_assets_per_adult;
      end if;
      Log( "Apply_Asset_Means_Test test.upper_limit" & Format( test.upper_limit ) & " cap " & Format( cap ) & " test.include_property " & test.include_property'Img ); 
      if( cap > test.upper_limit )then
         result_state := not_entitled;
      elsif( cap > test.lower_limit )then
         contribution := ( test.taper / 250.0 ) * ( cap - test.lower_limit ); -- weekly contribution from annual rate
         result_state := partially_entitled;
      else 
         result_state := fully_entitled;
      end if;         
   end Apply_Asset_Means_Test;

   procedure Calculate_Residential(
      hh        : Model.WSC.Household.Household;
      params    : Parameters_Rec;
      ad        : Person;
      prs       : in out Personal_Result;
      wsc_run   : Run;
      randoms   : in out M_Randoms.Random_List ) is
      
      mtsys   : Social_Care_Means_Test_System renames params.social_care.means_test.residential; 
      options : Care_Options renames params.social_care.options;
      
      net_housing_assets_per_adult : Amount := ( hh.hdata.house_value - hh.hdata.mortgage_outstanding ) / Amount( hh.Num_Adults );         
   begin   
      prs.gross_care_costs := params.av_costs.residential_per_week;
      if prs.lifetime_client_contributions > 0.0 then
         Log( "positive lifetime_client_contributions " & Format( prs.lifetime_client_contributions ));         
      end if;
      Log( "Calculate_Residential:: prs.lifetime_client_contributions " & Format( prs.lifetime_client_contributions ) &
           " params.social_care.means_test.maxima.maximum_lifetime_payment " & Format( params.social_care.means_test.maxima.maximum_lifetime_payment ));
      if prs.lifetime_client_contributions > params.social_care.means_test.maxima.maximum_lifetime_payment then
         -- FIXME what if part way through year?
         Log( "exceeded lifetime max payment " );
         prs.client_contributions := 0.0;
         prs.la_contributions := params.av_costs.residential_per_week;
         prs.passes_residential_means_test := fully_entitled;
      else
         Apply_Asset_Means_Test( 
            ad,
            net_housing_assets_per_adult,
            mtsys.assets,
            prs.tarriff_income,
            prs.passes_residential_capital_test );
    
         Log( "prs.passes_residential_capital_test " & prs.passes_residential_capital_test'Img & " tariff income " & Format( prs.tarriff_income ));
         
         Apply_Income_Means_Test(
            ad,
            hh.Num_Adults,
            hh.hdata,
            mtsys.income,
            params.benefits.pension_credit.guaranteed_credit,
            prs,
            prs.passes_residential_income_test );
            
         prs.passes_residential_means_test := Combine_Results(
            prs.passes_residential_income_test,
            prs.passes_residential_capital_test );
         declare
               total_contribs : Amount := prs.client_contributions + prs.tarriff_income;
         begin   
            if( total_contribs > 0.0 and then total_contribs > params.social_care.means_test.maxima.maximum_weekly_charge_residential )then
               declare
                  total_client_contributions : Amount := params.social_care.means_test.maxima.maximum_weekly_charge_residential;
                  tarriff_prop : Rate := prs.tarriff_income / total_contribs;
               begin
                  prs.la_contributions :=  prs.gross_care_costs - params.social_care.means_test.maxima.maximum_weekly_charge_residential;
                  prs.passes_residential_means_test := partially_entitled;
                  --
                  -- distribute max payment in proportion to capital and income contributions
                  -- FIXME does it work this way?
                  -- 
                  prs.tarriff_income :=  total_client_contributions * tarriff_prop;
                  prs.client_contributions := total_client_contributions * ( 1.0 - tarriff_prop );
               end;
            end if;
         end;
         if( options.preserve_for_existing_claimants )then
            if( prs.la_contributions < prs.highest_la_contribution )then
               prs.la_contributions := Amount'Min( prs.highest_la_contribution, prs.gross_care_costs );
               prs.client_contributions := Amount'Max( 0.0, prs.gross_care_costs - prs.la_contributions );
               if( prs.client_contributions > 0.0 )then
                  prs.passes_residential_means_test := partially_entitled;
               else
                  prs.passes_residential_means_test := fully_entitled;
               end if;
            end if;
         end if;
         
         if( prs.passes_residential_means_test = not_entitled )then
            prs.la_contributions := 0.0;
         end if;
         prs.client_contributions := params.av_costs.residential_per_week - prs.la_contributions;
      end if;
   end Calculate_Residential;

   
   procedure Calculate_Non_Residential(
      hh             : Model.WSC.Household.Household;
      params         : Parameters_Rec;
      ad             : Person;
      prs            : in out Personal_Result;
      uap_thresholds : UAP_Array;
      wsc_run : Run;
      randoms        : in out M_Randoms.Random_List ) is
         
   use Model.WSC.Household.Regressions;
      private_care_demand_rand : Rate;
      net_housing_assets_per_adult : Amount := ( hh.hdata.house_value - hh.hdata.mortgage_outstanding ) / Amount( hh.Num_Adults );         
      mtsys   : Social_Care_Means_Test_System renames params.social_care.means_test.non_residential;   
      options : Care_Options renames params.social_care.options;
      latent_hours_of_care_demand : Amount := 0.0;
   begin
      randoms.Next_Normal( private_care_demand_rand );
      --
      -- Assumptions here are:
      -- 
      -- a) private and state paid care have same hours
      -- b) no private and state care for same person
      -- c) hours chosen are same for private and state
      -- d) takeup for private is indepnendent of cost
      -- e) people will choose state if there is any LA contribution at all
      --
      Get_Non_Residential_Care_Hours_Offer( 
         sys              => params.social_care.needs_assessment_rules,
         num_adults_in_bu => hh.Num_Adults,
         prs              => prs,
         hours            => latent_hours_of_care_demand, 
         ad               => ad,
         thresholds       => uap_thresholds,
         rand_list        => randoms );
      if( latent_hours_of_care_demand > 0.0 )then   
         Assert( 
            params.av_costs.hour_of_care > 0.0, "params.av_costs.hour_of_care should be positive was " & 
            Format( params.av_costs.hour_of_care ));
         prs.gross_care_costs := latent_hours_of_care_demand  * params.av_costs.hour_of_care;
         
         if prs.lifetime_client_contributions > params.social_care.means_test.maxima.maximum_lifetime_payment then
            -- FIXME what if part way through year?
            Log( "exceeded lifetime max payment " );
            prs.client_contributions := 0.0;
            prs.la_contributions := prs.gross_care_costs;
            prs.passes_non_residential_means_test := fully_entitled;
         else
         
            Apply_Asset_Means_Test( 
               ad,
               net_housing_assets_per_adult,
               mtsys.assets,
               prs.tarriff_income,
               prs.passes_non_residential_capital_test );
            Add_To_Map( prs.intermediate, "Calculate_Non_Residential: hours_offered pre means test", latent_hours_of_care_demand );
            
            Apply_Income_Means_Test(
                  ad,
                  hh.Num_Adults,
                  hh.hdata,
                  mtsys.income,
                  params.benefits.pension_credit.guaranteed_credit,
                  prs,
                  prs.passes_non_residential_income_test );
            
            prs.passes_non_residential_means_test := Combine_Results( 
               prs.passes_non_residential_income_test, 
               prs.passes_non_residential_capital_test );
   
            declare
                  total_contribs : Amount := prs.client_contributions + prs.tarriff_income;
            begin   
               if( total_contribs > 0.0 and then total_contribs > params.social_care.means_test.maxima.maximum_weekly_charge_non_residential )then
                  declare
                     total_client_contributions : Amount := params.social_care.means_test.maxima.maximum_weekly_charge_non_residential;
                     tarriff_prop : Rate := prs.tarriff_income / total_contribs;
                  begin
                     prs.la_contributions :=  prs.gross_care_costs - params.social_care.means_test.maxima.maximum_weekly_charge_non_residential;
                     prs.passes_non_residential_means_test := partially_entitled;
                     --
                     -- distribute max payment in proportion to capital and income contributions
                     -- FIXME does it work this way?
                     -- FIXME make this a procedure & not a dup of residential
                     prs.tarriff_income :=  total_client_contributions * tarriff_prop;
                     prs.client_contributions := total_client_contributions * ( 1.0 - tarriff_prop );
                  end;
               end if;
            end;

            prs.client_contributions := prs.client_contributions + prs.tarriff_income;
            Assert( Nearly_Equal( prs.client_contributions + prs.la_contributions, prs.gross_care_costs ), 
               " care cost mismatch " &
               " prs.client_contributions " & Format( prs.client_contributions ) &
               " prs.la_contribitions " & Format( prs.la_contributions ) & 
               " prs.gross_care_costs " & Format( prs.gross_care_costs ));   

            if( options.preserve_for_existing_claimants )then
               if( prs.la_contributions < prs.highest_la_contribution )then
                  --
                  -- Limit to actual amount spent
                  --
                  prs.la_contributions := Amount'Min( prs.highest_la_contribution, prs.gross_care_costs );
                  prs.client_contributions := Amount'Max( 0.0, prs.gross_care_costs - prs.la_contributions );
                  if( prs.client_contributions > 0.0 )then
                     prs.passes_non_residential_means_test := partially_entitled;
                  else
                     prs.passes_non_residential_means_test := fully_entitled;
                  end if;
               end if;
            end if;
         end if;
         case prs.passes_non_residential_means_test is
            when not_entitled =>
               --
               -- private care demand - same hours as state 
               --
               prs.la_contributions := 0.0;
               prs.hours_of_care_la := 0.0;
               declare
                  demands_private_care : Boolean;
                  pr : Amount := Private_Care_Demand_Probit( ad );
               begin
                  demands_private_care := Evaluate_Probit( 
                     v          =>  pr + private_care_demand_rand, 
                     threshold  => wsc_run.probit_thresholds( private_care_demand )); 
                  Log( "Calculate_Non_Residential; got probit v as " & Format( pr ) & " rand " & Format( private_care_demand_rand ) & " demands_private_care " & demands_private_care'Img & " hours offerered " & Format( latent_hours_of_care_demand ));
                  
                  if( demands_private_care )then
                     prs.hours_of_care_private := latent_hours_of_care_demand; -- FIXME scale this?
                     prs.client_contributions := prs.hours_of_care_private * params.av_costs.hour_of_care;
                  else
                     prs.hours_of_care_private := 0.0;
                     prs.client_contributions := 0.0;
                  end if;
               end;
         when fully_entitled =>
            Assert( prs.client_contributions = 0.0, "Client Contributions Must Be Zero at this point; were " & Format( prs.client_contributions ));
            prs.hours_of_care_la := latent_hours_of_care_demand;
            prs.hours_of_care_private := 0.0;
         when partially_entitled =>
            Assert( prs.client_contributions > 0.0, "Client Contributions Must Be > Zero at this point; were " & Format( prs.client_contributions ));
            Assert( prs.la_contributions > 0.0, "LA Contributions Must Be > Zero at this point; were " & Format( prs.la_contributions ));
            declare
               prop_met_by_la : Amount := prs.la_contributions / prs.gross_care_costs;
            begin
               prs.hours_of_care_la := latent_hours_of_care_demand * prop_met_by_la;
               prs.hours_of_care_private := latent_hours_of_care_demand - prs.hours_of_care_la;
            end;   
         when not_applicable =>
            Assert( False, "N/A should be impossible for Calculate_Non_Residential body pid " & ad.pid'Img );
         end case; 
      end if; -- hours pre means test
   end Calculate_Non_Residential;
 
   --
   -- FIXME extend somehow to benefit unit level claimants
   --
   procedure Preserve_For_Existing_Claimants( 
      sys        : Parameters_Rec; 
      res        : in out Household_Result ) is
         
      procedure Set_One_Pers( which : Calculated_Incomes ) is
      begin         
         for buno in 1 .. res.res.Num_Benefit_Units loop
            for adno in 1 .. res.benefit_units( buno ).res.num_people loop
               declare
                  pers : Personal_Result renames res.benefit_units( buno ).people( adno );
               begin
                  if( pers.highest_previous_income( which ) > pers.income( which ))then
                     pers.income( which ) := pers.highest_previous_income( which );
                  end if;
               end;
            end loop;
         end loop;
      end Set_One_Pers;   
         
   begin
      
      if( sys.benefits.dla.care.preserve_for_existing_claimants )then
         Set_One_Pers( disab_liv_allowcare );
      end if;
      if( sys.benefits.dla.mobility.preserve_for_existing_claimants )then
         Set_One_Pers( disab_liv_allowmob );
      end if;
      if( sys.benefits.attendance_allowance.preserve_for_existing_claimants )then
         Set_One_Pers( attendance_allow );
      end if;
      if( sys.benefits.state_pension.preserve_for_existing_claimants )then
         Set_One_Pers( ni_retir_pension );
      end if;
      
   end Preserve_For_Existing_Claimants;   

   procedure Calculate_One_Household(
      wave           : Waves;
      hh             : Model.WSC.Household.Household;
      params         : Parameters_Rec;
      results        : out Household_Result;
      wsc_run        : Run;
      uap_thresholds : UAP_Array;
      randoms        : in out M_Randoms.Random_List ) is
      last_Wave      : constant Waves := Waves'Pred( wave );
   begin
      Zero( results, clear_historical => False );
      results.res.sernum := hh.hid;
      results.res.num_benefit_units := hh.num_benefit_units;
      bu_benefits:
      for buno in 1 .. hh.num_benefit_units loop
         declare
            bu  : Benefit_Unit renames hh.benefit_units( buno );
            brs : Benefit_Unit_Result renames results.benefit_units( buno );
         begin
            brs.res.num_people := bu.Num_Adults;
            adults_benefits:
            for adno in  1 .. bu.Num_Adults loop
               declare
                  prs            : Personal_Result renames brs.people( adno );
                  prs_last       : Personal_Result renames brs.people_last_period( adno );
                  ad             : Person renames bu.adults( adno );
                  last_aa        : constant Amount := prs_last.income( attendance_allow );
                  last_dla_care  : constant Amount := prs_last.income( disab_liv_allowcare );
                  last_dla_mob   : constant Amount := prs_last.income( disab_liv_allowmob );
                  last_dla       : constant Amount := last_dla_mob + last_dla_care;
               begin
                  prs.sernum := ad.pid;
                  prs.is_residential := ad.years_in_residential_care > 0;
                  -- we fill calculated with recorded
                  -- gross incomes and overwrite AA and DLA
                  for i in Calculated_Incomes loop
                     prs.income( i ) := ad.current_income( i ); 
                  end loop;
                  Calculate_Attendance_Allowance(
                        ad,
                        prs.income( attendance_allow ),
                        last_aa, 
                        params.benefits.attendance_allowance,
                        wsc_run,
                        randoms );
                  Calculate_DLA( 
                     ad,
                     prs.income( disab_liv_allowcare ),
                     prs.income( disab_liv_allowmob ),
                     last_dla,
                     params.benefits.dla,
                     wsc_run,
                     randoms  );
               end;
            end loop adults_benefits;
            Calculate_State_Pension( 
               bu,
               brs,
               params.benefits.state_pension,
               wsc_run );
            Calculate_Guaranteed_Pension_Credit( 
               bu, 
               brs, 
               params.benefits.pension_credit.guaranteed_credit, 
               params.benefits.state_pension,
               wsc_run );
            Calculate_Savings_Credit( 
               bu, 
               brs, 
               params.benefits.pension_credit.savings_credit, 
               wsc_run );
            if( params.benefits.pension_credit.guaranteed_credit.preserve_for_existing_claimants )then
               for adno in 1 .. bu.num_adults loop
                  null;-- if( brs.people( adno ).incomes( pension_credit ) 
               end loop;
            end if;
         end;
      end loop bu_benefits;
      bu_care:
      for buno in 1 .. hh.num_benefit_units loop
         declare
            bu  : Benefit_Unit renames hh.benefit_units( buno );
            brs : Benefit_Unit_Result renames results.benefit_units( buno );
         begin
            adults_care:
            for adno in  1 .. bu.Num_Adults loop
               declare
                  prs : Personal_Result renames brs.people( adno );
                  ad  : Person renames bu.adults( adno );
               begin
                  prs.is_residential := ad.years_in_residential_care > 0;
                  if( prs.is_residential )then
                     Calculate_Residential(
                        hh      => hh,
                        params  => params,
                        ad      => ad,
                        prs     => prs,
                        randoms => randoms,
                        wsc_run => wsc_run );
                  else
                     Calculate_Non_Residential(
                        hh       => hh,
                        params   => params,
                        ad       => ad,
                        prs      => prs,
                        uap_thresholds => uap_thresholds,
                        randoms  => randoms,
                        wsc_run => wsc_run );
                  end if;
                  prs.receives_social_care := 
                        ad.years_in_residential_care > 0 or 
                        prs.hours_of_care_private > 0.0 or 
                        prs.hours_of_care_la > 0.0;
                end;
            end loop adults_care;
         end;
      end loop bu_care;
      Preserve_For_Existing_Claimants( params, results );
      Accumulate( hh, results, wsc_run);
   end Calculate_One_Household;

   
end Model.WSC.Static_Calculator;
