with Model.WSC.Household.Regressions;
with Ada.Numerics.Float_Random;
with Ada.Text_IO;
with WSC_Enums;
with GNATColl.Traces;

package body Model.WSC.Static_Calculator is
   
   use Ada.Text_IO;
   use WSC_Enums;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.STATIC_CALCULATOR" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
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
   

      -- Total 	Mobility Award Type
      -- Higher Rate 	Lower Rate 	Nil Rate
      -- Caseload (Thousands) 	Caseload (Thousands) 	Caseload (Thousands) 	Caseload (Thousands)
      -- Total 	244.54 	154.27 	60.12 	30.16
      -- Care Award Type 	64.49 	46.90 	14.35 	3.25
      -- Higher Rate
      -- Middle Rate 	69.63 	35.24 	27.54 	6.85
      -- Lower Rate 	72.61 	38.79 	13.77 	20.05
      -- Nil Rate 	37.81 	33.34 	4.46 	-
   
   Random_Gen : Ada.Numerics.Float_Random.Generator;

   function Get_Random return Rate is
   begin
      return Rate( Ada.Numerics.Float_Random.Random( Random_Gen ));
   end Get_Random;
   
   function Calculate_Attendance_Allowance( 
      pers           : Person; 
      aa_last_period : Amount;
      sys            : Attendance_Allowance_System;
      wsc_run : Run ) return Amount is
   use Model.WSC.Household.Regressions;
      p : Amount := 0.0;
      pr : Amount;
      receives_aa : Boolean;
   begin
      if(( pers.age < sys.low_age ) or ( pers.age > sys.high_age ) or ( pers.years_in_residential_care > 0 ))then
         return 0.0;
      end if;
      pr := Receives_AA_Probit( pers, aa_last_period);
      receives_aa := Evaluate_Probit( 
         v          =>  pr, 
         add_random => True,
         scaler     => sys.test_generosity );
         
      Put_Line( " Calculate_Attendance_Allowance got pr as " & Format( pr ) & " => receives_aa " & receives_aa'Img );
      if( receives_aa )then
         if( Get_Random < AA_Prop_Low )then
            p := sys.benefit_rate( low );
         else
            p := sys.benefit_rate( high );
         end if;
      end if;
      Put_Line( " Calculate_Attendance_Allowance got pr as " & Format( pr ) & " => receives_aa " & receives_aa'Img & " amount as " & Format( p ));
      return p;   
   end  Calculate_Attendance_Allowance;
   
   procedure Calculate_DLA( 
      pers            : Person; 
      care            : out Amount;
      mobility        : out Amount;
      dla_last_period : Amount;
      sys             : Disability_Living_Allowance_System;     
      wsc_run : Run ) is
   use Model.WSC.Household.Regressions;
      p            : Amount := 0.0;
      probit_pr    : Amount;
      receives_dla : Boolean; 
      cum          : Amount := 0.0;
   begin
      care := 0.0;
      mobility := 0.0;
      if( dla_last_period = 0.0 ) and then -- allow to stay on at 65+ if already on
       (( pers.age < sys.care.low_age ) or ( pers.age > sys.care.high_age ))then
         return;
      end if;
      probit_pr := Receives_DLA_Probit( pers, dla_last_period );
      receives_dla := Evaluate_Probit( 
         v          =>  probit_pr, 
         add_random => True,
         scaler     => sys.mobility.test_generosity );
      if( receives_dla )then
         p := Get_Random; -- random between 0 and 1
         for c in High_Middle_Low_Nil_Type loop
            for m in High_Low_Nil_Type loop
               cum := cum + ( DLA_Counts_Wales( c, m ) / DLA_TOTAL_WALES );
               if( cum > p )then
                  Put_Line( "Calculate_DLA; got prob as " & Format( p ) & " => care state " & c'Img & " mob state " & m'Img );
                  care := sys.care.benefit_rate( c );
                  mobility := sys.mobility.benefit_rate( m );
                  return;
               end if;
            end loop;            
         end loop;
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
   
   procedure Calculate_Guaranteed_Pension_Credit( 
      bu  : Benefit_Unit;  
      res : in out Benefit_Unit_Result;
      sys : Guaranteed_Credit_System;
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
      if( is_couple )then
         standard_guarantee := sys.couple;
      else
         standard_guarantee := sys.single;
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
         additional_amounts := sys.severe_disability_couple;
      elsif( num_disabled = 1 )then
         additional_amounts := sys.severe_disability_single;
      end if;
      if( num_carers >= 1 )then
         additional_amounts := additional_amounts + sys.carer_single;
      end if;
      mig := standard_guarantee + additional_amounts;
      income := Calculate_Income( bu, res, sys.incomes, True );
      -- FIXME not person 1? bu.head_id ??
      res.people( 1 ).minimum_income_guarantee := mig;
      res.minimum_income_guarantee := mig;
      res.people( 1 ).income( pension_credit ) := Amount'Min( 0.0, mig - income );
      
      Add_To_Map( res.intermediate, "GPC: MIG", mig );
      Add_To_Map( res.intermediate, "GPC: Additional Amounts", additional_amounts );
      Add_To_Map( res.intermediate, "GPC: Standard Guarantee", standard_guarantee );
      Add_To_Map( res.intermediate, "GPC: Income", income );
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
      qualifying_income := Calculate_Income( bu, res, sys.qualifying_incomes, False );
      income := Calculate_Income( bu, res, sys.incomes, False );
      if( is_couple )then
         maximum := sys.maximum_couple;
         threshold := sys.threshold_couple;
      else
         maximum := sys.maximum_single;
         threshold := sys.threshold_single;
      end if;
      excess_income := Amount'Max( 0.0, qualifying_income - threshold );
      credit := excess_income * sys.withdrawal_rate;
      credit := Amount'Min( credit, maximum );
      income_over_mig := income - res.minimum_income_guarantee;
      if( income_over_mig > 0.0 )then
         credit := credit - (( 1.0 - sys.withdrawal_rate )*income_over_mig);         
      end if;
      credit := Amount'Max( 0.0, credit );
      res.people( 1 ).income( pension_credit ) := res.people( 1 ).income( pension_credit ) + credit;
      Add_To_Map( res.intermediate, "CS: Excess Income", excess_income );
      Add_To_Map( res.intermediate, "CS: Maximum", maximum );
      Add_To_Map( res.intermediate, "CS: Income", income );
      Add_To_Map( res.intermediate, "CS: Qualifying Income", qualifying_income );
      Add_To_Map( res.intermediate, "CS: Threshold", threshold );
      Add_To_Map( res.intermediate, "CS: Income over MIG ", income_over_mig );
      Add_To_Map( res.intermediate, "CS: Savings Credit", credit );
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
            if( bu.adults( adno ).age > sys.age_men )then
               res.people( adno ).income( ni_retir_pension ) := sys.class_a;
            end if;
         else
            if( bu.adults( adno ).age > sys.age_women )then
               res.people( adno ).income( ni_retir_pension ) := sys.class_a;
            end if;
         end if;
      end loop;
   end Calculate_State_Pension;
 
   procedure Calculate_One_Household(
      hh       : Model.WSC.Household.Household;
      params   : Parameters_Rec;
      results  : in out Household_Result;
      previous_results : Individual_Results_Map;
      wsc_run : Run ) is
   begin
      for buno in 1 .. hh.num_benefit_units loop
         declare
            bu  : Benefit_Unit renames hh.benefit_units( buno );
            brs : Benefit_Unit_Result renames results.benefit_units( buno );
         begin
            Calculate_Guaranteed_Pension_Credit( 
               bu, 
               brs, 
               params.benefits.pension_credit.guaranteed_credit, 
               wsc_run );
            if( params.benefits.pension_credit.guaranteed_credit.preserve_for_existing_claimants )then
               for adno in 1 .. bu.num_adults loop
                  null;-- if( brs.people( adno ).incomes( pension_credit ) 
               end loop;
            end if;
         end;
      end loop;
   end Calculate_One_Household;
   
  
   
begin
   Ada.Numerics.Float_Random.Reset( Random_Gen );
end Model.WSC.Static_Calculator;
