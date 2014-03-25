with Ada.Strings.Unbounded;
with Text_Utils;
with Ada.Assertions;
with Ada.Unchecked_Deallocation;

package body Model.WSC.Results is
   use Ada.Strings.Unbounded;
   use Text_Utils;
   use Ada.Assertions;
 
   function To_String( intermediate : Auxiliary_Results; indent : String ) return String is
   begin
      return Base_Model_Types.To_String( intermediate, indent );
   end To_String;

   function Which_Incomes_Received( a : Calculated_Incomes_Array ) return Income_Package.Set is
      s : Income_Package.Set;
   begin
      for i in a'Range loop
         if( a( i ) /= 0.0 )then
            s.Include( i );
         end if;
      end loop;
      return s;
   end Which_Incomes_Received;
      
   function Calculate_Income( 
      gross_income : Incomes_Array;
      calc_income  : Calculated_Incomes_Array;
      which_to_include : Included_Incomes_Array ) return Amount is
         a : Amount := 0.0;
   begin
      for i in Incomes_Type loop
         if( i in Calculated_Incomes )then
            a := a + calc_income( i ) * which_to_include( i );
         else
            a := a + gross_income( i ) * which_to_include( i );
         end if;
      end loop;
      return a;
   end Calculate_Income;
   
   
      
   function Make_Summary_Items( 
      ad  : Person;
      res : Personal_Result ) return Summary_Items_Array is
      s : Summary_Items_Array := ( others => 0.0 );
   begin
      s( population ) := 1.0;
      if( ad.age >= 60 )then
         s( age60_plus_population ) := 1.0;
         if( ad.sex = male )then
            s( male_60_plus_population ) := 1.0;
         else
            s( female_60_plus_population ) := 1.0;
         end if;
      else
         return s;
      end if;
      if( ad.age >= 65 )then
         s( age65_plus_population ) := 1.0;
      end if;
      if( ad.age >= 70 )then
         s( age70_plus_population ) := 1.0;
      end if;
      if( ad.age >= 80 )then
         s( age80_plus_population ) := 1.0;
      end if;
      if( ad.age >= 60 )then
         if( res.receives_social_care )then
            s( social_care_clients ) := 1.0;
            if( res.is_residential )then
               s( residential_clients ) := 1.0;
               case res.passes_residential_means_test is
                  when not_applicable =>
                     Assert( False, "receives res care but res.passes_residential_means_test = not_applicable and is residential");
                  when not_entitled =>
                     s( residential_privately_funded_clients ) := 1.0;  
                  when partially_entitled => 
                     s( residential_partially_publicly_funded_clients ) := 1.0;  
                  when fully_entitled =>
                     s( residential_fully_publicly_funded_clients ) := 1.0;  
               end case;
               s( residential_private_funding ) := res.client_contributions * 52.0;
               s( residential_public_funding ) :=  res.la_contributions * 52.0;
            else
               case res.passes_non_residential_means_test is
                  when not_applicable =>
                     Assert( False, "receives social care but res.passes_non_residential_means_test = not_applicable ");
                  when not_entitled =>
                     s( non_residential_clients ) := 1.0;
                     s( non_residential_privately_funded_clients ) := 1.0;  
                  when partially_entitled =>
                     s( non_residential_clients ) := 1.0;
                     s( non_residential_partially_publicly_funded_clients ) := 1.0;  
                  when fully_entitled =>
                     s( non_residential_clients ) := 1.0;
                     s( non_residential_fully_publicly_funded_clients ) := 1.0;  
               end case;
               s( non_residential_private_funding ) := res.client_contributions * 52.0;
               s( non_residential_public_funding ) := res.la_contributions * 52.0;
            end if;
         end if;
         if( res.income( disab_liv_allowcare ) > 0.0 )then
            s( dla_care_recipients ) := 1.0;
            s( dla_care_cost ) := res.income( disab_liv_allowcare ) * 52.0;
         end if;
         if( res.income( disab_liv_allowmob ) > 0.0 )then
            s( dla_mob_recipients ) := 1.0;
            s( dla_mob_cost ) := res.income( disab_liv_allowmob ) * 52.0;
         end if;
         if( res.income( attendance_allow ) > 0.0 )then
            s( aa_recipients ) := 1.0;
            s( aa_cost ) := res.income( attendance_allow ) * 52.0;
         end if;
         if( res.income( pension_credit ) > 0.0 )then
            s( pc_recipients ) := 1.0;
            s( pc_cost ) := res.income( pension_credit ) * 52.0;
         end if;
         if( res.income( ni_retir_pension ) > 0.0 )then
            s( pension_recipients ) := 1.0;
            s( pension_cost ) := res.income( ni_retir_pension ) * 52.0;
         end if;
         
         s( combined_benefit_spend ) := 
             s( pension_cost ) +
             s( pc_cost ) +
             s( aa_cost ) +
             s( dla_mob_cost ) +
             s( dla_care_cost );
         s( combined_benefit_recipients ) := 
             s( pension_recipients ) +
             s( pc_recipients ) +
             s( aa_recipients ) +
             s( dla_mob_recipients ) +
             s( dla_care_recipients );
             
         case ad.health_status is
            when poor => s( health_poor_or_very_poor ) := 1.0; 
                         s( health_poor ) := 1.0;
            when very_poor => s( health_poor_or_very_poor ) := 1.0; 
                              s( health_very_poor ) := 1.0;
            when others => null;
         end case;
   
         s( net_income ) := res.net_income;
         s( disposable_income ) := res.disposable_income;
         
         s( public_expenditure_net_of_user_charges_and_other_income ) := 
            s( residential_public_funding ) + 
            s( non_residential_public_funding );
            
         s( private_spend_on_social_care ) := 
            s( residential_private_funding ) + 
            s( non_residential_private_funding );
      end if; -- adults > 0
      return s;
   end Make_Summary_Items;

   procedure Zero( res : in out Personal_Result; clear_historical : Boolean ) is
   begin
      res.sernum := Sernum_Value'First;
      res.is_residential := False;
      res.receives_social_care :=  False;
      res.income :=  ( others => 0.0 );      
      if( clear_historical )then
         res.highest_previous_income := ( others => 0.0 );                  
      end if;
      res.la_contributions :=  0.0;
      res.client_contributions :=  0.0;
      res.gross_care_costs :=  0.0;
      res.total_payments_to_date :=  0.0;
      res.remaining_capital_stock :=  0.0;
      res.disposable_income :=  0.0;
      res.tarriff_income :=  0.0;
      res.net_income :=  0.0;
      res.marginal_rate :=  0.0;
      res.capital_contribution :=  0.0;
      res.minimum_income_guarantee :=  0.0;
      res.summary := ( others => 0.0 );
      res.costs_summary := ( others => 0.0 );
      res.intermediate.Clear;
      
      res.passes_residential_means_test := not_applicable;
      res.passes_non_residential_means_test := not_applicable;
      res.passes_residential_capital_test := not_applicable;
      res.passes_residential_income_test := not_applicable;
      res.passes_non_residential_income_test := not_applicable;
      res.passes_non_residential_capital_test := not_applicable;
      
      res.hours_of_care_la :=  0.0;
      res.hours_of_care_private :=  0.0;
      res.uap := UAP_Level'First;
      if( clear_historical )then
         res.lifetime_gross_payments := 0.0;
         res.lifetime_client_contributions := 0.0;
         res.lifetime_la_contributions := 0.0;
         res.lifetime_capital_contributions := 0.0;
      end if;
   end Zero;
   
   procedure Zero( hh : out Household_Result; clear_historical : Boolean ) is
   begin
      hh.res.sernum                   := Sernum_Value'First;
      hh.res.num_benefit_units        := Benefit_Unit_Count'First;
      hh.res.receives_social_care     := False;
      hh.res.income                   := ( others => 0.0 );
      hh.res.la_contributions :=  0.0;
      hh.res.tarriff_income :=  0.0;
      hh.res.client_contributions :=  0.0;
      hh.res.gross_care_costs :=  0.0;
      hh.res.total_payments_to_date :=  0.0;
      hh.res.remaining_capital_stock :=  0.0;
      
      hh.res.disposable_income :=  0.0;
      hh.res.net_income :=  0.0;
      hh.res.marginal_rate :=  0.0;
      hh.res.capital_contribution :=  0.0;
      hh.res.minimum_income_guarantee :=  0.0;
      hh.res.tarriff_income := 0.0;

      hh.res.equivalence_scale :=  0.0;
      hh.res.hours_of_care_la :=  0.0;
      hh.res.hours_of_care_private :=  0.0;
      hh.res.costs_summary            := ( others => 0.0 );
      hh.res.summary                  := ( others => 0.0 );
      if( clear_historical )then
         hh.res.lifetime_gross_payments := 0.0;
         hh.res.lifetime_client_contributions := 0.0;
         hh.res.lifetime_la_contributions := 0.0;
         hh.res.lifetime_capital_contributions := 0.0;
      end if;
      hh.res.intermediate.Clear;
      for buno in Benefit_Unit_Number loop
         hh.benefit_units( buno ).res.num_people := 0;
         hh.benefit_units( buno ).res.receives_social_care := False;
         hh.benefit_units( buno ).res.income := ( others => 0.0 );
         hh.benefit_units( buno ).res.la_contributions :=  0.0;
         hh.benefit_units( buno ).res.client_contributions :=  0.0;
         hh.benefit_units( buno ).res.gross_care_costs :=  0.0;
         hh.benefit_units( buno ).res.total_payments_to_date :=  0.0;
         hh.benefit_units( buno ).res.remaining_capital_stock :=  0.0;
         
         hh.benefit_units( buno ).res.disposable_income :=  0.0;
         hh.benefit_units( buno ).res.net_income :=  0.0;
         hh.benefit_units( buno ).res.marginal_rate :=  0.0;
         hh.benefit_units( buno ).res.capital_contribution :=  0.0;
         hh.benefit_units( buno ).res.minimum_income_guarantee :=  0.0;
         hh.benefit_units( buno ).res.equivalence_scale :=  0.0;
         hh.benefit_units( buno ).res.hours_of_care_la :=  0.0;
         hh.benefit_units( buno ).res.hours_of_care_private :=  0.0;
         hh.benefit_units( buno ).res.summary := ( others => 0.0 );
         hh.benefit_units( buno ).res.costs_summary := ( others => 0.0 );
         hh.benefit_units( buno ).res.intermediate.Clear;
         if( clear_historical )then
            hh.benefit_units( buno ).res.lifetime_gross_payments := 0.0;
            hh.benefit_units( buno ).res.lifetime_client_contributions := 0.0;
            hh.benefit_units( buno ).res.lifetime_la_contributions := 0.0;
            hh.benefit_units( buno ).res.lifetime_capital_contributions := 0.0;
         end if;
         for pno in Adult_Number loop
            Zero( hh.benefit_units( buno ).people(  pno  ), clear_historical );
            if( clear_historical )then
               Zero( hh.benefit_units( buno ).people_last_period(  pno  ), clear_historical );
            end if;
         end loop;
      end loop;
   end Zero;

   function Calculate_Income(
      bu  : Benefit_Unit; 
      res : Benefit_Unit_Result;
      which_to_include : Included_Incomes_Array ) return Amount is
      inc : Amount := 0.0;
   begin
      for adno in 1 .. bu.num_adults loop
         inc := inc + Calculate_Income( 
            bu.adults( adno ).current_income, 
            res.people( adno ).income,
            which_to_include );
         -- if( use_current )then
         -- else
            -- inc := inc + Calculate_Income( 
               -- bu.adults( adno ).annual_income, 
               -- res.people(  adno  ).income,
               -- which_to_include );
         -- end if;
         -- FIXME We need to add children's calculated incomes to Model.WSC.Results;
         -- and loop here
      end loop;
      return inc;
   end Calculate_Income;
   
   procedure Accumulate( 
      hh       : Model.WSC.Household.Household; 
      res      : in out Household_Result; 
      wsc_run : Run ) is
         
       use WSC_Enums.Calculated_Incomes_Package;
   begin
     res.res.num_benefit_units := hh.num_benefit_units;
     for b in 1 .. hh.num_benefit_units loop
        res.benefit_units( b ).res.num_people := hh.benefit_units( b ).num_adults;
        for a in 1 .. hh.benefit_units( b ).num_adults loop
           res.benefit_units( b ).people( a ).net_income := 
               Calculate_Income( 
                  hh.benefit_units( b ).adults( a ).current_income,
                  res.benefit_units( b ).people( a ).income,
                  NET_INCOME_LIST );
            res.res.income := res.res.income + res.benefit_units( b ).people( a ).income;
            res.benefit_units( b ).res.income := res.benefit_units( b ).res.income + res.benefit_units( b ).people( a ).income;
            
            res.benefit_units( b ).people(  a  ).disposable_income := res.benefit_units( b ).people(  a  ).net_income +
               res.benefit_units( b ).people( a ).la_contributions - res.benefit_units( b ).people( a ).client_contributions;
   
            res.benefit_units( b ).res.net_income := res.benefit_units( b ).res.net_income + res.benefit_units( b ).people( a ).net_income;
            res.res.net_income := res.res.net_income + res.benefit_units( b ).people( a ).net_income;

            res.benefit_units( b ).res.tarriff_income := res.benefit_units( b ).res.tarriff_income + res.benefit_units( b ).people( a ).tarriff_income;
            res.res.tarriff_income := res.res.tarriff_income + res.benefit_units( b ).people( a ).tarriff_income;
            
            res.benefit_units( b ).res.la_contributions := res.benefit_units( b ).res.la_contributions + res.benefit_units( b ).people( a ).la_contributions;
            res.res.la_contributions := res.res.la_contributions + res.benefit_units( b ).people( a ).la_contributions;
                  
            res.benefit_units( b ).res.lifetime_gross_payments := res.benefit_units( b ).res.lifetime_gross_payments + res.benefit_units( b ).people( a ).lifetime_gross_payments; 
            res.benefit_units( b ).res.lifetime_client_contributions := res.benefit_units( b ).res.lifetime_client_contributions + res.benefit_units( b ).people( a ).lifetime_client_contributions; 
            res.benefit_units( b ).res.lifetime_la_contributions := res.benefit_units( b ).res.lifetime_la_contributions + res.benefit_units( b ).people( a ).lifetime_la_contributions; 

            res.res.lifetime_gross_payments := res.res.lifetime_gross_payments + res.benefit_units( b ).people( a ).lifetime_gross_payments; 
            res.res.lifetime_client_contributions := res.res.lifetime_client_contributions + res.benefit_units( b ).people( a ).lifetime_client_contributions; 
            res.res.lifetime_la_contributions := res.res.lifetime_la_contributions + res.benefit_units( b ).people( a ).lifetime_la_contributions; 
          
            res.benefit_units( b ).res.hours_of_care_la := res.benefit_units( b ).res.hours_of_care_la + res.benefit_units( b ).people( a ).hours_of_care_la;                      
            res.benefit_units( b ).res.hours_of_care_private := res.benefit_units( b ).res.hours_of_care_private + res.benefit_units( b ).people( a ).hours_of_care_private;
            res.res.hours_of_care_la := res.res.hours_of_care_la + res.benefit_units( b ).people( a ).hours_of_care_la;                      
            res.res.hours_of_care_private := res.res.hours_of_care_private + res.benefit_units( b ).people( a ).hours_of_care_private;
            
            res.benefit_units( b ).res.gross_care_costs := res.benefit_units( b ).res.gross_care_costs + res.benefit_units( b ).people( a ).gross_care_costs;
            res.res.gross_care_costs := res.res.gross_care_costs + res.benefit_units( b ).people( a ).gross_care_costs;
            
            res.benefit_units( b ).res.client_contributions := res.benefit_units( b ).res.client_contributions + res.benefit_units( b ).people( a ).client_contributions;
            res.res.client_contributions := res.res.client_contributions + res.benefit_units( b ).people( a ).client_contributions;
            
            res.benefit_units( b ).res.disposable_income := res.benefit_units( b ).res.disposable_income + res.benefit_units( b ).people( a ).disposable_income;
            res.res.disposable_income := res.res.disposable_income + res.benefit_units( b ).people( a ).disposable_income;

            res.benefit_units( b ).res.total_payments_to_date := res.benefit_units( b ).res.total_payments_to_date + res.benefit_units( b ).people( a ).total_payments_to_date;
            res.res.total_payments_to_date := res.res.total_payments_to_date + res.benefit_units( b ).people( a ).total_payments_to_date;
            
            res.benefit_units( b ).res.remaining_capital_stock := res.benefit_units( b ).res.remaining_capital_stock + res.benefit_units( b ).people( a ).remaining_capital_stock;
            res.res.remaining_capital_stock := res.res.remaining_capital_stock + res.benefit_units( b ).people( a ).remaining_capital_stock;
            
            
            -- don't accumulate MIGs!
            -- res.benefit_units( b ).minimum_income_guarantee := res.benefit_units( b ).minimum_income_guarantee + res.benefit_units( b ).people( a ).minimum_income_guarantee;
            -- res.minimum_income_guarantee := res.minimum_income_guarantee + res.benefit_units( b ).people( a ).minimum_income_guarantee;
            -- FIXME gross,net,contributions
            if( res.benefit_units( b ).people( a ).is_residential )then
               res.res.costs_summary( social_care_residential ) := res.res.costs_summary( social_care_residential ) + 
                  res.benefit_units( b ).people( a ).la_contributions;
               res.benefit_units( b ).res.costs_summary( social_care_residential ) := res.benefit_units( b ).res.costs_summary( social_care_residential ) + res.benefit_units( b ).people( a ).la_contributions;
            else
               res.res.costs_summary( social_care_non_residential ) := res.res.costs_summary( social_care_non_residential ) + res.benefit_units( b ).people( a ).la_contributions;
               res.benefit_units( b ).res.costs_summary( social_care_non_residential ) := res.benefit_units( b ).res.costs_summary( social_care_non_residential ) + res.benefit_units( b ).people( a ).la_contributions;
            end if;
            res.benefit_units( b ).res.costs_summary( attendance_allowance ) := res.benefit_units( b ).res.costs_summary( attendance_allowance ) + res.benefit_units( b ).people( a ).income( attendance_allow );
            res.benefit_units( b ).res.costs_summary( state_pension ) := res.benefit_units( b ).res.costs_summary( state_pension ) + res.benefit_units( b ).people( a ).income( ni_retir_pension );
            res.benefit_units( b ).res.costs_summary( savings_credit ) := res.benefit_units( b ).res.costs_summary( savings_credit ) + res.benefit_units( b ).people( a ).income( pension_credit );
            -- guaranteed_pension_credit,
            res.benefit_units( b ).res.costs_summary( dla_mobility ) := res.benefit_units( b ).res.costs_summary( dla_mobility ) + res.benefit_units( b ).people( a ).income( disab_liv_allowmob );
            res.benefit_units( b ).res.costs_summary( dla_care ) := res.benefit_units( b ).res.costs_summary( dla_care ) + res.benefit_units( b ).people( a ).income( disab_liv_allowcare );

            res.res.costs_summary( attendance_allowance ) := res.res.costs_summary( attendance_allowance ) + res.benefit_units( b ).people( a ).income( attendance_allow );
            res.res.costs_summary( state_pension ) := res.res.costs_summary( state_pension ) + res.benefit_units( b ).people( a ).income( ni_retir_pension );
            res.res.costs_summary( savings_credit ) := res.res.costs_summary( savings_credit ) + res.benefit_units( b ).people( a ).income( pension_credit );
            -- guaranteed_pension_credit,
            res.res.costs_summary( dla_mobility ) := res.res.costs_summary( dla_mobility ) + res.benefit_units( b ).people( a ).income( disab_liv_allowmob );
            res.res.costs_summary( dla_care ) := res.res.costs_summary( dla_care ) + res.benefit_units( b ).people( a ).income( disab_liv_allowcare );
             
        end loop;
     end loop;        
   end Accumulate;
   
   function Difference( res1, res2 : Household_Result ) return Household_Result is
      use WSC_Enums.Calculated_Incomes_Package;
      diff : Household_Result;
   begin
      for i in Calculated_Incomes loop                              
         diff.res.income( i ) := res2.res.income( i ) - res1.res.income( i );
      end loop;
      diff.res.client_contributions := res2.res.client_contributions - res1.res.client_contributions; 
      diff.res.la_contributions := res2.res.la_contributions - res1.res.la_contributions; 
      diff.res.total_payments_to_date := res2.res.total_payments_to_date - res1.res.total_payments_to_date; 
      diff.res.remaining_capital_stock := res2.res.remaining_capital_stock - res1.res.remaining_capital_stock; 
      diff.res.disposable_income := res2.res.disposable_income - res1.res.disposable_income; 
      diff.res.net_income := res2.res.net_income - res1.res.net_income;
      diff.res.tarriff_income := res2.res.tarriff_income - res1.res.tarriff_income;
      diff.res.marginal_rate := res2.res.marginal_rate - res1.res.marginal_rate;
      diff.res.capital_contribution := res2.res.capital_contribution - res1.res.capital_contribution;
      diff.res.minimum_income_guarantee := res2.res.minimum_income_guarantee - res1.res.minimum_income_guarantee; 
      diff.res.hours_of_care_la  := res2.res.hours_of_care_la - res1.res.hours_of_care_la;
      diff.res.hours_of_care_private := res2.res.hours_of_care_private - res1.res.hours_of_care_private; 
          
      diff.res.lifetime_gross_payments := res2.res.lifetime_gross_payments - res1.res.lifetime_gross_payments; 
      diff.res.lifetime_client_contributions := res2.res.lifetime_client_contributions - res1.res.lifetime_client_contributions; 
      diff.res.lifetime_la_contributions := res2.res.lifetime_la_contributions - res1.res.lifetime_la_contributions;
      
      
      for b in 1 .. res1.res.num_benefit_units loop
         diff.benefit_units( b ).res.client_contributions := res2.benefit_units( b ).res.client_contributions - res1.benefit_units( b ).res.client_contributions;
         diff.benefit_units( b ).res.la_contributions := res2.benefit_units( b ).res.la_contributions - res1.benefit_units( b ).res.la_contributions;
         diff.benefit_units( b ).res.tarriff_income := res2.benefit_units( b ).res.tarriff_income - res1.benefit_units( b ).res.tarriff_income;
         diff.benefit_units( b ).res.gross_care_costs := res2.benefit_units( b ).res.gross_care_costs - res1.benefit_units( b ).res.gross_care_costs;
         diff.benefit_units( b ).res.disposable_income := res2.benefit_units( b ).res.disposable_income - res1.benefit_units( b ).res.disposable_income;
         diff.benefit_units( b ).res.net_income := res2.benefit_units( b ).res.net_income - res1.benefit_units( b ).res.net_income;
         diff.benefit_units( b ).res.marginal_rate := res2.benefit_units( b ).res.marginal_rate - res1.benefit_units( b ).res.marginal_rate;
         diff.benefit_units( b ).res.capital_contribution := res2.benefit_units( b ).res.capital_contribution - res1.benefit_units( b ).res.capital_contribution;
         diff.benefit_units( b ).res.minimum_income_guarantee := res2.benefit_units( b ).res.minimum_income_guarantee - res1.benefit_units( b ).res.minimum_income_guarantee;
         diff.benefit_units( b ).res.hours_of_care_la  :=res2.benefit_units( b ).res.hours_of_care_la - res1.benefit_units( b ).res.hours_of_care_la;
         diff.benefit_units( b ).res.hours_of_care_private := res2.benefit_units( b ).res.hours_of_care_private - res1.benefit_units( b ).res.hours_of_care_private;

         diff.benefit_units( b ).res.lifetime_gross_payments := res2.benefit_units( b ).res.lifetime_gross_payments - res1.benefit_units( b ).res.lifetime_gross_payments;
         diff.benefit_units( b ).res.lifetime_client_contributions := res2.benefit_units( b ).res.lifetime_client_contributions - res1.benefit_units( b ).res.lifetime_client_contributions;
         diff.benefit_units( b ).res.lifetime_la_contributions := res2.benefit_units( b ).res.lifetime_la_contributions - res1.benefit_units( b ).res.lifetime_la_contributions;
         diff.benefit_units( b ).res.lifetime_capital_contributions := res2.benefit_units( b ).res.lifetime_capital_contributions - res1.benefit_units( b ).res.lifetime_capital_contributions;

         for i in Calculated_Incomes loop
            diff.benefit_units( b ).res.income( i ) := res2.benefit_units( b ).res.income( i ) - res1.benefit_units( b ).res.income( i );
         end loop;
         for a in 1 .. res1.benefit_units( b ).res.num_people loop
            diff.benefit_units( b ).people( a ).client_contributions := 
               res2.benefit_units( b ).people( a ).client_contributions - 
               res1.benefit_units( b ).people( a ).client_contributions;
            diff.benefit_units( b ).people( a ).la_contributions := 
               res2.benefit_units( b ).people( a ).la_contributions - 
               res1.benefit_units( b ).people( a ).la_contributions;
            diff.benefit_units( b ).people( a ).tarriff_income := 
               res2.benefit_units( b ).people( a ).tarriff_income - 
               res1.benefit_units( b ).people( a ).tarriff_income;
            diff.benefit_units( b ).people( a ).gross_care_costs := 
               res2.benefit_units( b ).people( a ).gross_care_costs - 
               res1.benefit_units( b ).people( a ).gross_care_costs;

            diff.benefit_units( b ).people( a ).disposable_income := 
               res2.benefit_units( b ).people( a ).disposable_income - 
               res1.benefit_units( b ).people( a ).disposable_income;
            diff.benefit_units( b ).people( a ).net_income := 
               res2.benefit_units( b ).people( a ).net_income - 
               res1.benefit_units( b ).people( a ).net_income;
            diff.benefit_units( b ).people( a ).marginal_rate := 
               res2.benefit_units( b ).people( a ).marginal_rate - 
               res1.benefit_units( b ).people( a ).marginal_rate;
            diff.benefit_units( b ).people( a ).capital_contribution := 
               res2.benefit_units( b ).people( a ).capital_contribution - 
               res1.benefit_units( b ).people( a ).capital_contribution;
               
            diff.benefit_units( b ).people( a ).hours_of_care_la  := 
                 res2.benefit_units( b ).people( a ).hours_of_care_la - 
                 res1.benefit_units( b ).people( a ).hours_of_care_la;
            diff.benefit_units( b ).people( a ).hours_of_care_private := 
                  res2.benefit_units( b ).people( a ).hours_of_care_private - 
                  res1.benefit_units( b ).people( a ).hours_of_care_private;

            diff.benefit_units( b ).people( a ).lifetime_gross_payments := 
               res2.benefit_units( b ).people( a ).lifetime_gross_payments - 
               res1.benefit_units( b ).people( a ).lifetime_gross_payments;
            diff.benefit_units( b ).people( a ).lifetime_client_contributions := 
               res2.benefit_units( b ).people( a ).lifetime_client_contributions - 
               res1.benefit_units( b ).people( a ).lifetime_client_contributions;
            diff.benefit_units( b ).people( a ).lifetime_la_contributions := 
               res2.benefit_units( b ).people( a ).lifetime_la_contributions - 
               res1.benefit_units( b ).people( a ).lifetime_la_contributions;
            diff.benefit_units( b ).people( a ).lifetime_capital_contributions := 
               res2.benefit_units( b ).people( a ).lifetime_capital_contributions - 
               res1.benefit_units( b ).people( a ).lifetime_capital_contributions;
               
            for i in Calculated_Incomes loop
               diff.benefit_units( b ).people( a ).income( i ) := 
                    res2.benefit_units( b ).people( a ).income( i ) - 
                    res1.benefit_units( b ).people( a ).income( i );
            end loop;   
         end loop;
      end loop;
      return diff;
   end Difference;

   function To_String( 
      res : Personal_Result; 
      include_non_zeros : Boolean := False ) return String is
      s : Unbounded_String;
   begin
      s := s & "    PID " & res.sernum'Img & Line_BREAK; 
      s := s & "    Local Authority Contributions :" & Format_With_Commas( res.la_contributions ) & Line_BREAK; 
      s := s & "    Client Contributions :" & Format_With_Commas( res.client_contributions ) & Line_BREAK; 
      s := s & "    Local Authority Contributions :" & Format_With_Commas( res.la_contributions ) & Line_BREAK; 
      s := s & "    Tarriff Income :" & Format_With_Commas( res.tarriff_income ) & Line_BREAK; 
      s := s & "    Gross Care Costs :" & Format_With_Commas( res.gross_care_costs ) & Line_BREAK; 
      s := s & "    Total Payments to Date :"  & Format_With_Commas( res.total_payments_to_date ) & LINE_BREAK; 
      s := s & "    Disposable Income :" & Format_With_Commas( res.disposable_income ) & Line_BREAK; 
      s := s & "    Net Income :" & Format_With_Commas( res.net_income ) & Line_BREAK; 
      s := s & "    Marginal Rate :" & Format_With_Commas( res.marginal_rate ) & Line_BREAK; 
      s := s & "    Capital Contribution :" & Format_With_Commas( res.capital_contribution ) & Line_BREAK; 
      s := s & "    Passes Residential Means Test " & res.passes_residential_means_test'Img & Line_Break;
      s := s & "    Passes Non Residential Means Test " & res.passes_non_residential_means_test'Img & Line_Break;
      s := s & "    Hours of Care From LA " & Format( res.hours_of_care_la ) & Line_Break;
      s := s & "    Private Hours of Care  " & Format( res.hours_of_care_private ) & Line_Break;
      s := s & "    UAP Level  " & UAP_Level'Image( res.uap ) & Line_Break;
      s := s & "    Residential: Passes Income Test  " & Means_Test_Result'Image( res.passes_residential_income_test ) & Line_Break;
      s := s & "    Residential: Passes Capital Test  " & Means_Test_Result'Image( res.passes_residential_capital_test ) & Line_Break;
      s := s & "    Non Residential: Passes Income Test  " & Means_Test_Result'Image( res.passes_non_residential_income_test ) & Line_Break;
      s := s & "    Non Residential: Passes Capital Test  " & Means_Test_Result'Image( res.passes_non_residential_capital_test ) & Line_Break;
      
      s := s & "    Highest LA Contribution " & Format( res.highest_la_contribution ) & LINE_BREAK;
      
      s := s & "    Lifetime Gross Costs " & Format( res.lifetime_gross_payments ) & LINE_BREAK;
      s := s & "    Lifetime Client Contributions " & Format( res.lifetime_client_contributions  ) & LINE_BREAK;
      s := s & "    Lifetime LA Contributions " & Format( res.lifetime_la_contributions ) & LINE_BREAK;
      s := s & "    Remaining Capital Stock :"  & Format_With_Commas( res.remaining_capital_stock  ) & LINE_BREAK;
      s := s & "    Lifetime Capital Contributions " & Format( res.lifetime_capital_contributions ) & LINE_BREAK;
      s := s & "    Incomes " & LINE_BREAK;
      for i in Calculated_Incomes loop
         if( res.income( i ) /= 0.0 or include_non_zeros )then
            s := s & "      " & To_String( i ) & " : " & Format( res.income( i )) & LINE_BREAK;
         end if;
      end loop;
      
      s := s & "    Highest Previous Incomes " & LINE_BREAK;
      for i in Calculated_Incomes loop
         if( res.highest_previous_income( i ) /= 0.0 or include_non_zeros )then
            s := s & "      " & To_String( i ) & " : " & Format( res.highest_previous_income( i )) & LINE_BREAK;
         end if;
      end loop;
      s := s & " INTERMEDIATE PERSONAL RESULTS " & LINE_BREAK;
      s := s & To_String( res.intermediate, "      " ); 
      return TS( s );  
   end To_String;
      
   function To_String( 
      bures : Benefit_Unit_Result; 
      include_non_zeros : Boolean := False ) return String is
      s : Unbounded_String;
   begin
      s := s & "  Gross Care Costs :" & Format_With_Commas( bures.res.gross_care_costs ) & Line_BREAK; 
      s := s & "  Client Contributions :" & Format_With_Commas( bures.res.client_contributions ) & Line_BREAK; 
      s := s & "  Local Authority Contributions :" & Format_With_Commas( bures.res.la_contributions ) & Line_BREAK; 
      s := s & "  Tarriff Income :" & Format_With_Commas( bures.res.tarriff_income ) & Line_BREAK; 
      s := s & "  Total Payments to Date :"  & Format_With_Commas( bures.res.total_payments_to_date  ) & LINE_BREAK;
      s := s & "  Remaining Capital Stock :"  & Format_With_Commas( bures.res.remaining_capital_stock  ) & LINE_BREAK;
      s := s & "  Disposable Income :" & Format_With_Commas( bures.res.disposable_income ) & Line_BREAK; 
      s := s & "  Net Income :" & Format_With_Commas( bures.res.net_income ) & Line_BREAK; 
      s := s & "  Marginal Rate :" & Format_With_Commas( bures.res.marginal_rate ) & Line_BREAK; 
      s := s & "  Capital Contribution :" & Format_With_Commas( bures.res.capital_contribution ) & Line_BREAK; 
      s := s & "  Hours of Care From LA " & Format( bures.res.hours_of_care_la ) & Line_Break;
      s := s & "  Private Hours of Care  " & Format( bures.res.hours_of_care_private ) & Line_Break;
      s := s & "  Lifetime Gross Costs " & Format( bures.res.lifetime_gross_payments ) & LINE_BREAK;
      s := s & "  Lifetime Client Contributions " & Format( bures.res.lifetime_client_contributions  ) & LINE_BREAK;
      s := s & "  Lifetime LA Contributions " & Format( bures.res.lifetime_la_contributions ) & LINE_BREAK;
      s := s & "  Lifetime Capital Contributions " & Format( bures.res.lifetime_capital_contributions ) & LINE_BREAK;
      s := s & "  Incomes " & LINE_BREAK;
      for i in Calculated_Incomes loop
         if( bures.res.income( i ) /= 0.0 or include_non_zeros )then
            s := s & "    " & To_String( i ) & " : " & Format( bures.res.income( i )) & LINE_BREAK;
         end if;
      end loop;
      for pno in 1 .. bures.res.num_people loop
         s := s & " PERSON " & Format( pno ) & LINE_BREAK;
         s := s & To_String( bures.people( pno )) & LINE_BREAK;         
         s := s & " PERSON " & Format( pno ) & " PREVIOUS PERIOD " & LINE_BREAK;
         s := s & To_String( bures.people_last_period( pno )) & LINE_BREAK;         
      end loop;
      s := s & " INTERMEDIATE BU RESULTS " & LINE_BREAK;
      s := s & To_String( bures.res.intermediate, "   " ); 
      return TS( s );  
   end To_String;

   function To_String( 
      res : Household_Result; 
      include_non_zeros : Boolean := False ) return String  is
      s : Unbounded_String;
   begin
      s := s & "Local Authority Contributions :" & Format_With_Commas( res.res.la_contributions ) & Line_BREAK; 
      s := s & "Client Contributions :" & Format_With_Commas( res.res.client_contributions ) & Line_BREAK; 
      s := s & "Tarriff Income :" & Format_With_Commas( res.res.tarriff_income ) & Line_BREAK; 
      s := s & "Gross Care Costs:" & Format_With_Commas( res.res.gross_care_costs ) & Line_BREAK; 
      s := s & "Total Payments to Date :"  & Format_With_Commas( res.res.total_payments_to_date  ) & LINE_BREAK;
      s := s & "Remaining Capital Stock :"  & Format_With_Commas( res.res.remaining_capital_stock  ) & LINE_BREAK;
      s := s & "Disposable Income :" & Format_With_Commas( res.res.disposable_income ) & Line_BREAK; 
      s := s & "Net Income :" & Format_With_Commas( res.res.net_income ) & Line_BREAK; 
      s := s & "Marginal Rate :" & Format_With_Commas( res.res.marginal_rate ) & Line_BREAK; 
      s := s & "Capital Contribution :" & Format_With_Commas( res.res.capital_contribution ) & Line_BREAK; 
      s := s & "Hours of Care From LA " & Format( res.res.hours_of_care_la ) & Line_Break;
      s := s & "Private Hours of Care  " & Format( res.res.hours_of_care_private ) & Line_Break;
      s := s & "Lifetime Gross Costs " & Format( res.res.lifetime_gross_payments ) & LINE_BREAK;
      s := s & "Lifetime Client Contributions " & Format( res.res.lifetime_client_contributions  ) & LINE_BREAK;
      s := s & "Lifetime Capital Contributions " & Format( res.res.lifetime_capital_contributions  ) & LINE_BREAK;
      s := s & "Lifetime LA Contributions " & Format( res.res.lifetime_la_contributions ) & LINE_BREAK;
      s := s & "Incomes " & LINE_BREAK;
      for i in Calculated_Incomes loop
         if( res.res.income( i ) /= 0.0 or include_non_zeros )then
            s := s & To_String( i ) & " : " & Format( res.res.income( i )) & LINE_BREAK;
         end if;
      end loop;
      for buno in 1 .. res.res.num_benefit_units loop
         s := s & " BENEFIT UNIT " & Format( buno ) & LINE_BREAK;
         s := s & To_String( res.benefit_units( buno )) & LINE_BREAK;         
      end loop;
      s := s & " INTERMEDIATE HOUSEHOLD RESULTS " & LINE_BREAK;
      s := s & To_String( res.res.intermediate, "" ); 
      return TS( s );  
   end To_String;
   
   function Compare_Results_Equal( pre, post : Personal_Result ) return Boolean is
      use Calculated_Incomes_Package;
   begin
      if( pre.is_residential /= post.is_residential )then return False; end if;
      if( pre.receives_social_care /= post.receives_social_care )then return False; end if;
      if( pre.income /= post.income )then return False; end if;
      if( pre.la_contributions /= post.la_contributions )then return False; end if;
      if( pre.gross_care_costs /= post.gross_care_costs )then return False; end if;
      if( pre.client_contributions /= post.client_contributions )then return False; end if;
      if( pre.disposable_income /= post.disposable_income )then return False; end if;
      if( pre.net_income /= post.net_income )then return False; end if;
      if( pre.marginal_rate /= post.marginal_rate )then return False; end if;
      if( pre.capital_contribution /= post.capital_contribution )then return False; end if;
      if( pre.minimum_income_guarantee /= post.minimum_income_guarantee )then return False; end if;
      return True;
   end Compare_Results_Equal;

   procedure Free_Household_Result is new Ada.Unchecked_Deallocation( 
      Object => Household_Result, 
      Name => Household_Result_Access );
   
   procedure Free( hh : in out Household_Result_Access ) is
   begin
      Free_Household_Result( hh );
   end Free;

end Model.WSC.Results;
