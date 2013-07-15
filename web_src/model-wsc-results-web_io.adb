with Ada.Strings.Unbounded;
with Model.WSC.Household.Web_IO;
with Model.WSC.Formatting;
with Templates_Parser;
with Model.WSC.Global_Settings;
with Text_Utils;
with Web_Utils;
with WSC_Enums;

package body Model.WSC.Results.Web_IO is

   use Ada.Strings.Unbounded;
   use Model.WSC.Household.Web_IO;
   use Text_Utils;
   use Templates_Parser;
   
   function To_String( caption: String; p1, p2, diff : Calculated_Incomes_Array ) return String is
      translations : Translate_Set;
      labels : Vector_Tag;
      prev   : Vector_Tag;
      postv  : Vector_Tag;
      diffv  : Vector_Tag;
      n      : Natural := 0;

   begin
      for i in Calculated_Incomes loop
         if( p1( i ) /= 0.0 ) or ( p2( i ) /= 0.0 ) then
            labels := labels & Prettify_Image( Incomes_Type'Image( i ));
            prev := prev & Format( p1( i ));
            postv := postv & Format( p2( i ));
            diffv := diffv & Format( diff( i ));
            n := n + 1;
         end if;         
      end loop;
      if( n > 0 )then
         Insert( translations, Assoc( "CAPTION", caption ));
         Insert( translations, Assoc( "LABEL", labels ));
         Insert( translations, Assoc( "PRE", prev ));
         Insert( translations, Assoc( "POST", postv ));
         Insert( translations, Assoc( "DIFF", diffv ));
         return 
            Web_Utils.Parse_Template( 
               Model.WSC.Global_Settings.template_components_path & 
               Model.WSC.Global_Settings.dir_separator & "results_table", 
               translations );
      else
         return "";
      end if;   
   end To_String;
   
   function To_String( p1, p2, diff : Personal_Result ) return String is
      translations : Translate_Set;
      main_translations : Translate_Set;
      labels : Vector_Tag;
      prev : Vector_Tag;
      postv : Vector_Tag;
      diffv : Vector_Tag;
   begin
      if( p1.la_contributions /= 0.0 or p2.la_contributions /= 0.0 )then 
         labels := labels & "Local Authority Contributions";
         prev := prev & Format_With_Commas( p1.la_contributions  );
         postv := postv & Format_With_Commas( p2.la_contributions  );
         diffv := diffv & Format_With_Commas( diff.la_contributions  ); 
      end if;
      
      if( p1.client_contributions /= 0.0 or p2.client_contributions /= 0.0 )then 
         labels := labels & "Client Contributions";
         prev := prev & Format_With_Commas( p1.client_contributions  );
         postv := postv & Format_With_Commas( p2.client_contributions  );
         diffv := diffv & Format_With_Commas( diff.client_contributions  ); 
      end if;
      
      
      if( p1.tarriff_income /= 0.0 or p2.tarriff_income /= 0.0 )then 
         labels := labels & "Tarriff Income";
         prev := prev & Format_With_Commas( p1.tarriff_income  );
         postv := postv & Format_With_Commas( p2.tarriff_income  );
         diffv := diffv & Format_With_Commas( diff.tarriff_income  ); 
      end if;
      
      if( p1.gross_care_costs /= 0.0 or p2.gross_care_costs /= 0.0 )then 
         labels := labels & "Gross Care Costs";
         prev := prev & Format_With_Commas( p1.gross_care_costs  );
         postv := postv & Format_With_Commas( p2.gross_care_costs  );
         diffv := diffv & Format_With_Commas( diff.gross_care_costs  ); 
      end if;
      
      if( p1.total_payments_to_date /= 0.0 or p2.total_payments_to_date /= 0.0 )then   
         labels := labels & "Total Payments to Date";
         prev := prev & Format_With_Commas( p1.total_payments_to_date );
         postv := postv & Format_With_Commas( p2.total_payments_to_date );
         diffv := diffv & Format_With_Commas( diff.total_payments_to_date );
      end if;
      
      if( p1.remaining_capital_stock /= 0.0 or p2.remaining_capital_stock /= 0.0 )then   
         labels := labels & "Remaining Capital Stock";
         prev := prev & Format_With_Commas( p1.remaining_capital_stock );
         postv := postv & Format_With_Commas( p2.remaining_capital_stock );
         diffv := diffv & Format_With_Commas( diff.remaining_capital_stock );
      end if;
      
      
      if( p1.marginal_rate /= 0.0 or p2.marginal_rate /= 0.0 )then   
         labels := labels & "Marginal Rate";
         prev := prev & Format_With_Commas( p1.marginal_rate  );
         postv := postv & Format_With_Commas( p2.marginal_rate  );
         diffv := diffv & Format_With_Commas( diff.marginal_rate  ); 
      end if;
      
      if( p1.capital_contribution /= 0.0 or p2.capital_contribution /= 0.0 )then   
         labels := labels & "Capital Contribution";
         prev := prev & Format_With_Commas( p1.capital_contribution  );
         postv := postv & Format_With_Commas( p2.capital_contribution  );
         diffv := diffv & Format_With_Commas( diff.capital_contribution  ); 
      end if;
      
      if( p1.passes_residential_means_test /= not_applicable or p2.passes_residential_means_test /= not_applicable )then   
         labels := labels & "Passes Residential Means Test";
         prev := prev & Prettify_Image( p1.passes_residential_means_test'Img );
         postv := postv & Prettify_Image( p2.passes_residential_means_test'Img );
         diffv := diffv & "";

         labels := labels & "Residential: Passes Income Test";
         prev := prev & Prettify_Image( Means_Test_Result'Image( p1.passes_residential_income_test ));
         postv := postv & Prettify_Image( Means_Test_Result'Image( p2.passes_residential_income_test ));
         diffv := diffv & "";
   
         labels := labels & "Residential: Passes Capital Test";
         prev := prev & Prettify_Image( Means_Test_Result'Image( p1.passes_residential_capital_test ));
         postv := postv & Prettify_Image( Means_Test_Result'Image( p2.passes_residential_capital_test ));
         diffv := diffv & "";
      end if;
      
      if( p1.passes_non_residential_means_test  /= not_applicable or p2.passes_non_residential_means_test  /= not_applicable  )then   
         labels := labels & "Passes Non Residential Means Test";
         prev := prev & Prettify_Image( p1.passes_non_residential_means_test'Img );
         postv := postv & Prettify_Image( p2.passes_non_residential_means_test'Img );
         diffv := diffv & "";

         labels := labels & "Non Residential: Passes Income Test";
         prev := prev & Prettify_Image( Means_Test_Result'Image( p1.passes_non_residential_income_test ));
         postv := postv & Prettify_Image( Means_Test_Result'Image( p2.passes_non_residential_income_test ));
         diffv := diffv &  "";
   
         labels := labels & "Non Residential: Passes Capital Test";
         prev := prev & Prettify_Image( Means_Test_Result'Image( p1.passes_non_residential_capital_test ));
         postv := postv & Prettify_Image( Means_Test_Result'Image( p2.passes_non_residential_capital_test ));
         diffv := diffv & "";
      end if;
      
      if( p1.hours_of_care_la /= 0.0 or p2.hours_of_care_la /= 0.0 )then   
         labels := labels & "Hours of Care From LA";
         prev := prev & Format( p1.hours_of_care_la  );
         postv := postv & Format( p2.hours_of_care_la  );
         diffv := diffv & Format( diff.hours_of_care_la  );
      end if;
      
      if( p1.hours_of_care_private /= 0.0 or p2.hours_of_care_private /= 0.0 )then   
         labels := labels & "Private Hours of Care";
         prev := prev & Format( p1.hours_of_care_private  );
         postv := postv & Format( p2.hours_of_care_private  );
         diffv := diffv & Format( diff.hours_of_care_private  );
      end if;
      
      labels := labels & "UAP Level";
      prev := prev &  Prettify_Image( UAP_Level'Image( p1.uap  ));
      postv := postv &  Prettify_Image( UAP_Level'Image( p2.uap  ));
      diffv := diffv &  "";

      if( p1.highest_la_contribution /= 0.0 or p2.highest_la_contribution /= 0.0 )then 
         labels := labels & "Highest LA Contribution";
         prev := prev & Format( p1.highest_la_contribution  );
         postv := postv & Format( p2.highest_la_contribution  );
         diffv := diffv & Format( diff.highest_la_contribution  );
      end if;
  
      if( p1.lifetime_gross_payments /= 0.0 or p2.lifetime_gross_payments /= 0.0 )then 
         labels := labels & "Lifetime Gross Costs";
         prev := prev & Format( p1.lifetime_gross_payments  );
         postv := postv & Format( p2.lifetime_gross_payments  );
         diffv := diffv & Format( diff.lifetime_gross_payments  );
      end if;

      if( p1.lifetime_client_contributions  /= 0.0 or p2.lifetime_client_contributions  /= 0.0 )then
         labels := labels & "Lifetime Client Contributions";
         prev := prev & Format( p1.lifetime_client_contributions   );
         postv := postv & Format( p2.lifetime_client_contributions   );
         diffv := diffv & Format( diff.lifetime_client_contributions   );
      end if;

      if( p1.lifetime_la_contributions  /= 0.0 or p2.lifetime_la_contributions  /= 0.0 )then
         labels := labels & "Lifetime LA Contributions";
         prev := prev & Format( p1.lifetime_la_contributions  );
         postv := postv & Format( p2.lifetime_la_contributions  );
         diffv := diffv & Format( diff.lifetime_la_contributions  );
      end if;


      if( p1.lifetime_capital_contributions  /= 0.0 or p2.lifetime_capital_contributions  /= 0.0 )then
         labels := labels & "Lifetime Capital Contributions";
         prev := prev & Format( p1.lifetime_capital_contributions  );
         postv := postv & Format( p2.lifetime_capital_contributions  );
         diffv := diffv & Format( diff.lifetime_capital_contributions  );
      end if;
      
      labels := labels & "Disposable Income";
      prev := prev & Format_With_Commas( p1.disposable_income  );
      postv := postv & Format_With_Commas( p2.disposable_income  );
      diffv := diffv & Format_With_Commas( diff.disposable_income  ); 
   
      labels := labels & "Net Income";
      prev := prev & Format_With_Commas( p1.net_income  );
      postv := postv & Format_With_Commas( p2.net_income  );
      diffv := diffv & Format_With_Commas( diff.net_income  ); 

      Insert( translations, Assoc( "CAPTION", "Results" ));
      Insert( translations, Assoc( "LABEL", labels ));
      Insert( translations, Assoc( "PRE", prev ));
      Insert( translations, Assoc( "POST", postv ));
      Insert( translations, Assoc( "DIFF", diffv ));
      declare
         main_table   : constant String :=  Web_Utils.Parse_Template( 
            Model.WSC.Global_Settings.template_components_path & 
            Model.WSC.Global_Settings.dir_separator & "results_table", 
            translations );
         inc_table    : constant String := To_String( "Calculated Incomes", p1.income, p2.income, diff.income );
         hp_inc_table : constant String := To_String( "Highest Previous Income", 
            p1.highest_previous_income, 
            p2.highest_previous_income, 
            diff.highest_previous_income );
      begin
         Insert( main_translations, Assoc( "MAIN-RESULTS", main_table ));
         Insert( main_translations, Assoc( "INCOMES", inc_table ));
         Insert( main_translations, Assoc( "HIGH-PREV-INCOMES", hp_inc_table ));
      end;      
      return 
         Web_Utils.Parse_Template( 
            Model.WSC.Global_Settings.template_components_path & 
            Model.WSC.Global_Settings.dir_separator & "person_result", 
            main_translations );      
   end To_String;
   
   function To_String( h1, h2, diff : Household_Result ) return String is
      translations      : Translate_Set;
      main_translations : Translate_Set;
      labels            : Vector_Tag;
      prev              : Vector_Tag;
      postv             : Vector_Tag;
      diffv             : Vector_Tag;
      people            : Vector_Tag;
   begin
      
      if( h1.res.la_contributions /= 0.0 or h2.res.la_contributions /= 0.0 )then 
         labels := labels & "Local Authority Contributions";
         prev := prev & Format_With_Commas( h1.res.la_contributions  );
         postv := postv & Format_With_Commas( h2.res.la_contributions  );
         diffv := diffv & Format_With_Commas( diff.res.la_contributions  ); 
      end if;
      
      if( h1.res.client_contributions /= 0.0 or h2.res.client_contributions /= 0.0 )then  
         labels := labels & "Client Contributions";
         prev := prev & Format_With_Commas( h1.res.client_contributions  );
         postv := postv & Format_With_Commas( h2.res.client_contributions  );
         diffv := diffv & Format_With_Commas( diff.res.client_contributions  ); 
      end if;
      
      if( h1.res.tarriff_income /= 0.0 or h2.res.tarriff_income /= 0.0 )then 
         labels := labels & "Tarriff Income";
         prev := prev & Format_With_Commas( h1.res.tarriff_income  );
         postv := postv & Format_With_Commas( h2.res.tarriff_income  );
         diffv := diffv & Format_With_Commas( diff.res.tarriff_income  ); 
      end if;
      
      if( h1.res.gross_care_costs /= 0.0 or h2.res.gross_care_costs /= 0.0 )then 
         labels := labels & "Gross Care Costs";
         prev := prev & Format_With_Commas( h1.res.gross_care_costs  );
         postv := postv & Format_With_Commas( h2.res.gross_care_costs  );
         diffv := diffv & Format_With_Commas( diff.res.gross_care_costs  );
      end if;
   
      if( h1.res.total_payments_to_date /= 0.0 or h2.res.total_payments_to_date /= 0.0 )then
         labels := labels & "Total Payments to Date";
         prev := prev & Format_With_Commas( h1.res.total_payments_to_date  );
         postv := postv & Format_With_Commas( h2.res.total_payments_to_date );
         diffv := diffv & Format_With_Commas( diff.res.total_payments_to_date );
      end if;
      
      if( h1.res.remaining_capital_stock /= 0.0 or h2.res.remaining_capital_stock /= 0.0 )then
         labels := labels & "Remaining Capital Stock";
         prev := prev & Format_With_Commas( h1.res.remaining_capital_stock  );
         postv := postv & Format_With_Commas( h2.res.remaining_capital_stock );
         diffv := diffv & Format_With_Commas( diff.res.remaining_capital_stock );
      end if;

      
      if( h1.res.marginal_rate /= 0.0 or h2.res.marginal_rate /= 0.0 )then 
         labels := labels & "Marginal Rate";
         prev := prev & Format_With_Commas( h1.res.marginal_rate  );
         postv := postv & Format_With_Commas( h2.res.marginal_rate  );
         diffv := diffv & Format_With_Commas( diff.res.marginal_rate  ); 
      end if;
      
      if( h1.res.capital_contribution /= 0.0 or h2.res.capital_contribution /= 0.0 )then
         labels := labels & "Capital Contribution";
         prev := prev & Format_With_Commas( h1.res.capital_contribution  );
         postv := postv & Format_With_Commas( h2.res.capital_contribution  );
         diffv := diffv & Format_With_Commas( diff.res.capital_contribution  ); 
      end if;
      
      if( h1.res.hours_of_care_la /= 0.0 or h2.res.hours_of_care_la /= 0.0 )then   
         labels := labels & "Hours of Care From LA";
         prev := prev & Format( h1.res.hours_of_care_la  );
         postv := postv & Format( h2.res.hours_of_care_la  );
         diffv := diffv & Format( diff.res.hours_of_care_la  );
      end if;
      
      if( h1.res.hours_of_care_private /= 0.0 or h2.res.hours_of_care_private /= 0.0 )then   
         labels := labels & "Private Hours of Care";
         prev := prev & Format( h1.res.hours_of_care_private  );
         postv := postv & Format( h2.res.hours_of_care_private  );
         diffv := diffv & Format( diff.res.hours_of_care_private  );
      end if;
     
      if( h1.res.lifetime_gross_payments /= 0.0 or h2.res.lifetime_gross_payments /= 0.0 )then 
         labels := labels & "Lifetime Gross Costs";
         prev := prev & Format( h1.res.lifetime_gross_payments  );
         postv := postv & Format( h2.res.lifetime_gross_payments  );
         diffv := diffv & Format( diff.res.lifetime_gross_payments  );
      end if;
      
      if( h1.res.lifetime_client_contributions  /= 0.0 or h2.res.lifetime_client_contributions  /= 0.0 )then
         labels := labels & "Lifetime Client Contributions";
         prev := prev & Format( h1.res.lifetime_client_contributions   );
         postv := postv & Format( h2.res.lifetime_client_contributions   );
         diffv := diffv & Format( diff.res.lifetime_client_contributions   );
      end if;
      
      if( h1.res.lifetime_capital_contributions  /= 0.0 or h2.res.lifetime_capital_contributions  /= 0.0 )then
         labels := labels & "Lifetime Capital Contributions";
         prev := prev & Format( h1.res.lifetime_capital_contributions   );
         postv := postv & Format( h2.res.lifetime_capital_contributions   );
         diffv := diffv & Format( diff.res.lifetime_capital_contributions   );
      end if;
      
      if( h1.res.lifetime_la_contributions  /= 0.0 or h2.res.lifetime_la_contributions  /= 0.0 )then
         labels := labels & "Lifetime LA Contributions";
         prev := prev & Format( h1.res.lifetime_la_contributions  );
         postv := postv & Format( h2.res.lifetime_la_contributions  );
         diffv := diffv & Format( diff.res.lifetime_la_contributions  );
      end if;
      
      labels := labels & "Disposable Income";
      prev := prev & Format_With_Commas( h1.res.disposable_income  );
      postv := postv & Format_With_Commas( h2.res.disposable_income  );
      diffv := diffv & Format_With_Commas( diff.res.disposable_income  ); 
      
      labels := labels & "Net Income";
      prev := prev & Format_With_Commas( h1.res.net_income  );
      postv := postv & Format_With_Commas( h2.res.net_income  );
      diffv := diffv & Format_With_Commas( diff.res.net_income  ); 
      
      Insert( translations, Assoc( "CAPTION", "Household Data" ));
      
      Insert( translations, Assoc( "LABEL", labels ));
      Insert( translations, Assoc( "PRE", prev ));
      Insert( translations, Assoc( "POST", postv ));
      Insert( translations, Assoc( "DIFF", diffv ));
      declare
         inc_table  : constant String := To_String( "Calculated Household Incomes", h1.res.income, h2.res.income, diff.res.income );
         main_table : constant String := Web_Utils.Parse_Template( 
            Model.WSC.Global_Settings.template_components_path & 
            Model.WSC.Global_Settings.dir_separator & "results_table", 
            translations );      
      begin
         Insert( main_translations, Assoc( "MAIN-RESULTS", main_table ));
         Insert( main_translations, Assoc( "INCOMES", inc_table ));
      end;
      return 
         Web_Utils.Parse_Template( 
            Model.WSC.Global_Settings.template_components_path & 
            Model.WSC.Global_Settings.dir_separator & "household_results", 
            main_translations );            
   end To_String;
   
end Model.WSC.Results.Web_IO;
