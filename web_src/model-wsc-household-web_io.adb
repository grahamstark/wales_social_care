with Ada.Strings.Unbounded;

with AWS.URL;
with Model.WSC.Formatting;
with Model.WSC.Global_Settings;
with Templates_Parser;
with Text_Utils;
with Time_Format;
with WSC_Enums;
with Web_Utils;

package body Model.WSC.Household.Web_IO is

   use Model.WSC.Formatting;
   use Templates_Parser;
   use Text_Utils;
   use WSC_Enums;
   
  function Personal_Url( 
      wave      : Waves;
      hid       : Sernum_Value;
      pid       : Sernum_Value;
      iteration : Positive ) return String is
   use Ada.Strings.Unbounded;
      s : Unbounded_String;
   begin
      s := s & "wave="& Trim( Waves'Image( wave ));
      s := s & "&amp;hid=" & Trim( Sernum_Value'Image( hid ));
      s := s & "&amp;pid=" & Trim( Sernum_Value'Image( pid ));
      s := s & "&amp;iter=" & Trim( Positive'Image( iteration ));
      return TS( s );
   end Personal_Url;
      
  function Person_To_URL( 
      wave           : Waves;
      pers           : Person; 
      iteration      : Positive := 1; 
      which_direction : Direction := curr ) return String is
      w : Waves := wave;
   begin
      if( pers.dies_this_period ) and then which_direction = next then
         return "";
      end if;
      case which_direction is
      when next => 
            if w < Waves'Last then
               w := Waves'Succ( wave );
            end if;
      when prev =>
            if w < Waves'First then
               w := Waves'Pred( wave );
            end if;
      when curr =>
         w := wave;
      end case;
      return Personal_Url( wave, pers.hid, pers.pid, iteration );
   end Person_To_URL;
   
   procedure Get_Person_Ids_From_URL(
      params       : AWS.Parameters.List; 
      wave         : out Waves; 
      -- dataset_name : out String;
      hid          : out Sernum_Value;
      pid          : out Sernum_Value;
      iteration    : out Positive ) is
      
      wave_name      : constant String := AWS.Parameters.Get( params, "wave", 1 );
      hid_name       : constant String := AWS.Parameters.Get( params, "hid", 1 );
      pid_name       : constant String := AWS.Parameters.Get( params, "pid", 1 );
      iteration_name : constant String := AWS.Parameters.Get( params, "iter", 1 );
   begin
      -- dataset_name    := AWS.Parameters.Get( params, "dataset_name", 1 );
      hid := Sernum_Value'Value( hid_name );
      pid := Sernum_Value'Value( pid_name );
      wave := Waves'Value( wave_name );
      iteration := Positive'Value( iteration_name );
   end Get_Person_Ids_From_URL;
   


   function To_String( hdata : Household_Data ) return String is
      translations : Translate_Set;
      labels : Vector_Tag;
      values : Vector_Tag;
   begin
      labels := labels & "Interview Date";
      values := values & Time_Format.Format( hdata.interview_date );
      labels := labels & "Current Simulated Date";
      values := values & Time_Format.Format( hdata.current_simulated_date );
      labels := labels & "Wave";
      values := values & Waves'Image( hdata.wave );
      labels := labels & "Hid";
      values := values & Sernum_Value'Image( hdata.hid );
      labels := labels & "Origin Hid";
      values := values & Sernum_Value'Image( hdata.origin_hid );
      labels := labels & "Tenure";values := values & Prettify_Image( Tenure_Type'Image( hdata.tenure ));
      labels := labels & "Region";values := values & Prettify_Image( Region_Type'Image( hdata.region ));
      labels := labels & "Gross rent";values := values & Format_With_Commas( hdata.gross_rent );
      labels := labels & "Net rent";values := values & Format_With_Commas( hdata.net_rent );
      labels := labels & "Mortgage Outstanding";values := values & Format_With_Commas( hdata.mortgage_outstanding );
      labels := labels & "Mortgage Payment";values := values & Format_With_Commas( hdata.mortgage_payment );      
      labels := labels & "Gross housing costs";values := values & Format_With_Commas( hdata.gross_housing_costs );
      labels := labels & "Net housing costs";values := values & Format_With_Commas( hdata.net_housing_costs );
      labels := labels & "Total income";values := values & Format_With_Commas( hdata.total_income );
      labels := labels & "House value";values := values & Format_With_Commas( hdata.house_value );
      labels := labels & "Other property value";values := values & Format_With_Commas( hdata.other_property_value );
      labels := labels & "Years Outstanding on Mortgage";values := values & Natural'Image( hdata.years_outstanding_on_mortgage );
      labels := labels & "Weights";values := values & Weights_Package.To_String( hdata.weights );
      labels := labels & "CT band";values := values & Prettify_Image( Council_Tax_Band'Image( hdata.ct_band ));
      labels := labels & "Has full sample";values := values & Boolean'Image( hdata.has_full_sample );
      
      Insert( translations, Assoc( "CAPTION", "Household Data" ));
      Insert( translations, Assoc( "LABEL", labels ));
      Insert( translations, Assoc( "VALUE", values ));
      return 
         Web_Utils.Parse_Template( 
            Model.WSC.Global_Settings.template_components_path & 
            Model.WSC.Global_Settings.dir_separator & "display_table", 
            translations );      
   end To_String;

   --
   -- FIXME: to t_Web_IO
   -- 
   function To_String( caption : String; inc : Incomes_Array ) return String is
      translations : Translate_Set;
      labels : Vector_Tag;
      values : Vector_Tag;
      n      : Natural := 0;
   begin
      for i in Incomes_Type loop
         if( not (i in Calculated_Incomes) and ( inc( i ) /= 0.0 ))then
            labels := labels & Prettify_Image( Incomes_Type'Image( i ));
            values := values & Format_With_Commas( inc( i ));
            n := n + 1;
         end if;         
      end loop;
      if( n > 0 )then
         Insert( translations, Assoc( "CAPTION", caption ));
         Insert( translations, Assoc( "LABEL", labels ));
         Insert( translations, Assoc( "VALUE", values ));
         return 
            Web_Utils.Parse_Template( 
               Model.WSC.Global_Settings.template_components_path & 
               Model.WSC.Global_Settings.dir_separator & "income_list", 
               translations );
      else
         return "";
      end if;
   end To_String;
   
   function To_String( caption : String; f : Fitness_Array ) return String is
      translations : Translate_Set;
      labels : Vector_Tag;
      help_needed : Vector_Tag;
      difficulty : Vector_Tag;
   begin
      for t in Task_Type loop
         labels := labels & Prettify_Image( Task_Type'Image( t ));
         help_needed := help_needed & Prettify_Image( Help_Needed_Type'Image( f( t ).help )); 
         difficulty := difficulty & Prettify_Image( Difficulty_Type'Image( f( t ).diff ));
      end loop;
      Insert( translations, Assoc( "CAPTION", caption ));
      Insert( translations, Assoc( "LABEL", labels ));
      Insert( translations, Assoc( "HELP-NEEDED", help_needed ));
      Insert( translations, Assoc( "DIFFICULTY", difficulty ));
      return 
         Web_Utils.Parse_Template( 
            Model.WSC.Global_Settings.template_components_path & 
            Model.WSC.Global_Settings.dir_separator & "fitness", 
            translations );
   end To_String;      


   function To_String( pers : Person; print_regressors : Boolean := False ) return String is
      main_translations : Translate_Set;
      translations : Translate_Set;
      labels : Vector_Tag;
      values : Vector_Tag;
   begin
      labels := labels & "PID";values := values & Sernum_Value'Image( pers.pid );
      labels := labels & "Pno";values := values & Adult_Count'Image( pers.pno );
      labels := labels & "Age";values := values & Age_Range'Image( pers.age );
      labels := labels & "Age";values := values & Age_Range'Image( pers.age );
      labels := labels & "Sex";values := values &  Prettify_Image( Gender_Type'Image( pers.sex ));
      labels := labels & "Respondent Weights";values := values & Weights_Package.To_String( pers.respondent_weights );
      labels := labels & "Enumeration Weights";values := values & Weights_Package.To_String( pers.enumeration_weights );
      labels := labels & "Disability Score";values := values & Format( pers.activities_of_daily_living_score );
      labels := labels & "Health Score";values := values & Format( pers.health_score );
      labels := labels & "Hdsp";values := values & Prettify_Image( Head_Or_Spouse'Image( pers.hdsp ));
      labels := labels & "Years In Residential Care";values := values & Age_Range'Image( pers.years_in_residential_care );
      labels := labels & "Has Full Sample";values := values & Prettify_Image( Boolean'Image( pers.has_full_sample ));
      labels := labels & "Receives Informal Care from household member";values := values & Prettify_Image( Boolean'Image( pers.Receives_informal_care_from_household_member ));
      labels := labels & "Receives informal care from non householder";values := values & Prettify_Image( Boolean'Image( pers.Receives_informal_care_from_non_householder ));
      labels := labels & "Hours of Care Received";values := values & Hours_Count'Image( pers.hours_of_care_recieved );
      labels := labels & "Hours of Care Given";values := values & Hours_Count'Image( pers.hours_of_care_given );
      -- can't use this because of silly design fault 
      -- labels := labels & "Dies this period";values := values & Prettify_Image( Boolean'Image( pers.dies_this_period ));
      labels := labels & "Seperates this period";values := values & Prettify_Image( Boolean'Image( pers.seperates_this_period ));
      labels := labels & "Usual Hours Worked Per Week";values := values & Hours_Count'Image( pers.usual_hours_worked_per_week );
      labels := labels & "Highest Qualification";values := values &  Prettify_Image( Qualification_Type'Image( pers.highest_qualification ));
      labels := labels & "Is Disabled";values := values & Boolean'Image( pers.is_disabled );
      labels := labels & "Marital Status";values := values & Prettify_Image( Marital_Status_Type'Image( pers.marital_status ));
      labels := labels & "Partner Status";values := values & Prettify_Image( Partner_Status_Type'Image( pers.partner_status ));
      labels := labels & "Health Status";values := values & Prettify_Image( Health_Status_Type'Image( pers.health_status ));
      
      labels := labels & "Disability Living Allowance Mobility level";
      values := values & Prettify_Image( High_Low_Nil_Type'Image( pers.disability_living_allowance_mobility_level ));
      labels := labels & "Disability Living Allowance Care level";
      values := values & Prettify_Image( High_Middle_Low_Nil_Type'Image( pers.disability_living_allowance_care_level ));
      labels := labels & "Attendance Allowance Level";
      values := values & Prettify_Image( High_Low_Nil_Type'Image( pers.attendance_allowance_level ));

      labels := labels & "Personal Wealth";
      values := values & Format_With_Commas( pers.personal_wealth );

      Insert( translations, Assoc( "CAPTION", "Household Data" ));
      Insert( translations, Assoc( "LABEL", labels ));
      Insert( translations, Assoc( "VALUE", values ));
      declare
         p_body : String := Web_Utils.Parse_Template( 
               Model.WSC.Global_Settings.template_components_path & 
               Model.WSC.Global_Settings.dir_separator & "display_table", 
               translations );      
      begin
         Insert( main_translations, Assoc( "BODY", p_body ));
      end;
      -- Insert( main_translations, Assoc( "ANNUAL-INCOMES", To_String(  "Annual Incomes", pers.annual_income )));
      Insert( main_translations, Assoc( "CURRENT-INCOMES", To_String(  "Current Income", pers.current_income )));
      Insert( main_translations, Assoc( "FITNESS-MEASURES", To_String(  "Fitness Measures", pers.fitness )));
      
      if( print_regressors )then
         Insert( main_translations, Assoc( "REGRESSORS", Regressors_Package.To_String( pers.regressors )));
      end if;         
      return  Web_Utils.Parse_Template( 
               Model.WSC.Global_Settings.template_components_path & 
               Model.WSC.Global_Settings.dir_separator & "personal", 
               main_translations );    
   end To_String;
   
   function To_String( bu : Benefit_Unit ) return String is
      main_translations : Translate_Set;
      translations : Translate_Set;
   begin
       return  Web_Utils.Parse_Template( 
               Model.WSC.Global_Settings.template_components_path & 
               Model.WSC.Global_Settings.dir_separator & "benefit_unit", 
               translations );    
   end To_String;
   
   
   function To_String( hh : Household ) return String is
      main_translations : Translate_Set;
      translations : Translate_Set;
   begin
      
      return  Web_Utils.Parse_Template( 
               Model.WSC.Global_Settings.template_components_path & 
               Model.WSC.Global_Settings.dir_separator & "household", 
               translations );    
   end To_String;
   
end Model.WSC.Household.Web_IO;
