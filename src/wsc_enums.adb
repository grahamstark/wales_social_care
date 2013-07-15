with Text_Utils;
with GNATColl.Traces;

package body WSC_Enums is

   use Text_Utils;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "WSC_ENUMS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
   function To_String( wave : Waves ) return String is
      ws : constant String := Waves'Image( wave );
      wl : constant Natural := ws'Length;
   begin
      return ws( 1 .. wl );
   end To_String;      

   
   function Group_From_Item( item : Summary_Items_Type ) return Summary_Items_Group is
      g : Summary_Items_Group;
   begin
      case item is
         when population ..  age80_plus_population => g := population;
         when social_care_clients .. non_residential_privately_funded_clients => g := care_clients;
         when residential_private_funding .. non_residential_public_funding => g := care_costs;
         when dla_care_recipients .. aa_cost  | 
              combined_benefit_recipients | 
              combined_benefit_spend  => g := benefits;
         when public_expenditure_net_of_user_charges_and_other_income | 
              private_spend_on_social_care => g := net_expenditure;
         when health_poor_or_very_poor .. health_very_poor => g := health;
         when net_income | disposable_income => g := incomes;
      end case;
      return g;
   end  Group_From_Item;
   
   function Pretty_Print( i : Tenure_Type ) return String is
   begin
      case i is
          when owned_outright => return "Owned Outright";
          when owned_with_mortgage => return "Owned with Mortgage";
          when local_authority_rented => return "Local Authority rented";
          when housing_assoc_rented => return "Housing Assoc. rented";
          when rented_from_employer => return "Rented from Employer";
          when rented_private_unfurnished => return "Rented private unfurnished";
          when rented_private_furnished => return "Rented private furnished";
          when other_rented => return "Other rented";
      end case;
   end Pretty_Print;
    
   function To_String( i : Incomes_Type ) return String is
   begin
      return Prettify_Image( Incomes_Type'Image( i ));
   end To_String;

   function Year_From_Wave( wave : Waves ) return Year_Range is
   begin
      return Waves'Pos( wave ) - Waves'Pos( a ) + BHPS_START_YEAR + 1;
   end Year_From_Wave;
   
   function Wave_From_Year( year : Year_Range ) return Waves is
   begin
      return Waves'Val( year - BHPS_START_YEAR - 1 );
   end Wave_From_Year;
   

   type MT_Array is array(  Means_Test_Result,  Means_Test_Result ) of  Means_Test_Result;
   
   MT_Combined : constant MT_Array :=
                           --                       1
                           --   not_applicable      not_entitled  partially_entitled fully_entitled 
      ( not_applicable     => ( not_applicable,     not_entitled, partially_entitled, fully_entitled ),
        not_entitled       => ( not_entitled,       not_entitled, not_entitled,       not_entitled   ),
        partially_entitled => ( partially_entitled, not_entitled, partially_entitled, partially_entitled ),
        fully_entitled     => ( fully_entitled,     not_entitled, partially_entitled, fully_entitled ));
        
   
   function Combine_Results( result_1, result_2 :  Means_Test_Result ) return Means_Test_Result is
      combined_result : Means_Test_Result;
   begin
      return MT_Combined( result_1, result_2 );
      -- if( result_1 = not_entitled or result_2 = not_entitled )then
         -- combined_result := not_entitled;
      -- elsif( result_1 = fully_entitled and result_2 = fully_entitled ) then
         -- combined_result := fully_entitled;
      -- elsif( result_1 = not_applicable and result_2 = not_applicable )then
         -- combined_result := not_applicable;
      -- elsif( result_1 = not_applicable )then
         -- combined_result := result_2;
      -- elsif( result_2 = not_applicable )then
         -- combined_result := result_1;
      -- else
         -- combined_result := partially_entitled;
      -- end if;
      -- return combined_result;
   end Combine_Results;
   
   function To_Unbounded_String( s : Sernum_Value ) return Unbounded_String is
      ss : constant String := Sernum_Value'Image( s );
   begin
      return Text_Utils.TuS( Text_Utils.Trim( ss ));
   end To_Unbounded_String;
   
   function Pretty_Print( t : Summary_Items_Type ) return String is
   begin
      case t is
      when population => return "People in Households with someone aged 60+";      
            
      when male_60_plus_population => return "Males Aged 60+";
      when female_60_plus_population => return "Females Aged 60+";
            
      when age60_plus_population => return "Population Aged 60+";
      when age65_plus_population => return "Population Aged 65+";
      when age70_plus_population => return "Population Aged 70+";
      when age80_plus_population => return "Population Aged 80+";
            
      when social_care_clients => return "Total Social Care Clients (Privately&amp;Publicly Funded)";
            
      when residential_clients => return "Total Residential Clients (Privately&amp;Publicly Funded)";
      when non_residential_clients => return "Total Non-Residential Clients (Privately&amp;Publicly Funded)";
            
      when residential_fully_publicly_funded_clients => return "Fully Publicly Funded Clients : Residential";
      when residential_partially_publicly_funded_clients => return "Partially Publicly Funded Clients : Residential";
      when residential_privately_funded_clients => return "Privately Funded Clients : Residential";
            
      when non_residential_fully_publicly_funded_clients => return "Fully Publicly Funded Clients : Non-Residential";
      when non_residential_partially_publicly_funded_clients => return "Partially Publicly Funded Clients : Non-Residential";
      when non_residential_privately_funded_clients => return "Privately Funded Clients : Non-Residential";
            
      when residential_private_funding => return "Private Funding : Residential";
      when residential_public_funding => return "Public Funding : Residential";
            
      when non_residential_private_funding => return "Private Funding : Non - Residential";
      when non_residential_public_funding => return "Public Funding : Non - Residential";
            
      when dla_care_recipients => return "Disability Living Allowance (Care Component) - Recipients";
      when dla_mob_recipients => return "Disability Living Allowance (Mobility Component) - Recipients";
      when aa_recipients => return "Attendance Allowance - Recipients";
      when dla_mob_cost => return "Disability Living Allowance (Mobility Component) - Cost";
      when dla_care_cost => return "Disability Living Allowance (Care Component) - Cost";
      when pension_recipients => return "State Pension - Recipients";
      when pension_cost => return "State Pension - Cost";
      when pc_recipients => return "Pension Credit - Recipients";
      when pc_cost => return "Pension Credit - Cost";
      when aa_cost => return "Attendance Allowance - Cost";
            
      when public_expenditure_net_of_user_charges_and_other_income => return "Public Expenditure Net Of User Charges and Other Income";
      when private_spend_on_social_care => return "Private Spend on Social Care";
            
      when net_income => return "Net Income";
      when disposable_income => return "Disposable Income";
            
      when combined_benefit_spend => return "Combined Benefit Spend";
      when combined_benefit_recipients => return "Combined Benefit Recipients";
      when health_poor_or_very_poor => return "Health Poor or Very Poor";
      when health_poor => return "Health Poor";
      when health_very_poor => return "Health Very Poor";
      end case;
   end  Pretty_Print;

end WSC_Enums; 
