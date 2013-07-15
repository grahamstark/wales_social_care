with GNATColl.Traces;
with Text_Utils;
with Web_Utils;

with Model.WSC.Formatting;
with Model.WSC.Household.Web_IO;
with Model.WSC.Results.Web_IO;
with Model.WSC.Results.DAO;
with Model.WSC.Results;
with Model.WSC.Global_Settings;
with Model.WSC.Parameters;
with Model.WSC.Household.Transitions;

package body Model.WSC.Household.Complete_Web_IO is
   use Model.WSC.Formatting;
   use Text_Utils;
   use Model.WSC.Results;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.HOUSEHOLD.COMPLETE_WEB_IO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   function Write_Person(
         db         : DB_Type; 
         pers       : Person; 
         pre        : Personal_Result;
         post       : Personal_Result;
         diff       : Personal_Result;
         pid        : Sernum_value;
         iteration  : Positive; 
         wave       : Waves;
         start_wave : Waves;
         end_wave   : Waves ) return String is
      translations : Translate_Set;
      next_wave    : Waves := Waves'Succ( wave );
   begin
      if( pers.pid = pid )then
         null;
         -- do something to set focus on this person
      end if;
      Insert( translations, Assoc( "ROOT", Model.WSC.Global_Settings.WSC_Web_Root ));
      Insert( translations, Assoc( "IN_CARE_HOME", pers.years_in_residential_care > 0 ));
      Insert( translations, Assoc( "MOVES_TO_CARE_THIS_PERIOD", pers.years_in_residential_care = 1 ));
      Insert( translations, Assoc( "PERSONAL_INFO", Model.WSC.Household.Web_IO.To_String( pers )));
      Insert( translations, Assoc( "PERSONAL_RESULTS", Model.WSC.Results.Web_IO.To_String( pre, post, diff )));
      if( wave < end_wave )then
         if db.Contains_Person( next_wave, pers.pid )then
            declare
               next_per     : Person := db.Get_Person( next_wave, pers.pid );
            begin      
               Insert( translations, Assoc( "NEXT_LINK", Model.WSC.Household.Web_IO.Person_To_URL( next_wave, next_per, iteration )));
            end;
            Insert( translations, Assoc( "DIES_THIS_PERIOD", False ));
         else
            Insert( translations, Assoc( "DIES_THIS_PERIOD", True ));
            Insert( translations, Assoc( "NEXT_PERIOD", "" ));
         end if;
         Insert( translations, Assoc( "LAST_PERIOD", False ));
      else
         Insert( translations, Assoc( "LAST_PERIOD", True ));
      end if;
      if( wave > start_wave )then
         declare
            prev_wave    : Waves := Waves'Pred( wave );
            prev_per     : Person := db.Get_Person( prev_wave, pers.pid );
         begin            
            Insert( translations, Assoc( "FIRST_PERIOD", False ));
            Insert( translations, Assoc( "PREV_LINK", Model.WSC.Household.Web_IO.Person_To_URL( prev_wave, prev_per, iteration )));
         end;
      else 
         Insert( translations, Assoc( "FIRST_PERIOD", True ));
      end if;
      return 
         Web_Utils.Parse_Template( 
            Model.WSC.Global_Settings.template_components_path & 
            Model.WSC.Global_Settings.dir_separator & "person", 
            translations );      
   end Write_Person;
      
   function Write_Benefit_Unit( 
         db         : DB_Type; 
         bu         : Benefit_Unit; 
         pre        : Benefit_Unit_Result;
         post       : Benefit_Unit_Result;
         diff       : Benefit_Unit_Result;
         pid        : Sernum_value;
         iteration  : Positive; 
         wave       : Waves;
         buno       : Benefit_Unit_Number;
         start_wave : Waves;
         end_wave   : Waves ) return String is
      translations : Translate_Set;
      people  :  Vector_Tag;
      pnos    :  Vector_Tag;
      pcount  :  Person_Count := 0;      
   begin
      for adno in 1 .. bu.num_adults loop
         pcount := pcount + 1;
         pnos := pnos & Trim( pcount'Img );
         people := people & Write_Person(
            db,
            bu.adults( adno ),
            pre.people( adno ),
            post.people( adno ),
            diff.people( adno ),
            pid,
            iteration, 
            wave,
            start_wave,
            end_wave );
      end loop;
      -- TODO children ... 
      Insert( translations, Assoc( "PERSON", people ));
      Insert( translations, Assoc( "PNO", pnos ));
      Insert( translations, Assoc( "BENEFIT-UNIT-INFO", "" ));
      Insert( translations, Assoc( "BUNO", buno ));
      Insert( translations, Assoc( "BENEFIT-UNIT-RESULTS", "" ));
      return 
         Web_Utils.Parse_Template( 
            Model.WSC.Global_Settings.template_components_path & 
            Model.WSC.Global_Settings.dir_separator & "benefit_unit", 
            translations );      
   end Write_Benefit_Unit;

   function Write_Household( 
      db                 : DB_Type; 
      wsc_run            : Run; 
      hid                : Sernum_Value; 
      pid                : Sernum_value;
      iteration          : Positive; 
      wave               : Waves;
      extra_translations : Translate_Set ) return String is
         
      translations  : Translate_Set;
      hh            : Household_Access;
      pre_results   : Household_Result_Access;
      post_results  : Household_Result_Access;
      diff          : Household_Result_Access;
      bunos         : Vector_Tag;
      bus           : Vector_Tag;
      end_wave      : Waves := Wave_From_Year( wsc_run.end_year );
      start_wave    : Waves := Wave_From_Year( wsc_run.start_year );
      
   begin
      if not db.Contains_HH( wave, hid )then
         return "no such household";
      end if;
      hh := new Household;
      pre_results := new Model.WSC.Results.Household_Result;
      post_results := new Model.WSC.Results.Household_Result;
      diff := new Model.WSC.Results.Household_Result;
      
      hh.all := db.Get_Household( wave, hid );
      
      Model.WSC.Household.Transitions.Uprate( hh.all, wsc_run, uprate_from_base_data );
      pre_results.all := Model.WSC.Results.DAO.Get( wsc_run, hh.all, Model.WSC.Parameters.PRE_SYS, iteration );
      post_results.all := Model.WSC.Results.DAO.Get( wsc_run, hh.all, Model.WSC.Parameters.POST_SYS, iteration );
      diff.all := Model.WSC.Results.Difference( pre_results.all, post_results.all );
      Insert( translations, Assoc( "ROOT", Model.WSC.Global_Settings.WSC_Web_Root ));
      Insert( translations, Assoc( "HOUSEHOLD-INFO", Model.WSC.Household.Web_IO.To_String( hh.all.hdata )));
      Insert( translations, Assoc( "HOUSEHOLD-RESULTS", Model.WSC.Results.Web_IO.To_String( pre_results.all, post_results.all, diff.all )));
      for buno in 1 .. hh.num_benefit_units loop
         bunos := bunos & Trim( buno'Img );
         bus := bus & Write_Benefit_Unit( 
            db,
            hh.all.benefit_units( buno ), 
            pre_results.all.benefit_units( buno ), 
            post_results.all.benefit_units( buno ),
            diff.all.benefit_units( buno ),
            pid,
            iteration,
            wave,
            buno,
            start_wave,
            end_wave );
      end loop;
      Insert( translations, Assoc( "BUNO", bunos ));
      Insert( translations, Assoc( "BENEFIT_UNIT", bus ));
      Insert( translations, extra_translations );
      Free( hh );
      Free( pre_results );
      Free( post_results );
      Free( diff );
      return 
         Web_Utils.Parse_Template( 
            Model.WSC.Global_Settings.template_components_path & 
            Model.WSC.Global_Settings.dir_separator & "example_page", 
            translations );      
   end Write_Household;

end Model.WSC.Household.Complete_Web_IO;
