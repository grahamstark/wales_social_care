with Ada.Calendar;
with Ada.Calendar.Formatting;

with GNATColl.Traces;

with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Assertions;
with Ada.Numerics.Float_Random;
with Maths_Functions;

with Model.WSC.Household.Database;
with Model.WSC.Household.Regressions;
with Model.WSC.Uprate;
with Model.WSC.Global_Settings;
with Maths_Functions.Weights_Generator;
with Maths_Functions;
with ONS_Definitions;
with Transition_Events;
with Weighting_Commons;
with Text_Utils;

package body Model.WSC.Household.Transitions is

   use Ada.Calendar;
   use Model.WSC.Uprate;
   use Ada.Assertions;
   use Transition_Events;   
   use Text_Utils;
   
   Random_Gen : Ada.Numerics.Float_Random.Generator;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.HOUSEHOLD.TRANSITIONS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   function Make_Wealth_Randoms return Wealth_Random_Array is
      a : Wealth_Random_Array;
      -- Sdevs from significant wealth regression in Model.WSC.Household.Regressions
      -- FIXME NOTE NOTE NOTE change if regression changes!!
      SD_MALE   : constant Rate := 0.4071714;
      SD_FEMALE : constant Rate := 0.4592151;
   begin
      a( has_wealth ) := Random_Normal_Generator.Draw( 
                        mean => 0.0, 
                        standard_deviation => 1.0 ); --probit 1/0
      
      a( sig_wealth_male ) := Random_Normal_Generator.Draw( 
                        mean => 0.0, 
                        standard_deviation => SD_MALE ); --probit 1/0
      a( sig_wealth_female ) := Random_Normal_Generator.Draw( 
                        mean => 0.0, 
                        standard_deviation => SD_FEMALE ); --probit 1/0
                        
      return a;
   end Make_Wealth_Randoms;


 
   
   -- zero if no reported problem; otherwse 1 .. 5 for 65-69 .. 85+ age band
   -- see Daffodil http://www.daffodilcymru.org.uk/index.php?pageNo=1062&PHPSESSID=05nhl3dd73jprvqufe12t80lt5&at=a&sc=1&loc=1&np=1
   function Unable_To_Manage_Some_Activity( ad : Person ) return Natural is
      age_pos     : Natural := 0;
      help_needed : Boolean := False;
   begin
      for t in Task_Type loop
         if( ad.fitness( t ).help > by_self )then
            help_needed := True;
            exit;
         end if;
      end loop;
      if( help_needed )then
         case ad.age is
            when 0 .. 64 =>
               age_pos := 0;
            when 65 .. 69 =>
               age_pos := 1;
            when 70 .. 74 =>
               age_pos := 2;
            when 75 .. 79 =>
               age_pos := 3;
            when 80 .. 84 =>
               age_pos := 4;
            when 85 .. Age_Range'Last =>
               age_pos := 5;
         end case;
      end if;
      return age_pos;
   end  Unable_To_Manage_Some_Activity;

   function Last_Year( 
      new_date : Time ) return Time is 
      y   : Year_Number;
      m   : Month_Number;
      d   : Day_Number;
      sec : Day_Duration;
   begin
      Split( new_date, y, m, d, sec );
      return Time_Of( y-1, m, 1 ); 
   end Last_Year;
   
   function Date_From_Wave(
      wave      : Waves;
      base_date : Time ) return Time is 
      y   : Year_Number;
      m   : Month_Number;
      d   : Day_Number;
      sec : Day_Duration;
   begin
      Split( base_date, y, m, d, sec );
      return Time_Of( Year_From_Wave( wave ), m, 1 ); 
   end Date_From_Wave;
   

   procedure Add_One_Year( date : in out Time ) is
      y        : Year_Number;
      m        : Month_Number;
      d        : Day_Number;
      sec      : Day_Duration;
   begin
      Split( date, y, m, d, sec );      
      date := Time_Of( y+1, m, 1 ); 
   end Add_One_Year;
   
   function Get_Uprate_Amount(
      old_date   : Time; 
      new_date   : Time;
      which      : Uprate_Targets; 
      wsc_run    : Run ) return Rate is
      assump   : Uprate_Assumption := wsc_run.uprate_assumptions( which );
      ch       : Rate;
      rpich    : Rate := 1.0;
      years_between : Natural;
      old_date_year, new_date_year : Year_Number; 
      old_date_month, new_date_month : Month_Number;
      old_date_day, new_date_day : Day_Number;
      old_date_sec, new_date_sec : Day_Duration;
   begin
      
      Split( old_date, old_date_year, old_date_month, old_date_day, old_date_sec );      
      Split( new_date, new_date_year, new_date_month, new_date_day, new_date_sec );  
      years_between := new_date_year - old_date_year;
      
      if( assump.use_obr )then
         ch := Get_Ratio_Between( assump.element, old_date_year, old_date_month, new_date_year, new_date_month );
         rpich := Get_Ratio_Between( rpi, old_date_year, old_date_month, new_date_year, new_date_month );
         Log( "useobr = " & assump.use_obr'Img 
              & " which = " & which'Img 
              & " assump_element " & assump.element'Img
              & " base y = " &  old_date_year'Img 
              & " new y = " & new_date_year'Img 
              & " m = " & new_date_month'Img 
              & " ch = " & ch'Img 
              & " rpich = " & rpich'Img );
      else
         return ( 1.0 + ( assump.percent_change / 100.0 )) ** years_between;
      end if;
      if( assump.element = gdp_at_market_prices ) and ( not wsc_run.real_terms ) then -- gdp is in real terms
         return ch * rpich;
      elsif( assump.element /= gdp_at_market_prices ) and ( wsc_run.real_terms ) then
         return ch / rpich;
      end if;
      return ch;
   end  Get_Uprate_Amount;
   

   procedure Uprate( 
      incomes  : in out Incomes_Array; 
      old_date : Time;
      new_date : Time; 
      wsc_run : Run ) is
      dother   : Amount;
      dbens    : Amount;
      dearn    : Amount;
   begin
      dbens := Get_Uprate_Amount( old_date, new_date, upr_state_benefits, wsc_run ); -- Get_Ratio_Between( gdp_at_market_prices_nominal, y-1, m, y, m );
      dearn := Get_Uprate_Amount( old_date, new_date, upr_earnings, wsc_run ); -- Get_Ratio_Between( obr_earnings, y-1, m, y, m );
      dother := Get_Uprate_Amount( old_date, new_date, upr_other_income, wsc_run ); -- Get_Ratio_Between( rpix, y-1, m, y, m ); -- no ROSSI index?
      for i in Earnings loop
         incomes( i ) := incomes( i ) * dearn;
      end loop;
      for i in State_Benefits loop
         incomes( i ) := incomes( i ) * dbens;
      end loop;
      for i in Other_Income loop
        incomes( i ) := incomes( i ) * dother; 
      end loop;
   end Uprate;

   procedure Uprate( 
      hh          : in out Household; 
      wsc_run     : Run;
      uprate_type : Type_Of_Uprating ) is
      dhouse  : Amount;
      date_last_year  : constant Time := Last_Year( hh.hdata.current_simulated_date );
      target_date     : constant Time := ( if uprate_type = annual_increment then date_last_year else hh.hdata.interview_date );
      dwealth : Amount;
   begin
      if( uprate_type = no_uprating )then
         return;
      end if;
      Log( "uprating to " & Ada.Calendar.Formatting.Image( hh.hdata.current_simulated_date ));
      --
      -- uprate from 12 months ago, except for wealth
      -- assume 1st day of month always
      --
      dhouse := Get_Uprate_Amount( 
         target_date, 
         hh.hdata.current_simulated_date, 
         upr_housing_costs, 
         wsc_run );
      --
      -- Wealth is uprated from the *interview(* date, since we use tghe regression every year to infer an amount
      -- 
      dwealth := Get_Uprate_Amount( 
         target_date, 
         hh.hdata.current_simulated_date, 
         upr_wealth, 
         wsc_run );

      -- pro tem assumptions
      -- 1) rents & mortgage costs rise with nominal gdp
      
      hh.hdata.gross_rent := hh.hdata.gross_rent * dhouse;
      hh.hdata.net_rent := hh.hdata.net_rent * dhouse;
      hh.hdata.mortgage_outstanding := hh.hdata.mortgage_outstanding * dhouse;
      hh.hdata.gross_housing_costs := hh.hdata.gross_housing_costs * dhouse;
      hh.hdata.net_housing_costs := hh.hdata.net_housing_costs * dhouse;
      hh.hdata.total_income := hh.hdata.total_income * dhouse;
      hh.hdata.house_value := hh.hdata.house_value * dhouse;
      hh.hdata.other_property_value := hh.hdata.other_property_value * dhouse;
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 ..  hh.benefit_units( buno ).num_adults loop
            declare
               bu : Benefit_Unit renames hh.benefit_units( buno );
               ad : Person renames bu.adults( adno );
            begin
               -- Uprate( ad.annual_income, target_date, hh.hdata.current_simulated_date, wsc_run );
               Uprate( ad.current_income, target_date, hh.hdata.current_simulated_date, wsc_run );
               ad.personal_wealth := ad.personal_wealth * dwealth;
               Recalculate_Regressors( 
                  ad, 
                  hh.Num_Adults, 
                  hh.Num_Children,
                  hh.hdata );
            end; 
         end loop;
         for chno in 1 .. hh.benefit_units( buno ).num_children loop
            declare
               ch : Person renames hh.benefit_units( buno ).children( chno );
            begin
               -- Uprate( ch.annual_income, target_date, hh.hdata.current_simulated_date, wsc_run );
               Uprate( ch.current_income, target_date, hh.hdata.current_simulated_date, wsc_run );
            end; 
         end loop;
      end loop;
   end Uprate;
   
   procedure Move_Child_To_Seperate_BU( 
      hh : in out Household; 
      buno : Benefit_Unit_Number; 
      chno : Person_Number;
      event_count: in out ev_count.Recorder  ) is
   begin
      hh.num_benefit_units := hh.num_benefit_units + 1;
      hh.benefit_units( hh.num_benefit_units ).num_adults := 1;
      hh.benefit_units( hh.num_benefit_units ).adults( 1 ) := hh.benefit_units( buno ).children( chno );
      hh.benefit_units( hh.num_benefit_units ).num_adults := 1;
      hh.benefit_units( hh.num_benefit_units ).num_people := 1;
      hh.benefit_units( hh.num_benefit_units ).position_of_head := 1;

      hh.benefit_units( buno ).num_children := hh.benefit_units( buno ).num_children - 1; 
      for c in chno ..  hh.benefit_units( buno ).num_children loop
         -- shuffle childen down 1
         hh.benefit_units( buno ).children( c ) := hh.benefit_units( buno ).children( c + 1 ); 
      end loop;
   end Move_Child_To_Seperate_BU;

   procedure Age( 
      hh          : in out Household; 
      wsc_run : Run;
      event_count : in out ev_count.Recorder  ) is
      weight  : constant Amount := hh.Weight ;
      hh_last_period : Household := hh;
   begin
      Log( "moving data from: " & Ada.Calendar.Formatting.Image( hh.hdata.current_simulated_date ));
      --
      -- assume 1st day of month always
      --
      Add_One_Year( hh.hdata.current_simulated_date );
      Log( "moving data to: " & Ada.Calendar.Formatting.Image( hh.hdata.current_simulated_date ));
      
      hh.hdata.wave := Waves'Succ( hh.hdata.wave );
      hh.wave := hh.hdata.wave;
      
      if( hh.hdata.years_outstanding_on_mortgage > 1 )then
         hh.hdata.years_outstanding_on_mortgage := hh.hdata.years_outstanding_on_mortgage - 1;
      else
         hh.hdata.years_outstanding_on_mortgage := 0;
         hh.hdata.mortgage_outstanding := 0.0; -- must have mortgage 
      end if;
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 ..  hh.benefit_units( buno ).num_adults loop
            declare
               bu : Benefit_Unit renames hh.benefit_units( buno );
               ad : Person renames bu.adults( adno );
               bu_last_period : Benefit_Unit renames hh_last_period.benefit_units( buno );
               ad_last_period : Person renames bu_last_period.adults( adno );
               is_couple : Boolean := bu.position_of_spouse > 0 and ( bu.position_of_head = ad.pno or bu.position_of_spouse = ad.pno );
            begin
               --
               -- add count of adults, since this is the 1st thing
               --
               ad.age := ad.age + 1;
               ad.hid := hh.hid;
               if( ad.age = 65 )then
                  event_count.add( new_65_plus, ad.wave, hh.hid, ad.pid, ad.age, ad.sex, weight );
               end if;
               ad.wave := hh.wave;
               event_count.Add( total_population, ad.wave, hh.hid, ad.pid, ad.age, ad.sex, weight );
               if( ad.years_in_residential_care > 0 )then
                  Log( ad.To_String );
                  event_count.Add( residential_population, ad.wave, hh.hid, ad.pid, ad.age, ad.sex, weight );
                  ad.years_in_residential_care := ad.years_in_residential_care + 1;
               else
                  event_count.Add( non_residential_population, ad.wave, hh.hid, ad.pid, ad.age, ad.sex, weight );
               end if;
               Recalculate_Regressors( 
                  ad, 
                  hh.Num_Adults, 
                  hh.Num_Children,
                  hh.hdata );
            end; 
         end loop;
         for chno in 1 .. hh.benefit_units( buno ).num_children loop
            declare
               ch : Person renames hh.benefit_units( buno ).children( chno );
               ch_last_period : Person renames hh_last_period.benefit_units( buno ).children( chno );
               is_couple : Boolean := False;
            begin
               --
               -- add count of children, before ageing
               --
               event_count.Add( total_population, ch.wave, hh.hid, ch.pid, ch.age, ch.sex, weight );
               ch.age := ch.age + 1;   
               ch.wave := hh.wave;
               ch.hid := hh.hid;
               Recalculate_Regressors( 
                  ch, 
                  hh.Num_Adults, 
                  hh.Num_Children,
                  hh.hdata );
              if( ch.age = 18 )then
                 Move_Child_To_Seperate_BU( hh, buno, chno, event_count );
                 event_count.Add( new_benefit_unit, ch.wave, hh.hid, ch.pid, ch.age, ch.sex, weight );
              end if;
            end; 
         end loop;
      end loop;
   end Age;
   
   
   
   -- At year t:
   -- 1. Predict receipt of informal care (and hours of care received) from other household members at time t
   -- 2. Predict receipt of informal care from other non-household members at time t
   -- 3. Do predictions for the survival/death regression to find out who survives at time t+1
   -- 4. (for survivors) do predictions for the splitting up regression to find out which couples become singles at time t+1
   -- 5. Do predictions of deteriorations (or improvements in) health status at time t+1
   -- 6. Do predictions of deteriorations (or improvements in) ADL indicators at time t+1
   -- 7. Predict retirement at t+1 (if still working at time t)
   -- 8. Predict changes in wealth levels at time t+1   procedure Apply_Main_Model(
   procedure Apply_Main_Model(
      hh             : in out Household; 
      hh_last_period : Household;
      which_person   : Person_Number;
      wsc_run : Run ) is
   use Model.WSC.Household.Regressions;
   begin
      null;
   end Apply_Main_Model;
   
   procedure Apply_Sub_Model(
      hh             : in out Household; 
      hh_last_period : Household;
      which_person   : Person_Number;
      wsc_run : Run ) is
   use Model.WSC.Household.Regressions;
   begin
      null;
   end Apply_Sub_Model;

   local_event_count: ev_count.Recorder;

   procedure Create_Initial_Care_Home_Population(       
      db           : in out Model.WSC.Household.Database.DB_Type;
      wsc_run      : Run;   
      wave         : Waves;
      auto_update  : Boolean ) is
      use Model.WSC.Household.Database;
      use Model.WSC.Household;
      use Base_Model_Types;
      use WSC_Enums;
      
      INITIAL_CARE_HOME_POPULATION : constant Amount := 21_500.0; -- from estimates of home-care population spreadsheet
      
      procedure Make_New_Res_HHlds( pl : Person_Ordering_List; target_count : Amount ) is
         residents_so_far  : Amount := 0.0;
         num_added         : Natural := 0;
         n                 : Natural := Natural( pl.Length );
         po                : Person_Ordering_Record;
         po_last           : Person_Ordering_Record;
         hh                : Household;
         hh_last_period    : Household;
         buno              : Benefit_Unit_Number; 
         adno              : Adult_Count;
         chno              : Child_Count;
         found             : Boolean;
         j                 : Natural := 0;
      begin
         Log( "Create_Initial_Care_Home_Population; n = " & n'Img & " target count : " & Format( target_count ));
         po_last.v := 9999999.0;
         for i in reverse 1 .. n loop
            if( residents_so_far >= target_count )then
               Log( "exiting with created population of " & 
                  Format( residents_so_far ) & 
                  " target was " & Format( target_count ) 
                  & " " & num_added'Img & " households added" );
               exit; 
            end if;   
            num_added := num_added + 1;
            po := pl.Element( i );
            Assert( po.v <= po_last.v, " mismatch of pos last " & Format( po_last.v ) & " curr " & Format( po.v ));
            
            Log( "Create_Initial_Care_Home_Population; wave " & wave'Img & i'Img & " : " & To_String( po ));
            hh := db.Get_Household( wave, po.hid );
            if( auto_update )then
               hh_last_period := hh;
               -- Age( hh, wsc_run, local_event_count );
               Impute_Care_Amounts( hh, hh_last_period, wsc_run, local_event_count );
               Predict_Health_Needs_And_Employment_Status( hh, wsc_run, local_event_count );
               hh.hid := hh.hid + 2_000_000;
               hh.hdata.hid := hh.hid; 
               hh.hdata.origin_hid := hh.hid;
               Log( "Create_Initial_Care_Home_Population got household " & 
                    hh.hid'Img & 
                    " new sernum " & hh.hid'Img );
            end if;
            hh.Find_Person_By_Pno( po.pno, buno, adno, chno, found );
            Log( "Create_Initial_Care_Home_Population changed years_in_residential_care for buno " & buno'Img & " adno " & adno'Img );
            Log( "hhld is " & To_String( hh ));
            declare
               ad : Person renames hh.benefit_units( buno ).adults( adno );
            begin
               ad.years_in_residential_care := 1;     
               ad.pid := ad.pid + 2_000_000;
               ad.hid := hh.hid;
               -- ad.
               local_event_count.Add( to_care_home, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
            end;            
            db.Write_Household( hh );
            residents_so_far := residents_so_far + po.weight;
            j := j + 1;
            Log( "population is now " & Format( residents_so_far ) & " of a target of target " & Format( Target_Count ) & " i = " & j'Img );
            po_last := po;
         end loop;
      end Make_New_Res_HHlds;
      
      sernums        : Sernum_List;
      hh             : Household;
      hh_last_period : Household;
      num_households : Natural;
      age_u_75, 
      age_75_84, 
      age_85_plus    : Person_Ordering_List;
    begin
      db.Get_Household_Sernums( wave, sernums );
      num_households := Natural( sernums.Length );
      households:
      for hhno in 1 .. num_households loop
         hh := db.Get_Household( 
            wave => wave, 
            hid  => sernums.Element( hhno ));
         if( hh.Num_People = 1 ) and ( hh.benefit_units( 1 ).adults( 1 ).age >= 65 )then
            hh_last_period := hh;
            Impute_Care_Amounts( hh, hh_last_period, wsc_run, local_event_count );
            Predict_Health_Needs_And_Employment_Status( hh, wsc_run, local_event_count );
            declare
               ad      : Person renames hh.benefit_units( 1 ).adults( 1 );
               po      : Person_Ordering_Record := ( 
                  hid    => hh.hid, 
                  pid    => ad.pid, 
                  pno    => ad.pno,
                  v      => ad.regressors( adlscore ), 
                  weight => hh.Weight ); -- extended2 == wales sample
            begin
               if( ad.years_in_residential_care <= 0 ) then  -- always true, of course
                  -- Log( "Create_Initial_Care_Home_Population wave " & wave'Img & " hh " & hh.hid'img & " adding a person aged " & ad.age'Img & "ad.years_in_residential_care " & ad.years_in_residential_care'Img );
                  -- Log( "po is " & To_String( po ));
                  if( ad.age < 75 )then
                     age_u_75.Append( po );
                  elsif( ad.age < 85 )then
                     age_75_84.Append( po );
                  elsif( ad.age >= 85 )then
                     age_85_plus.Append( po );
                  end if;
               end if;
            end;
         end if;
      end loop households;
      --
      -- proportions from Daffodil, assuming LA and private clients have
      -- same age distributions
      --
      Sort_By_V( age_u_75 );
      Sort_By_V( age_75_84 );
      Sort_By_V( age_85_plus );
      Log( "making u75 care home" );         
      Make_New_Res_HHlds( age_u_75, 0.1091026469 * INITIAL_CARE_HOME_POPULATION );
      Log( "making 75-84 care home" );         
      Make_New_Res_HHlds( age_75_84, 0.3096352485 * INITIAL_CARE_HOME_POPULATION );
      Log( "making 85+ care home" );         
      Make_New_Res_HHlds( age_85_plus, 0.5812621046 * INITIAL_CARE_HOME_POPULATION );
   end Create_Initial_Care_Home_Population;


   procedure Kill_Household_Members( 
      hh          : in out Household; 
      wsc_run     : Run;
      event_count : in out ev_count.Recorder ) is
   use Model.WSC.Household.Regressions;
      someone_dies : Boolean := False;
   begin
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 ..  hh.benefit_units( buno ).num_adults loop
            declare
               bu                  : Benefit_Unit renames hh.benefit_units( buno );
               ad                  : Person renames bu.adults( adno );
               pr                  : Amount;
            begin
               if( hh.hdata.region = wales )then
                  if( ad.years_in_residential_care > 0 )then
                     -- probs from BUPA survey, table 15, top row
                     declare
                        p  : constant Rate := Maths_Funcs.Random_0_To_1;
                        survival_prob : Rate;
                     begin
                        if ad.years_in_residential_care = 1 then
                           survival_prob := 0.545;
                        elsif ad.years_in_residential_care = 2 then
                           survival_prob := 0.692;
                        else
                           survival_prob := 0.686;
                        end if;
                        ad.dies_this_period := p > survival_prob;
                     end;
                     if( ad.age > 105 )then 
                        ad.dies_this_period := True;
                     end if;
                  elsif( ad.age >= 65 )then
                     pr := Dies_This_Period_Probit( ad, hh.hdata.region );
                     ad.dies_this_period := Evaluate_Probit( pr, wsc_run.probit_thresholds( dies_this_period ), wsc_run.Use_Random_Threshold );
                     log( "Kill_Household_Members; pr = " & 
                          pr'Img & 
                          " => ad.dies_this_period " & ad.dies_this_period'Img & 
                          " ad.years_in_residential_care " & ad.years_in_residential_care'Img & 
                          " threshold " & wsc_run.probit_thresholds( dies_this_period )'Img );
                  end if;
                  if( ad.dies_this_period )then
                     event_count.Add( death, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                     if( ad.years_in_residential_care > 0 )then
                        event_count.Add( residential_death, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                     else
                        event_count.Add( non_residential_death, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                     end if;
                     bu.Delete_Adult( adno, death );
                     someone_dies := True;
                  end if;
               end if;
            end;
         end loop;
      end loop;
      if( someone_dies )then
         hh.Remove_Empty_Benefit_Units;
      end if;
   end Kill_Household_Members;
   
   procedure Infer_Wealth( 
      hh           : in out Household;
      wsc_run      : Run;
      event_count  : in out ev_count.Recorder;
      capital_rand : Wealth_Random_Array ) is
   use Model.WSC.Household.Regressions;
   begin
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 ..  hh.benefit_units( buno ).num_adults loop
            declare
               bu                     : Benefit_Unit renames hh.benefit_units( buno );
               ad                     : Person renames bu.adults( adno );
               has_significant_wealth : Boolean;
               pr                     : Amount;
               rr                     : Regression_Results;
               which_p                : Wealth_Random_Type := ( if ad.sex = male then sig_wealth_male else sig_wealth_female );
             begin
               pr := Has_Wealth_Probit( ad );
               Log( "has significant wealth; pr = " & pr'Img );
               has_significant_wealth := Evaluate_Probit( 
                  pr + capital_rand( has_wealth ), 
                  wsc_run.probit_thresholds( has_wealth ));
               Log( "has significant wealth: " & has_significant_wealth'Img );
               if has_significant_wealth then
                  rr :=  Log_Wealth_Regression( ad, hh.hdata.region );
                  if( wsc_run.Use_Random_Threshold )then
                     rr.vp := rr.vp + capital_rand( which_p );
                  end if;
                  ad.personal_wealth := Exp( rr.vp );
                  Log( "ad.personal_wealth " & ad.personal_wealth'Img );
               end if;
             end;
          end loop;
       end loop;
    end Infer_Wealth;

   procedure Impute_Care_Amounts( 
      hh             : in out Household; 
      hh_last_period : Household;
      wsc_run : Run;
      event_count    : in out ev_count.Recorder ) is
   use Model.WSC.Household.Regressions;
   begin
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 ..  hh.benefit_units( buno ).num_adults loop
            declare
               bu                  : Benefit_Unit renames hh.benefit_units( buno );
               ad                  : Person renames bu.adults( adno );
               ad_last_period      : Person renames hh_last_period.benefit_units( buno ).adults( adno );
               is_couple           : Boolean := bu.position_of_spouse > 0 and ( bu.position_of_head = ad.pno or bu.position_of_spouse = ad.pno );
               pr                  : Amount;
               rr                  : Regression_Results;
               use_lagged_dep_vars : constant Boolean := ad.age >= 65;
               receives_informal_care_from_household_member : Boolean := False;
               receives_informal_care_from_non_householder : Boolean := False;
            begin
               if( hh.hdata.region = wales )then
                  if( ad.age >= 65 )then
                     --
                     --
                     pr :=  Informal_Care_From_Householder_Probit( ad, ad_last_period, hh.hdata.region, use_lagged_dep_vars );
                     Log( "prob informal care is " & Format( pr ));
                     
                     receives_informal_care_from_household_member := 
                        Evaluate_Probit( pr, wsc_run.probit_thresholds( informal_care_from_household_member ), wsc_run.Use_Random_Threshold );
                     --
                     -- record any increases in informal care
                     if( not ad.receives_informal_care_from_household_member and receives_informal_care_from_household_member )then
                        event_count.add(
                           care_received_from_household_member_starts,
                           hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                     end if;
                     ad.receives_informal_care_from_household_member := receives_informal_care_from_household_member;
                     if( ad.receives_informal_care_from_household_member )then
                        event_count.add(
                           care_received_from_household_member,
                           hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                        rr :=  Hours_Of_Care_Regression( ad, hh.hdata.region );   
                        if( wsc_run.Use_Random_Threshold )then
                           rr.vp := rr.vp + Random_Normal_Generator.Draw( 
                              mean => 0.0, 
                              standard_deviation => rr.sd );
                        end if;
                        Log( "got predicted hours as " & Format( rr.vp ));
                        if( rr.vp < 0.0 )then
                           ad.hours_of_care_recieved := 0;
                        elsif( rr.vp > Amount( Hours_Count'Last ))then
                           ad.hours_of_care_recieved := Hours_Count'Last; 
                        else
                           ad.hours_of_care_recieved := Hours_Count( rr.vp );
                        end if;
                     end if;
                     Recalculate_Regressors( ad, hh.Num_Adults, hh.Num_Children, hh.hdata );
                     pr :=  Informal_Care_From_Non_Householder_Probit( ad, ad_last_period, hh.hdata.wave );
                     receives_informal_care_from_non_householder  := 
                        Evaluate_Probit( pr, wsc_run.probit_thresholds( informal_care_from_non_householder ), wsc_run.Use_Random_Threshold );
                     --
                     -- record any increases in formal care
                     if( not ad.receives_informal_care_from_non_householder and receives_informal_care_from_non_householder )then
                        event_count.add(
                           care_received_from_non_householder_starts,
                           hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                     end if;
                     ad.receives_informal_care_from_non_householder  := receives_informal_care_from_non_householder;
                     if( ad.receives_informal_care_from_non_householder )then
                        event_count.add(
                           care_received_from_non_householder,
                           hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                     end if;
                     
                     Recalculate_Regressors( ad, hh.Num_Adults, hh.Num_Children, hh.hdata );
                     
                     
                  end if;
               end if;
            end;
         end loop;
      end loop;
   end Impute_Care_Amounts;
 
   procedure Reallocate_HH_Assets( 
      hl : in out  Household_List;
      wsc_run : Run  ) is
      -- TODO
   begin
      null;
   end  Reallocate_HH_Assets;
   
   procedure Split_Up_Household( 
      hh             : Household; 
      wsc_run : Run;
      new_households : out Household_List;
      event_count      : in out ev_count.Recorder ) is
   use Model.WSC.Household.Regressions;
      mhh : Household := hh;
   begin
      benefit_units:
      for buno in 1 .. mhh.num_benefit_units loop
         declare
            bu : Benefit_Unit renames mhh.benefit_units( buno );
         begin
            if( bu.num_adults > 1 )then
               adults:
               for adno in 1 ..  bu.num_adults loop
                  declare
                     ad : Person renames mhh.benefit_units( buno ).adults( adno );
                     pr : Amount;
                  begin
                     if( ad.pid < 0 )then -- like this so we get full hh print out
                        Log( "sernum missing for hh " & To_String( hh ));
                        Assert( ad.pid > 0, "pid missing for hh " & hh.hid'Img );
                     end if;

                     pr := HH_Split_Probit( ad, hh.hdata.region, hh.hdata.wave );
                     ad.seperates_this_period  := 
                        Evaluate_Probit( pr, wsc_run.probit_thresholds( hh_split ), wsc_run.Use_Random_Threshold );
                     Log( "Split_Up_Household; pr = " & pr'Img & " ad.seperates_this_period " & ad.seperates_this_period'Img );
                     
                     if( ad.seperates_this_period )then
                        -- always assume a new single household
                        declare
                           new_household : Household;
                           nad : Person renames new_household.benefit_units( 1 ).adults( 1 );
                        begin
                           event_count.add(
                              separation,
                              hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                           
                           new_household.num_benefit_units := 1;
                           new_household.benefit_units( 1 ).adults( 1 ) := ad;
                           new_household.benefit_units( 1 ).num_adults := 1;
                           new_household.benefit_units( 1 ).position_of_head := 1;
                           if( nad.marital_status = married )then
                              nad.marital_status := separated;
                           elsif( nad.marital_status = in_a_civil_partnership )then
                              nad.marital_status := have_a_dissolved_civil_partnership;
                           end if;
                           nad.partner_status := neither;
                           new_household.hdata := hh.hdata; -- SPLIT THIS UP
                           new_households.Append( new_household );
                           bu.Delete_Adult( adno, separation );
                           exit; -- allow at most 1 split per benefit unit
                        end;
                     end if;
                  end;
               end loop adults;
            end if;
         end;
      end loop benefit_units;
      mhh.Remove_Empty_Benefit_Units;
      --
      -- there m
      --
      Assert( mhh.Num_People > 0, "Split_Up_Household Must be at least 1 person in the original household" );
      new_households.Insert( 1, mhh );
      Reallocate_HH_Assets( new_households, wsc_run);
   end Split_Up_Household;
   
   procedure Predict_Health_Needs_And_Employment_Status( 
      hh        : in out Household;
      wsc_run : Run;
      event_count : in out ev_count.Recorder ) is 
   use Model.WSC.Household.Regressions;
      mhh : Household := hh;
      hl  : Household_List;
   begin
      benefit_units:
      for buno in 1 .. mhh.num_benefit_units loop
         declare
            bu : Benefit_Unit renames mhh.benefit_units( buno );
         begin
            adults:
            for adno in 1 ..  bu.num_adults loop
               declare
                  ad : Person renames mhh.benefit_units( buno ).adults( adno );
                  pr : Amount;
                  go : Boolean;
               begin
                  if( ad.pid < 0 )then -- like this so we get full hh print out
                     Log( "sernum missing for hh " & To_String( hh ));
                     Assert( ad.pid > 0, "pid missing for hh " & hh.hid'Img );
                  end if;
                  if( ad.age >= 65 )then
                     if ad.health_status > excellent then -- excellent is 1st
                        pr := Health_Better_Probit( ad, hh.hdata.region );
                        go  := Evaluate_Probit( pr, wsc_run.probit_thresholds( health_better ), wsc_run.Use_Random_Threshold );
                        if( go )then
                           event_count.add(
                              health_improving,
                              hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                           ad.health_status := Health_Status_Type'Pred( ad.health_status );
                        end if;
                        Log( "Health_Better pr = " & pr'Img & " go  "& go'Img  & " status now " & ad.health_status'Img );
                     elsif ad.health_status < Health_Status_Type'Last then 
                        pr := Health_Worse_Probit( ad, hh.hdata.region );
                        go  := Evaluate_Probit( pr, wsc_run.probit_thresholds( health_worse ), wsc_run.Use_Random_Threshold );
                        if( go )then
                           event_count.add(
                              health_worsening,
                              hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                           ad.health_status := Health_Status_Type'Succ( ad.health_status );
                        end if;
                        Log( "Health_Worse_Probit pid=" & ad.pid'Img & " pr = " & pr'Img & " go  "& go'Img & " status now " & ad.health_status'Img );
                     end if;       
                     
                     case ad.health_status is
                        when na => event_count.add( health_na, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                        when excellent => event_count.add( health_excellent, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                        when good => event_count.add( health_good, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                        when fair => event_count.add( health_fair, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                        when poor => event_count.add( health_poor, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                        when very_poor => event_count.add( health_very_poor, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                     end case;
                     
                     Recalculate_Regressors( ad, hh.Num_Adults, hh.Num_Children, hh.hdata );
                     for t in Task_Type'First .. bathing_or_showering loop
                        pr := Get_ADL_Change_Probit( ad, t, improvement, hh.hdata.region );
                        go  := Evaluate_Probit( pr, wsc_run.probit_thresholds( adl_improve ), wsc_run.Use_Random_Threshold );
                        Log( "Get_ADL_Change_Probit; improve " & t'Img & " pr = " & pr'Img & " go "& go'Img );
                        if( go )then
                           if( ad.fitness( t ).diff > Difficulty_Type'First ) then
                              ad.fitness( t ).diff := Difficulty_Type'Pred( ad.fitness( t ).diff );
                              case t is
                                 when manage_stairs => event_count.add( adl_manage_stairs_difficulty_improving, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                 when get_around_house => event_count.add( adl_get_around_house_difficulty_improving, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                 when get_in_or_out_of_bed => event_count.add( adl_get_in_or_out_of_bed_difficulty_improving, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                 when cut_toenails => event_count.add( adl_cut_toenails_difficulty_improving, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                 when bathing_or_showering => event_count.add( adl_bathing_or_showering_difficulty_improving, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                 -- when walk_down_road => event_count.add( adl_walk_down_road_difficulty_improving, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                              end case;
                           end if;
                           if( ad.fitness( t ).help > Help_Needed_Type'First ) then
                              ad.fitness( t ).help := Help_Needed_Type'Pred( ad.fitness( t ).help );
                              case t is
                                 when manage_stairs => event_count.add( adl_manage_stairs_help_improving, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                 when get_around_house => event_count.add( adl_get_around_house_help_improving, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                 when get_in_or_out_of_bed => event_count.add( adl_get_in_or_out_of_bed_help_improving, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                 when cut_toenails => event_count.add( adl_cut_toenails_help_improving, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                 when bathing_or_showering => event_count.add( adl_bathing_or_showering_help_improving, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                 -- when walk_down_road => event_count.add( adl_walk_down_road_help_improving, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                              end case;
                           end if;
                        else
                           pr := Get_ADL_Change_Probit( ad, t, worsening, hh.hdata.region );
                           go  := Evaluate_Probit( pr, wsc_run.probit_thresholds( adl_worsen ), wsc_run.Use_Random_Threshold );
                           Log( "Get_ADL_Change_Probit; worsen " & t'Img & " pr = " & pr'Img & " go  "& go'Img );
                           if( go )then
                              if( ad.fitness( t ).diff < Difficulty_Type'Last )then
                                 ad.fitness( t ).diff := Difficulty_Type'Succ( ad.fitness( t ).diff );
                                 case t is
                                    when manage_stairs => event_count.add( adl_manage_stairs_difficulty_worsening, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                    when get_around_house => event_count.add( adl_get_around_house_difficulty_worsening, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                    when get_in_or_out_of_bed => event_count.add( adl_get_in_or_out_of_bed_difficulty_worsening, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                    when cut_toenails => event_count.add( adl_cut_toenails_difficulty_worsening, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                    when bathing_or_showering => event_count.add( adl_bathing_or_showering_difficulty_worsening, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                    -- when walk_down_road => event_count.add( adl_walk_down_road_difficulty_worsening, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                 end case;
                              end if;
                              if( ad.fitness( t ).help < Help_Needed_Type'Last )then
                                 ad.fitness( t ).help := Help_Needed_Type'Succ( ad.fitness( t ).help );
                                 case t is
                                    when manage_stairs => event_count.add( adl_manage_stairs_help_worsening, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                    when get_around_house => event_count.add( adl_get_around_house_help_worsening, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                    when get_in_or_out_of_bed => event_count.add( adl_get_in_or_out_of_bed_help_worsening, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                    when cut_toenails => event_count.add( adl_cut_toenails_help_worsening, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                    when bathing_or_showering => event_count.add( adl_bathing_or_showering_help_worsening, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                    -- when walk_down_road => event_count.add( adl_walk_down_road_help_worsening, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );
                                 end case;
                              end if;
                           end if;
                        end if;
                     end loop;
                     
                  end if; -- 0 65s only
                  Recalculate_Regressors( ad, hh.Num_Adults, hh.Num_Children, hh.hdata );               
                  ad.activities_of_daily_living_score := ad.regressors( adlscore );
                  
                  if( ad.employment_status /= retired )then
                     pr := Retire_Probit( ad, hh.hdata.region, hh.wave ); 
                     go  := Evaluate_Probit( pr, wsc_run.probit_thresholds( retire ), wsc_run.Use_Random_Threshold ) or ad.age > 70;
                     Log( "Retire_Probit; pr = " & pr'Img & " go "& go'Img );
                     if( go )then
                        ad.employment_status := retired;
                        for i in gross_wage .. net_self_employment loop
                           ad.current_income( i ) := 0.0;
                           -- ad.annual_income( i ) := 0.0;
                        end loop;
                        ad.current_income( ni ) := 0.0;
                        -- ad.annual_income( ni ) := 0.0;
                        event_count.add( retire, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );                        
                        -- FIXME scale down inctax
                     end if;
                  end if;
               end;
            end loop adults;
         end;
      end loop benefit_units;
   end Predict_Health_Needs_And_Employment_Status;
   
   procedure Move_New_People_To_Care_Homes( 
      hh        : in out Household;
      wsc_run : Run;
      event_count : in out ev_count.Recorder ) is 
   use Model.WSC.Household.Regressions;
   begin
      benefit_units:
      for buno in 1 .. hh.num_benefit_units loop
         declare
            bu : Benefit_Unit renames hh.benefit_units( buno );
         begin
            adults:
            for adno in 1 ..  bu.num_adults loop
               declare
                  ad : Person renames hh.benefit_units( buno ).adults( adno );
                  pr : Amount;
                  go : Boolean;
               begin
                  if( ad.pid < 0 )then -- like this so we get full hh print out
                     Log( "sernum missing for hh " & To_String( hh ));
                     Assert( ad.pid > 0, "pid missing for hh " & hh.hid'Img );
                  end if;
                  if( ad.age >= 65 ) and ( ad.years_in_residential_care <= 0 ) and ( not ad.dies_this_period ) then
                     pr := To_Care_Probit( ad ); 
                     go  := Evaluate_Probit( pr, wsc_run.probit_thresholds( to_care ), wsc_run.Use_Random_Threshold );
                     if( go )then
                        ad.years_in_residential_care := 1;
                        event_count.add( to_care_home, hh.wave, hh.hid, ad.pid, ad.age, ad.sex, hh.Weight );                        
                     end if;
                  end if;
               end;
            end loop adults;
         end;
      end loop benefit_units;
   end Move_New_People_To_Care_Homes;
      
   procedure Create_Simulation_Data( 
      wsc_run           : Run; 
      monitor           : in out Model.WSC.Run_Settings.Model_Monitor'Class;
      do_reweighting    : Boolean;
      include_care_home : Boolean;
      event_count       : in out ev_count.Recorder;
      iteration         : Iteration_Number;
      uprate_type       : Type_Of_Uprating ) is

      use Model.WSC.Run_Settings;
      use Matrix_Functions;
      use Model.WSC.Household.Database;
      use Model.WSC.Household;
      use Base_Model_Types;
      use WSC_Enums;

      hid            : Sernum_Value;
      sernums        : Sernum_List;
      sernums_inc_residential : Sernum_List;
      sernums_only_residential : Sernum_List;
      sernums_excl_residential : Sernum_List;
      db            : DB_Type;
      hh             : Household;
      hh_last_period : Household;
      wave           : Waves := r;
      num_households : Households_Per_Wave_Number;
      new_sernum     : Sernum_Value;
      data_dir : constant String := Model.WSC.Global_Settings.Physical_Root & "/data/wales/";
      itstr : constant String := Iteration_Number'Image( iteration )( 2 .. Iteration_Number'Image( iteration )'Length );
      target_dir : constant String := data_dir & "estimated/"  & TS( wsc_run.dataset_name ) & "/";
      aborting : Boolean;
      capital_rand : Wealth_Random_Array; -- use same random for one household for capital for each period
   begin
      Ada.Directories.Create_Path( target_dir );
      monitor.Set_Stage( running );
      
      Log( "Reading data from " & Model.WSC.Global_Settings.Physical_Root & "data/wales" );
      Open( db, data_dir & "actual/",  actual, 1 );
      
      Write_Settings( 
         target_dir & "/wsc_run.txt",
         wsc_run );
      Create( db, target_dir, estimated, iteration );
      db.Get_Household_Sernums( r, sernums ); 
      num_households := Positive( sernums.Length );
      Log( "hh count before care home " & num_households'Img );

      Each_Household:
      for hhno in 1 .. num_households loop
         if( hhno mod 10 = 0 )then
            monitor.Set_Counter( 1, hhno ); 
            exit when monitor.Is_Aborting;
         end if;
         hid := sernums.Element( hhno );
         hh := db.Get_Household( 
            wave => r, 
            hid  => sernums.Element( hhno ));
         Assert( hh.hid = hid, " sernums mismatch at " & hh.hid'Img & " vs index hid=" & hid'Img );
         hh_last_period := hh;            
         capital_rand := Make_Wealth_Randoms;
         -- 
         -- FIXED: DON'T infer wealth beyond initial period: use uprating instead
         --
         -- Infer_Wealth( hh, wsc_run, event_count );
         Log( "Test_Create_Simulation_Data; on hh " & hhno'Img & " sernum " & hh.hid'Img );
         future_waves:
         loop
            new_sernum := Make_New_Sernum( Waves'Succ( hh.wave ), hhno, 1 );
            hh.hid := new_sernum;
            hh.hdata.hid := new_sernum;
            Age( hh, wsc_run, event_count );
            Infer_Wealth( hh, wsc_run, event_count, capital_rand );
            Uprate( hh, wsc_run, uprate_type );
            Impute_Care_Amounts( hh, hh_last_period, wsc_run, event_count );
            -- split here?
            Predict_Health_Needs_And_Employment_Status( hh, wsc_run, event_count );
            Kill_Household_Members( hh, wsc_run, event_count );
            if( hh.Num_People = 0 )then
               Log( "noone left in household" );
               event_count.Add( household_ends, wave, hh.hid, 0, 65, Gender_Type'First, hh.Weight );
               exit future_waves;
            end if;            
            Move_New_People_To_Care_Homes( hh, wsc_run, event_count );
            hh_last_period := hh;
            Log( "Writing Household wave :" & hh.hdata.wave'Img & " sernum " & hh.hdata.hid'Img );
            db.Write_Household( hh );
            exit future_waves when hh.wave = Simulation_Waves'Last;
         end loop future_waves;
      end loop Each_Household;
      db.Close;
   end Create_Simulation_Data;
   
   procedure Create_Simulation_Data( 
      wsc_run           : Run; 
      monitor           : in out Model.WSC.Run_Settings.Model_Monitor'Class;
      uprate_type       : Type_Of_Uprating ) is
      use Model.WSC.Run_Settings;
      package t_counter renames  Transition_Events.Transition_Events_Counter; 
      include_care_home : constant Boolean := True; -- FIXME to run settings
      outf        : File_Type;
      target_dir : constant String := Model.WSC.Global_Settings.Physical_Root & "/data/wales/estimated/"  & TS( wsc_run.dataset_name ) & "/";
   begin
      Ada.Directories.Create_Path( target_dir );
      Create( outf, Out_File,  target_dir & "data_creation_events.csv" );
      for iteration in 1 .. wsc_run.Num_Iterations loop
         declare
            event_count : Transition_Events.Transition_Events_Counter.Recorder;
         begin
            monitor.Set_Stage( pre_calculations ); 
            exit when monitor.Is_Aborting;
            Create_Simulation_Data( 
               wsc_run           => wsc_run, 
               monitor           => monitor, 
               do_reweighting    => False,
               include_care_home => include_care_home,
               event_count       => event_count,
               iteration         => iteration,
               uprate_type       => uprate_type );
            Put_Line( "Iteration " & Iteration_Number'Image( iteration ));
            Put_Line( outf, "Unweighed Counts" );
            Put_Line( outf, t_Counter.To_String( event_count.Get_Raw, ONS_Definitions.age_65_to_74 ));
            Put_Line( outf, "Weighed Counts" );
            Put_Line( outf, t_counter.To_String( event_count.Get_Weighted, ONS_Definitions.age_65_to_74 ));
            Put_Line( outf, "Unweighed Counts, per 1000 population" );
            Put_Line( outf, t_counter.To_String( t_counter.To_Per_1000_People( event_count.Get_Raw ), ONS_Definitions.age_65_to_74 ));
            Put_Line( outf, "Weighed Counts, per 1000 population" );
            Put_Line( outf, t_counter.To_String( t_counter.To_Per_1000_People( event_count.Get_Weighted ), ONS_Definitions.age_65_to_74 ));
         end;
      end loop;
      Close( outf );
      monitor.Set_Stage( complete );
   end Create_Simulation_Data;
   
end  Model.WSC.Household.Transitions;
