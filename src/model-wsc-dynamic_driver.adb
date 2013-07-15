with Ada.Assertions;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Base_Model_Types;
with DB_Commons;
with Model.Run_Settings;
with Model.WSC.Global_Settings;
with Model.WSC.Household.Transitions;
with Model.WSC.Household;
with Model.WSC.Results;
with Model.WSC.Static_Calculator;
with Model.WSC.Results.DAO;
with Model.WSC.Output.DAO;
with UAP_Threshold_IO;
with Text_Utils;
with GNATColl.Traces;

package body Model.WSC.Dynamic_Driver is
     
   package mhh renames Model.WSC.Household;
   package dao renames Model.WSC.Results.DAO;

   use Ada.Assertions;
   use Model.WSC.Household;
   use Model.WSC.Household.Transitions;
   
   use Ada.Calendar;
   use Text_Utils;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Summary_Items_Package;
   
   Min_Age_To_Load : constant Age_Range := 50;
 
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.DYNAMIC_DRIVER" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;
   
   procedure Make_Needs_Score_Thresholds(
      wsc_run     : Run;
      db          : in out DB_Type;
      wave        : Waves;
      sysno       : System_Number;
      iteration   : Positive;
      proportions : UAP_Array;
      calculator  : Make_Needs_Score_Access;
      thresholds  : out UAP_Array;
      weighter    : mww.Weighter;
      monitor     : in out Model_Monitor'Class ) is
      
      type Score_And_Weight is record
         score  : Amount;
         weight : Amount;
      end record;
      
      function Compare_Score_And_Weight( s1, s2  : Score_And_Weight ) return Boolean is
      begin
         return s1.score > s2.score;
      end  Compare_Score_And_Weight;
      
      package Needs_Package is new Ada.Containers.Vectors( Element_Type => Score_And_Weight, Index_Type => Positive );
      subtype Needs_List is Needs_Package.Vector;
      package Needs_Sorter is new Needs_Package.Generic_Sorting( "<" =>  Compare_Score_And_Weight );
      sernums        : Sernum_List;
      needs          : Needs_List;
      sw             : Score_And_Weight;
      o65_popn       : Amount := 0.0;
      hh             : mhh.Household;
      num_households : Households_Per_Wave_Number;
      year           : Year_Number := Year_From_Wave( wave );
      weight         : Amount;
   begin
      thresholds := ( others => Amount'First );
      db.Get_Household_Sernums( wave, sernums );
      num_households := Positive( sernums.Length );
      monitor.Set_Counter( 5, 3 );
      for hhno in 1 .. num_households loop
         if( hhno mod 10 = 0 )then
            monitor.Set_Counter( 1, hhno );      
            exit when monitor.Is_Aborting;
         end if;
         hh := db.Get_Household( 
            wave => wave, 
            hid  => sernums.Element( hhno ));
         weight := weighter.Get_Weight( hh.hid, wave );
         for buno in 1 .. hh.num_benefit_units loop
            for adno in 1 .. hh.benefit_units( buno ).num_adults loop
               if( hh.benefit_units( buno ).adults( adno ).age >= 65 )then
                  sw.weight := weight;
                  sw.score := calculator( hh.benefit_units( buno ).adults( adno ));
                  -- Log( "got score as " & Format( sw.score ) & " weight " & Format( sw.weight ));
                  o65_popn := o65_popn + weight;
                  needs.Append( sw ); 
               end if;
            end loop;
         end loop;
      end loop;                     
      Needs_Sorter.Sort( needs );
      declare
         use Needs_Package;
         c : Cursor := needs.First;
         popn : Amount := 0.0;
         pshare : Amount;
         ul : UAP_Level := UAP_Level'First;
      begin
         loop
            sw := Element( c );
            Next( c );
            popn := popn + sw.weight;
            pshare := 100.0 * ( popn / o65_popn );
            Log( "sorted; got score as " & Format( sw.score ) & 
                  " weight " & Format( sw.weight ) & 
                  " pshare " & Format( pshare ) & 
                  "proportions( " & ul'Img & " ) = " & 
                  proportions( ul )'Img );
            if pshare > proportions( ul ) then  
               popn := 0.0; -- since shares aren't cumulative
               loop
                  thresholds( ul ) := sw.score;
                  if( ul < UAP_Level'Last )then
                     ul := UAP_Level'Succ( ul );
                     -- should assert this? can never happen?               
                  end if;
                  exit when ul = UAP_Level'Last or proportions( ul ) /= 0.0;
               end loop;
            end if;
            exit when c = needs.Last;
         end loop;
      end;
      -- always qualify for 'none' ...
      thresholds( UAP_Level'Last ) := -99999999999.99; -- Amount'First;
      Log( "o65_popn " & Format( o65_popn ));
      for ul in UAP_Level loop
         Log( "threshold["&ul'Img&"]= " & thresholds( ul )'Img );
      end loop;
   end Make_Needs_Score_Thresholds;

   type UAP_By_Wave_Array is array( Waves ) of UAP_Array;

   
   procedure Load_Initial_Results_Incomes(
         wsc_run         : Run;
         db              : in out DB_Type;
         start_wave      : Waves;
         iteration       : Iteration_Number;
         monitor         : in out Model_Monitor'Class ) is
      wave_before_start_wave : Waves := Waves'Pred( start_wave );
      sernums                : Sernum_List;
      hh                     : mhh.Household;
      results                : Household_Result;
      num_households         : Households_Per_Wave_Number;
      year                   : Year_Number := Year_From_Wave( start_wave );
   begin
      Log( "Load_Initial_Results_Incomes entered" );
      db.Get_Household_Sernums( r, sernums );
      num_households := Positive( sernums.Length );
      Log( "Got " & num_households'Img & " households" );
      each_household:            
      for hhno in 1 .. num_households loop
         if( hhno mod 10 = 0 )then
            monitor.Set_Counter( 1, hhno );
            exit when monitor.Is_Aborting;
         end if;

         hh := db.Get_Household( 
            wave => r, 
            hid  => sernums.Element( hhno ));
         results.res.sernum := hh.hid;
         Log( "on household " & hh.hid'Img );
         
         hh.wave := wave_before_start_wave;
         hh.hdata.wave := wave_before_start_wave;
         -- create a simulation date so we can uprate initial values correctly 
         hh.hdata.current_simulated_date := 
            Model.WSC.Household.Transitions.Date_From_Wave( hh.wave, hh.hdata.interview_date );
            
         Model.WSC.Household.Transitions.Uprate( hh, wsc_run, uprate_from_base_data );
         results.res.num_benefit_units := hh.num_benefit_units;
         
         each_bu:
         for buno in 1 .. hh.num_benefit_units loop
            declare
               bu  : Benefit_Unit renames hh.benefit_units( buno );
               brs : Benefit_Unit_Result renames results.benefit_units( buno );
            begin
               brs.res.num_people := bu.Num_Adults;
               each_adult:
               for adno in  1 .. bu.Num_Adults loop
                  declare
                     prs  : Personal_Result renames brs.people( adno );
                     ad   : Person renames bu.adults( adno );
                  begin
                     Assert( ad.pid > 0, "adult with missing PID allowed through for household " & To_String( hh ));
                     if( ad.age >= 60 )then
                        prs.sernum := ad.pid;
                        prs.is_residential := ad.years_in_residential_care > 0;
                        for i in Calculated_Incomes loop
                           prs.income( i ) := ad.current_income( i ); 
                        end loop;
                     end if;
                  end;
               end loop each_adult; 
            end;      
         end loop each_bu;
         Log( "Setting intial results for hh " & hh.hid'Img & " wave " & hh.wave'Img );
         if( hh.Age_Of_Oldest_Member >= Min_Age_To_Load )then
            dao.Set( wsc_run, hh, 1, iteration, results, Min_Age_To_Load, True );   
            dao.Set( wsc_run, hh, 2, iteration, results, Min_Age_To_Load, True );
         end if;
      end loop each_household;
      Log( "Load_Initial_Results_Incomes exiting" );
   end Load_Initial_Results_Incomes;

   procedure Load_Previous_Waves_Results( 
      wsc_run    : Run;
      hh         : mhh.Household;
      sysno      : System_Number;
      iteration  : Iteration_Number;
      results    : in out Household_Result ) is
      last_wave  : Waves := Waves'Pred( hh.hdata.wave );
   begin
      bus:
      for buno in 1 .. hh.num_benefit_units loop
         declare
            bu  : Benefit_Unit renames hh.benefit_units( buno );
            brs : Benefit_Unit_Result renames results.benefit_units( buno );
         begin
            brs.res.num_people := bu.Num_Adults;
            adults:
            for adno in  1 .. bu.Num_Adults loop
               declare
                  prs            : Personal_Result renames brs.people( adno );
                  last_result    : Personal_Result;
                  ad             : Person renames bu.adults( adno );
                  client_sum     : Amount := 0.0;
                  la_sum         : Amount := 0.0;
                  gross_sum      : Amount := 0.0;
               begin
                  Assert( ad.pid > 0, "adult with missing PID allowed through for household " & To_String( hh ));
                  if( ad.age >= Min_Age_To_Load )then
                     --
                     -- FIXME really ugly
                     --
                     last_result := dao.Get( wsc_run, last_wave, sysno, iteration, ad.pid );
                     prs.lifetime_gross_payments := last_result.lifetime_gross_payments;
                     prs.lifetime_client_contributions := last_result.lifetime_client_contributions;
                     prs.lifetime_la_contributions := last_result.lifetime_la_contributions;
                     prs.lifetime_capital_contributions := last_result.lifetime_capital_contributions;
                     prs.highest_la_contribution := last_result.highest_la_contribution;
                     Log( "getting highest previous values of for " & ad.pid'Img & " last_wave " & last_wave'Img );
                     prs.highest_previous_income := dao.Highest_Values_Of( 
                        wsc_run,
                        last_wave, 
                        sysno,
                        iteration,
                        ad.pid );
                  end if;
               end;
            end loop adults;
         end; -- bu decl
      end loop bus;
      Log( "Exiting Load_Previous_Waves_Results" );
   end Load_Previous_Waves_Results;
               
   procedure Do_Main_Calculations_For_One_Wave( 
      iteration       : Positive;
      wave            : Waves;
      wsc_run         : Run;
      state           : in out State_Type;
      monitor         : in out Model_Monitor'Class;
      db              : in out DB_Type;
      pre_thresholds  : UAP_Array;
      sys_pre         : Parameters_Rec;
      post_thresholds : UAP_Array;
      sys_post        : Parameters_Rec;
      rand_list       : M_Randoms.Random_List;
      weighter        : mww.Weighter
      ) is
      
      res_file        : File_Type;
      inc_file        : File_Type;
      num_households  : Households_Per_Wave_Number;
      year            : constant Year_Number := Year_From_Wave( wave );
      output          : Outputs_Rec;
      pre_rand_list   : M_Randoms.Random_List := rand_list.Make_Copy;
      post_rand_list  : M_Randoms.Random_List := rand_list.Make_Copy;
      res_file_name   : String := wsc_run.Qualified_Output_Directory( iteration ) & "/results_" & wave'Img & ".sql";
      inc_file_name   : String := wsc_run.Qualified_Output_Directory( iteration ) & "/incomes_" & wave'Img & ".sql";
      is_first        : Boolean;
      is_last         : Boolean;
      ok              : Boolean;
      sernums         : Sernum_List;
   begin
      Log( "Main Calculations; on wave " & wave'Img );
      Create( inc_file, Out_File, inc_file_name );
      Create( res_file, Out_File, res_file_name );
      db.Get_Household_Sernums( wave, sernums );
      monitor.Set_Stage( running );
      num_households := Positive( sernums.Length );
      each_household:            
      for hhno in 1 .. num_households loop
         is_first := hhno = 1;
         is_last := hhno = num_households;
         declare
            hh : mhh.Household := db.Get_Household( 
               wave => wave, 
               hid  => sernums.Element( hhno ));
            pre_results     : Household_Result;
            post_results    : Household_Result;
            weight : Amount;
         begin
            Log( "on Household " & hh.hid'Img & " wave " & wave'Img );
            weight := weighter.Get_Weight( hh.hid, wave );            
            if( hh.Age_Of_Oldest_Member >= 60 )then
               Model.WSC.Household.Transitions.Uprate( hh, wsc_run, uprate_from_base_data );
               monitor.Assert( hh.wave = wave, "waves mismatch " & wave'Img & " vs " & hh.wave'Img );
               if( hhno mod 10 ) = 0 then
                  monitor.Set_Counter( 2, year );
                  monitor.Set_Counter( 1, hhno );
                  exit when monitor.Is_Aborting;
               end if;
               ------------------------------------------------------------------------------------------------------
               -- pre block
               ------------------------------------------------------------------------------------------------------
               Log( "starting base calculations for hhld " & hh.hid'Img );               
               Zero( pre_results, clear_historical => True );
               Load_Previous_Waves_Results( wsc_run, hh, 1, iteration, pre_results );
               Log( "Previous Results Loaded; sys1" );
               Calculate_One_Household( 
                  wave, 
                  hh, 
                  sys_pre, 
                  pre_results, 
                  wsc_run, 
                  pre_thresholds, 
                  pre_rand_list );
               Log( "sys1 calc complete" );   
               
               ------------------------------------------------------------------------------------------------------
               -- post block
               ------------------------------------------------------------------------------------------------------
               Log( "starting post calculations for hhld " & hh.hid'Img );               
               Zero( post_results, clear_historical => True );
               Load_Previous_Waves_Results( wsc_run, hh, 2, iteration, post_results );
               Log( "Previous Results Loaded; sys2" );
               Calculate_One_Household( 
                  wave, 
                  hh, 
                  sys_post, 
                  post_results, 
                  wsc_run, 
                  post_thresholds, 
                  post_rand_list );
               Log( "sys2 calc complete" );   
                  
               if( hhno < 2 )then
                  Log( To_String( hh ));
                  Log( To_String( pre_results));
                  Log( To_String( post_results));
                  Log( "Add to Outputs; Got weight as " & Format( weight ));
               end if;
               
               Add_To_Outputs( 
                  hh_ref   => hhno,
                  hh       => hh, 
                  year     => year,
                  weight   => weight,
                  result_1 => pre_results,
                  result_2 => post_results,
                  outputs  => output );
               Model.WSC.Results.DAO.Dump_As_SQL( 
                  res_file, 
                  inc_file, 
                  1, 
                  iteration,
                  wsc_run, 
                  is_first, 
                  False, -- 1st can't be last
                  hh, 
                  pre_results );
               Model.WSC.Results.DAO.Dump_As_SQL( 
                  res_file, 
                  inc_file, 
                  2, 
                  iteration,
                  wsc_run, 
                  False, -- 2nd can't be first  
                  is_last, 
                  hh, 
                  post_results );
            end if; -- over_60s_only;
         end; -- hhld declaration
      end loop each_household;
      Close( inc_file );
      Close( res_file );
      if( not monitor.Is_Aborting )then
         monitor.Set_Stage( generating_output );
         ok := DB_Commons.Load_Postgres( res_file_name, res_file_name & ".dump_results" );
         Log( "loading " & res_file_name & " results in " & ok'Img );
         ok := DB_Commons.Load_Postgres( inc_file_name, inc_file_name & ".dump_results" );
         Log( "loading " & inc_file_name & " results in " & ok'Img );
         Log( "saving output for iteration " & iteration'Img );
         output.summary_difference := output.summary_items_2 - output.summary_items_1;
         Model.WSC.Output.DAO.Save( wsc_run, iteration, wave, output );
         Log( "output done" );
      end if;
   end Do_Main_Calculations_For_One_Wave;
   
   --
   -- TODO: there is a MUCH nicer version of the re-weighting on Model..Weights.ads,
   -- which I'd forgotten I'd written ... 
   --
   procedure Run_Model(
      wsc_run        : in Run;
      pre_sys        : in Parameters_Array;
      post_sys       : in Parameters_Array;
      state          : out State_Type;
      monitor        : in out Model_Monitor'Class ) is
   use Model.WSC.Results;
   use Model.WSC.Static_Calculator;
   use Model.Run_Settings;
      start_wave      : Waves;
      end_wave        : Waves;
      run_id_str      : constant String := "; username |" & TS( wsc_run.username ) & "| id " & wsc_run.run_id'Img & ";";
   begin
      Log( "Run_Model; entered; " & run_id_str );
      start_wave := Wave_From_Year( wsc_run.Start_Year );
      end_wave := Wave_From_Year( wsc_run.End_Year );
      each_iteration:
      for iteration in 1 .. wsc_run.Num_Iterations loop
         monitor.Set_Counter( 3, iteration );
         declare
            pre_thresholds    : UAP_By_Wave_Array;
            post_thresholds   : UAP_By_Wave_Array;
            itstr             : constant String := Model.WSC.Global_Settings.Physical_Root & "data/randoms/randoms_" & Format( iteration ) & ".txt"; 
            db                : DB_Type;
            wave              : Waves := r;
            year              : Year_Number;
            rand_list         : M_Randoms.Random_List;
            weighter          : mww.Weighter;
            weight_error      : Eval_Error_Type;
            weight_iterations : Positive;
         begin
            Log( "opening random number file |" & itstr & "| " & run_id_str ); 
            rand_list.Load( itstr );
            Log( "Randoms Reset" & run_id_str );
            
            Open( db, Model.WSC.Global_Settings.Physical_Root & "data/wales/actual/", actual, 1 );
            Open( db, Model.WSC.Global_Settings.Physical_Root & "data/wales/estimated/" & TS( wsc_run.dataset_name ) & "/",  estimated, iteration );
            
            monitor.Set_Counter( 5, 1 );
            exit when monitor.Is_Aborting;
            weighter.Create( db, wsc_run, monitor, weight_iterations, weight_error );

            exit when monitor.Is_Aborting;
            monitor.Set_Counter( 5, 3 );
            exit when monitor.Is_Aborting;

            Log( "initial monitor OK" & run_id_str );
            Load_Initial_Results_Incomes( 
               wsc_run,
               db,
               start_wave,
               iteration,
               monitor );
            exit each_iteration when monitor.Is_Aborting;
            Log( "Load_Initial_Results_Incomes OK" & run_id_str );
            monitor.Set_Counter( 5, 4 );
            thresholds_for_each_wave:
            for wave in start_wave .. end_wave loop
               year := Year_From_Wave( wave );
               monitor.Set_Counter( 2, year );
               Log( "making thresholds; on wave " & wave'Img & run_id_str );
               Make_Needs_Score_Thresholds(
                  wsc_run,
                  db,
                  wave,
                  1,
                  iteration,
                  pre_sys( year ).social_care.needs_assessment_rules.uap_category,
                  Rec_Care_Probit'Access,
                  pre_thresholds( wave ),
                  weighter,
                  monitor );
               exit each_iteration when monitor.Is_Aborting;
               Uap_Threshold_IO.Save( 
                  run_id    => wsc_run.run_id,
                  username  => wsc_run.username,
                  sysno     => 1,
                  iteration => iteration,
                  wave      => wave,
                  uaps      => pre_thresholds( wave ));
                Make_Needs_Score_Thresholds(
                  wsc_run,
                  db,
                  wave,
                  2,
                  iteration,
                  post_sys( year ).social_care.needs_assessment_rules.uap_category,
                  Rec_Care_Probit'Access,
                  post_thresholds( wave ),
                  weighter,
                  monitor );
               exit each_iteration when monitor.Is_Aborting;
               Uap_Threshold_IO.Save( 
                  run_id    => wsc_run.run_id,
                  username  => wsc_run.username,
                  sysno     => 2,
                  iteration => iteration,
                  wave      => wave,
                  uaps      => post_thresholds( wave ));
            end loop thresholds_for_each_wave;
            Log( "Main Calculations; running from wave " & start_wave'Img & " to " & end_wave'Img & run_id_str );
            -- monitor.Reset;
            do_calculations_for_each_wave:
            for wave in start_wave .. end_wave loop
               year := Year_From_Wave( wave );
               monitor.Set_Counter( 2, year );
               Do_Main_Calculations_For_One_Wave( 
                  iteration,
                  wave,
                  wsc_run,
                  state,
                  monitor,
                  db, 
                  pre_thresholds( wave ), 
                  pre_sys( year ), 
                  post_thresholds( wave ), 
                  post_sys( year ),
                  rand_list,
                  weighter );
               exit each_iteration when monitor.Is_Aborting;
            end loop do_calculations_for_each_wave;
            Close( db );
            exit when Monitor.Is_Aborting;
            Log( "Closing db for iteration " & iteration'Img & " abort_status " & monitor.Is_Aborting'Img & run_id_str );
         end; -- output declaration
      end loop each_iteration;
      monitor.Set_Stage( complete );
      Log( "exiting run" & run_id_str );
   end Run_Model;
   
end Model.WSC.Dynamic_Driver;
