--
--  $Author: graham_s $
--  $Date: 2011-04-06 15:56:14 +0100 (Wed, 06 Apr 2011) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );


with Ada.Assertions;   
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with ONS_Definitions;

with Model.Run_Settings;

with Model.WSC.BHPS_Data_Creation_Libs;

with Model.WSC.Global_Settings;
with Model.WSC.Household.Database;
with Model.WSC.Household.Transitions;
with Model.WSC.Household.Weights;
with Model.WSC.Household;
with Model.WSC.Run_Declarations;
with Model.WSC.Uprate;
with Model.WSC.Regressors;

with Base_Model_Types;
with Transition_Events;
with Format_Utils;
with Text_Utils;
with WSC_Enums;
with GNATColl.Traces;

with Run_IO;
with Connection_Pool;
with Environment;

with GNATColl.Traces;

procedure Transitions_Runner is

   use Ada.Assertions;
   use Ada.Text_IO;
   use Ada.Calendar;
   use Text_Utils;
   use Ada.Strings.Unbounded;
   
   use Base_Model_Types;
   
   use Model.Run_Settings;
   use Model.WSC.Run_Declarations;
   use Model.WSC.Household.Transitions;
   use Model.WSC.Regressors;
   use ONS_Definitions;
   use WSC_Enums;
   
   package t_counter renames  Transition_Events.Transition_Events_Counter; 
   
   package UK_Format_Utils is new Format_Utils( Counter_Type => Counter_Type, Float_Type => Rate );
   use UK_Format_Utils;   
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "TRANSITIONS_RUNNER" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;
    
   procedure Accumulate_Data( wsc_run : Run ) is
   use Model.WSC.Household.Database;
   use Model.WSC.Household;
      type DA is array( Waves, Gender_Type,  Age_Band ) of Amount;
      type DP is array( Waves ) of Amount;
      total_health_scores, hhld, care_home : DA  := ( others=>( others => ( others => 0.0 )));
      
      unweighted_popn    : DP := ( others => 0.0 );
      weighted_popn      : DP := ( others => 0.0 );
      unweighted_popn_po : DP := ( others => 0.0 );
      weighted_popn_po   : DP := ( others => 0.0 );
      sernums            : Sernum_List;
      db                 : DB_Type;
      iteration          : Iteration_Number := 1;
   begin
      Open( db, Model.WSC.Global_Settings.Physical_Root & "data/wales",  actual, 1 );
      Open( db, wsc_run.Qualified_Run_Directory( iteration ) & "/data/final/", estimated, iteration );
      waves_loop:
      for wave in Simulation_Waves loop         
         db.Get_Household_Sernums( wave, sernums );
         declare
            num_households : constant Positive := Positive( sernums.Length );
            p              : Age_Band;
            hh             : Household;
            weight         : Amount;
         begin
            hhlds:
            for hhno in 1 .. num_households loop
               hh := db.Get_Household( 
                  wave => wave, 
                  hid  => sernums.Element( hhno ));
               weight := hh.hdata.weights( extended_2 ) *  WEIGHT_MULTIPLIER;
               unweighted_popn( wave ) := unweighted_popn( wave ) + Amount( hh.Num_People );
               weighted_popn( wave ) := weighted_popn( wave ) + ( Amount( hh.Num_People ) * weight );
               for buno in 1 .. hh.num_benefit_units loop
                  for adno in 1 .. hh.benefit_units( buno ).num_adults loop
                     declare
                        ad  : Person renames hh.benefit_units( buno ).adults( adno );
                     begin
                        p   := Ons_Definitions.Get_Age_Band( ad.age );
                        if( ad.age >= 65 )then
                           unweighted_popn_po( wave ) := unweighted_popn_po( wave ) + 1.0;
                           weighted_popn_po( wave ) := weighted_popn_po( wave ) + weight;
                        end if;
                        if( ad.years_in_residential_care > 0 )then
                           Care_Home( wave, ad.sex, p ) := Care_Home( wave, ad.sex, p ) + weight;
                        else
                           hhld( wave, ad.sex, p ) := hhld( wave, ad.sex, p ) + weight;
                           total_health_scores( wave, ad.sex, p ) := total_health_scores( wave, ad.sex, p ) + ( ad.regressors( adlscore ) * weight );
                        end if;
                     end;
                  end loop;
                  for chno in 1 .. hh.benefit_units( buno ).num_children loop
                     declare
                        ch : Person renames hh.benefit_units( buno ).children( chno );
                     begin
                        p := Ons_Definitions.Get_Age_Band( ch.age );
                        hhld( wave, ch.sex, p ) := hhld( wave, ch.sex, p ) + weight;
                        total_health_scores( wave, ch.sex, p ) := total_health_scores( wave, ch.sex, p ) + ( ch.regressors( adlscore ) * weight );
                     end;
                  end loop;
               end loop;
            end loop hhlds;
         end;
      end loop waves_loop;
      db.Close;
      for wave in Simulation_Waves loop
         Put_Line( " ========= WAVE " & wave'Img & " ========== (" & Year_From_Wave( wave )'Img & ")" );
         Put_Line( " popn in hhlds " );
         Put_Line( " Male popn, average ADL, Female popn, Average ADL" );
         for p in Age_Band loop
            Put( p'Img & ", " );
            for sex in Gender_Type loop
               declare
                  avhealth : Amount := 0.0;
               begin
                  if hhld( wave, sex, p ) > 0.0 then
                     avhealth := total_health_scores( wave, sex, p ) / hhld( wave, sex, p );
                  end if;
                  Put( Format( hhld( wave, sex, p )) & ",  " );
                  Put( Format( avhealth ) & ", " );
               end;
            end loop;
            New_Line;
         end loop;
         New_Line;
         
         Put_Line( " popn in care homes " );
         Put_Line( " Male popn, average ADL, Female popn, Average ADL" );
         for p in Age_Band loop
            Put( p'Img & ", " );
            for sex in Gender_Type loop
               Put( Format( care_home( wave, sex, p )) & ",  ");
            end loop;
            New_Line;
         end loop;
         New_Line;
         Put_Line( "Whole Population: " );
         Put_Line( "population (unweighted)," & Format( unweighted_popn( wave )));
         Put_Line( "population (weighted)," & Format( weighted_popn( wave )));
         Put_Line( "Pensioners Only: " );
         Put_Line( "population (unweighted)," & Format( unweighted_popn_po( wave )));
         Put_Line( "population (weighted)," & Format( weighted_popn_po( wave )));
         New_Line;
      end loop;
   end Accumulate_Data;


   wsc_run     : Run; 
   run_dir     : Unbounded_String;
   username      : Unbounded_String;
   dataset_name  : Unbounded_String;
   outf        : File_Type;
   monitor     : Model.Run_Settings.Model_Monitor;
   include_care_home : Boolean := False;
begin
   if Ada.Command_Line.Argument_Count /= 5 then
      Put_Line( "use: global_settings username out_file num_iterations dataset_name " );
      return;
   end if;
   Connection_Pool.Initialise(
         Environment.Get_Server_Name,
         Environment.Get_Username,
         Environment.Get_Password,
         50 );

   Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 1 ));   
   username := TuS( Ada.Command_Line.Argument( 2 ));
   Create( outf, Out_File, Ada.Command_Line.Argument( 3 )); 
   
   wsc_run := Run_IO.Get_New_Run_For( username );
   
   wsc_run.Num_Iterations := Positive'Value( Ada.Command_Line.Argument( 4 ));
   wsc_run.dataset_name := TuS( Ada.Command_Line.Argument( 5 ));
   
   Put_Line( "Made run dir as " & TS( run_dir ));
   Put_Line( outf, "include care home, " & Boolean'Image( include_care_home ));
   run_dir := Create_Directories_For_Run( wsc_run );    
   for iteration in 1 .. wsc_run.Num_Iterations loop
      declare
         event_count : Transition_Events.Transition_Events_Counter.Recorder;
      begin
         Model.WSC.Household.Transitions.Create_Simulation_Data( 
            wsc_run           => wsc_run, 
            monitor           => monitor, 
            do_reweighting    => False,
            include_care_home => include_care_home,
            event_count       => event_count,
            iteration         => iteration,
            uprate_type       => no_uprating );
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
end Transitions_Runner;
