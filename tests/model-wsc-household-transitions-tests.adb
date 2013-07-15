--
--  $Author: graham_s $
--  $Date: 2011-04-06 15:56:14 +0100 (Wed, 06 Apr 2011) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );

with Ada.Command_Line;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

with AUnit.Assertions;   
with GNAT.Regpat;

with BHPS.Binary_IO.Conversion_To_Binary;
with BHPS.Binary_IO;
with BHPS.Print;
with BHPS.State_Changes;
with BHPS.XLookup;
with BHPS;
with BHPS_Enums;
with BHPS_Indexes;
with Base_Model_Types;

with Connection_Pool;
with Environment;

with Indexed_Sequential_IO;
with Maths_Functions.Weights_Generator;
with Maths_Functions;
with Weighting_Commons;

with Model.Run_Settings;
with Model.WSC.BHPS_Data_Creation_Libs;
with Model.WSC.Global_Settings;
with Model.WSC.Household.Database;
with Model.WSC.Household.Examples;
with Model.WSC.Household.IO;
with Model.WSC.Household.Regressions;
with Model.WSC.Household.Weights;
with Model.WSC.Household.Transitions;
with Model.WSC.Household;
with Model.WSC.Run_Declarations;
with Model.WSC.Uprate;
with WSC_Enums;
with GNATColl.Traces;
with Transition_Events;
with T_Utils;
with Text_Utils;
with Run_IO;

package body Model.WSC.Household.Transitions.Tests is
   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;
   use Ada.Text_IO;
   use Text_Utils;
   use BHPS.Print;
   use Base_Model_Types;
   use Weighting_Commons;
   use Model.Run_Settings;
   
   default_wsc_run : Model.WSC.Run_Declarations.Run;
   default_username : Unbounded_String;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.HOUSEHOLD.TRANSITIONS.TESTS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   procedure Set_Up (T : in out Test_Case) is
   begin
      Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 1 ));
      default_username := TuS( Ada.Command_Line.Argument( 2 ));
      default_wsc_run := Run_IO.Get_New_Run_For( default_username );
      Maths_Funcs.Do_Debug( True );
   end Set_Up;

   procedure Shut_Down( T : in out Test_Case) is
   begin
      -- Close( log_file );
      null;
   end Shut_Down;
   
   function Update_Run_State(
      username       : Unbounded_String;
      run_id         : Natural;
      household      : Natural;
      year           : Year_Number;
      other_counter  : Natural;
      phase          : Phase_Type;
      health         : Health_Type := normal;
      other_counter2 : Natural := 0;
      other_counter3 : Natural := 0;
      other_counter4 : Natural := 0 ) return Boolean is
      use Text_Utils;
      aborting : Boolean := False; 
      run_state : State_Type;
   begin
      Put_Line( "run_id " & run_id'Img & " year " & year'Img & " household " & household'Img & " phase " & phase'Img );
      return aborting;
   end Update_Run_State;


   procedure Test_Delete_Person( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.WSC.Household.Examples;
   use Model.WSC.Run_Declarations;
      hh : Household := Get_Household( couple_bu_retired );-- single_retired_person
   begin
      Assert( hh.benefit_units( 1 ).Num_People = 2, "should have 2 people before deleting head" );
      hh.benefit_units( 1 ).Delete_Adult( 1, separation );
      Assert( hh.benefit_units( 1 ).Num_People = 1, "should have 1 person after deleting head" );
      Assert( hh.benefit_units( 1 ).adults( 1 ).sex = female, "head should now be female" &  To_String( hh.benefit_units( 1 ).adults( 1 )));
      Assert( hh.benefit_units( 1 ).adults( 1 ).marital_status = separated, "should be separated was " & To_String( hh.benefit_units( 1 ).adults( 1 )));
      Assert( hh.benefit_units( 1 ).adults( 1 ).partner_status = neither, " partner_status should be neither was " & To_String( hh.benefit_units( 1 ).adults( 1 )));
     
      hh := Get_Household( couple_bu_retired );-- single_retired_person
      hh.benefit_units( 1 ).Delete_Adult( 2, death );
      Assert( hh.benefit_units( 1 ).adults( 1 ).sex = male, "head should now be female" &  To_String( hh.benefit_units( 1 ).adults( 1 )));
      Assert( hh.benefit_units( 1 ).adults( 1 ).marital_status = widowed, "should be widowed was " & To_String( hh.benefit_units( 1 ).adults( 1 )));
      Assert( hh.benefit_units( 1 ).adults( 1 ).partner_status = neither, " partner_status should be neither was " & To_String( hh.benefit_units( 1 ).adults( 1 )));
   end Test_Delete_Person;
   
  
   procedure Test_Health( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.WSC.Household.Examples;
   use Model.WSC.Run_Declarations;
      hh             : Household;-- single_retired_person 
      hh_list        : Household_List;
      deaths         : Natural := 0;
      event_log      : Transition_Events.Transition_Events_Counter.Recorder;
   begin
      for htype in young_single .. old_sick_single_male loop
         Log( "**** " & htype'Img & " *** " ); 
         hh := Get_Household( htype ); 
         Log( "Initial state" );
         Log( To_String( hh ));
         ageing_loop:
         for wave in Simulation_Waves loop
            Age( hh, default_wsc_run, event_log );
            declare
               ad : Person renames hh.benefit_units( 1 ).adults( 1 );
            begin
               Log( "hh type " & htype'Img & " age is now " & ad.age'Img & " adlscore " & Format( ad.activities_of_daily_living_score ));
            end;
            Predict_Health_Needs_And_Employment_Status( hh, default_wsc_run, event_log );
         end loop ageing_loop;
      end loop;
   end Test_Health;

   
   procedure Test_Cumulative_Normal( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      package MF is new Maths_Functions( Amount );
      p          : Amount := 0.0;
      c          : Amount;
      inv        : Probability;
      prob       : Probability;
      diff       : Amount;
      happens    : Boolean;
      happens_nr : Boolean;
   begin
      loop
         p := p + 0.00001;
         exit when p >= 1.0;
         c := MF.Inverse_Cumulative_Normal( p );
         inv := MF.Cumulative_Normal( c );
         diff := p - inv;
         Assert( abs( diff ) < 1.0E-9, " diff was " & diff'Img );
         Log( p'Img & "," & c'Img & "," & inv'Img & "," & diff'Img );
      end loop;
      p := -100.0;
      loop
         p := p + 0.10;
         exit when p >= 100.0;
         happens := MF.Evaluate_Probit( p );
         happens_nr := MF.Evaluate_Probit( p, 0.5, False );
         prob := MF.Cumulative_Normal( p );
         Log( p'Img & "," & prob'Img & "," & happens'Img & "," & happens_nr'Img );
      end loop;
      -- check extremes for overflows
      inv := MF.Cumulative_Normal( Real'Last/2.0 );
      inv := MF.Cumulative_Normal( Real'First/2.0 );
   end Test_Cumulative_Normal;   
   
   
  
   procedure Test_Mortgages( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      m : Amount;
   begin
      m := Net_Present_Value( 25_000.0, 6, 0.1 );
      Assert( Nearly_Equal( m, 108_881.517, 0.01 ), "NPV( 25000, 6, 0.1 ) should be 108,881.517 but was " & m'Img );
   end Test_Mortgages;

   subtype Age_Band is Positive range 1 .. 15;
   
   procedure Test_Create_Simulation_Data_Via_Func( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      wsc_run : Model.WSC.Run_Declarations.Run := default_wsc_run;
      event_count : Transition_Events.Transition_Events_Counter.Recorder;
      monitor : Model.Run_Settings.Model_Monitor;
   begin
 
      Transitions.Create_Simulation_Data( 
         wsc_run           => wsc_run, 
         monitor           => monitor, 
         do_reweighting    => False,
         include_care_home => False,
         event_count       => event_count,
         iteration         => 1,
         uprate_type       => no_uprating );
   end Test_Create_Simulation_Data_Via_Func;
   
   
   procedure Test_Sort( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      l       : Person_Ordering_List;
      po      : Person_Ordering_Record;
      po_last : Person_Ordering_Record;
   begin
      for i in 1 .. 200 loop
         po.hid := Sernum_Value( i );
         po.pid := Sernum_Value( i );
         po.pno := 1;
         po.weight := 1.0;
         po.v := Maths_Funcs.Random_0_To_1;
         l.Append( po );
      end loop;
      Sort_By_V( l );
      for i in reverse 1 .. 200 loop
         po := l.Element( i );
         Put_Line( To_String( po ));
         if( i < 200 )then
            Assert( po.v < po_last.v, "pos out of order " & i'Img );
         end if;
         po_last := po;
      end loop;
   end Test_Sort;
   
   --------------------
   -- Register_Tests --
   --------------------
   procedure Register_Tests( t : in out Test_Case ) is
      use AUnit.Test_Cases.Registration;
   begin
      -- Register_Routine( t, Test_Make_Care_Home_Population'Access, "Test_Make_Care_Home_Population" );
      Register_Routine( t, Test_Mortgages'Access, "Test Mortgages" );
      Register_Routine( t, Test_Delete_Person'Access, "Test Delete Person" );
      Register_Routine ( t, Test_Health'Access, "Test_Health" );
      Register_Routine ( t, Test_Sort'Access, "Test Sort" );
      -- Register_Routine ( t, Test_Create_Simulation_Data'Access, "Test_Create_Simulation_Data" );
      -- Register_Routine ( t, Test_Create_Simulation_Data_Via_Func'Access, "Test_Create_Simulation_Data_Via_Func" );
      -- Register_Routine (T, Test_Cumulative_Normal'Access, "Test_Cumulative_Normal" );
      -- Register_Routine( t, Test_Calmar_Live'Access, "Test_Calmar_Live" );
   end Register_Tests;

   ----------
   -- Name --
   ----------
   function Name ( t : Test_Case ) return Message_String is
   begin
      return Format("WSC.Household.Transitions.Tests");
   end Name;
begin

   Connection_Pool.Initialise(
         Environment.Get_Server_Name,
         Environment.Get_Username,
         Environment.Get_Password,
         50 );
         
end Model.WSC.Household.Transitions.Tests;
