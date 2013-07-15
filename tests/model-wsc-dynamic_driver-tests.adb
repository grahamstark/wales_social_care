--
--  $Author: graham_s $
--  $Date: 2011-04-06 15:56:14 +0100 (Wed, 06 Apr 2011) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );

with Ada.Command_Line;
with Ada.Direct_IO;

with AUnit.Assertions;   
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

with Base_Model_Types;

with Model.WSC.Global_Settings;
with Model.WSC.Household.Database;
with Model.WSC.Household.Examples;
with Model.WSC.Household.IO;
with Model.WSC.Household.Regressions;
with Model.WSC.Household.Transitions_Basic;
with Model.WSC.Household;
with Model.WSC.Parameters.IO;
with Model.WSC.Results.IO;
with Model.WSC.Results;
with Model.WSC.Run_Declarations;
with Model.WSC.Uprate;
with Model.WSC.Results.IO;
with Model.WSC.Results;
with Model.WSC.Static_Calculator;
with WSC_Enums;

with GNATColl.Traces;

with T_Utils;


package body Model.WSC.Dynamic_Driver.Tests is
   
   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;
   use Ada.Text_IO;
   use WSC_Enums;
   use Model.WSC.Results.IO;
   use Model.WSC.Results;
   use Model.WSC.Static_Calculator;
   
   default_wsc_run : Model.WSC.Run_Declarations.Run;
   params : Parameters_Array;
   rand_list : M_Randoms.Random_List;
      
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.DYNAMIC_DRIVER.TESTS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   procedure Set_Up (T : in out Test_Case) is
   begin
      rand_list.Reset;
      default_wsc_run := Model.WSC.Run_Declarations.Read_Settings( Ada.Command_Line.Argument( 1 ));
      Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 2 ));
      Model.WSC.Global_Settings.Initialise_Logging;
      Put_Line( "Set_Up: opening " & Model.WSC.Global_Settings.Default_Text_Parameter_File_Name );
      params := Model.WSC.Parameters.IO.Read( 
         Model.WSC.Global_Settings.Default_Text_Parameter_File_Name,
         default_wsc_run.Start_Year,
         default_wsc_run.End_Year );
   end Set_Up;

   procedure Shut_Down( T : in out Test_Case) is
   begin
      null;
   end Shut_Down;
   
   procedure Test_Make_Needs_Score_Thresholds( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      proportions : UAP_Array;
      thresholds  : UAP_Array;
      db          : DB_Type;
      weights     : Model.Household.Transitions.Weights_Map; -- DUMMY not really used
   begin
      Open( db, Model.WSC.Global_Settings.Physical_Root & "data/wales/actual/",  actual, 1 );
      Open( db, Model.WSC.Global_Settings.Physical_Root & "data/wales/estimated/default/" ,  estimated, 1 );
      each_wave:
      for wave in Simulation_Waves loop
         Put_Line( "Test_Make_Needs_Score_Thresholds for ======== WAVE " & Waves'Image( wave ));
         proportions := ( 100.0, 0.0, 0.0, 0.0, 0.0 );
         Make_Needs_Score_Thresholds(
            db,
            wave,
            proportions,
            Model.WSC.Static_Calculator.Rec_Care_Probit'Access,
            thresholds,
            weights );
         Put_Line( "100% in top" );
         for i in UAP_Array'Range loop
            Put_Line( " prop[ " & i'Img & " ]=" & Format( proportions( i )) & " thresh " & Format( thresholds( i )));
         end loop;
         proportions := ( 0.0, 0.0, 0.0, 0.0, 100.0 );
         Put_Line( "ALL ZEROS" );
         Make_Needs_Score_Thresholds(
            db,
            wave,
            proportions,
            Model.WSC.Static_Calculator.Rec_Care_Probit'Access,
            thresholds,
            weights );
         for i in UAP_Array'Range loop
            Put_Line( " prop[ " & i'Img & " ]=" & Format( proportions( i )) & " thresh " & Format( thresholds( i )));
         end loop;
         Put_Line( "50 top 50 bottom" );
         proportions := ( 50.0, 0.0, 0.0, 0.0, 50.0 );
         Make_Needs_Score_Thresholds(
            db,
            wave,
            proportions,
            Model.WSC.Static_Calculator.Rec_Care_Probit'Access,
            thresholds,
            weights );
         for i in UAP_Array'Range loop
            Put_Line( " prop[ " & i'Img & " ]=" & Format( proportions( i )) & " thresh " & Format( thresholds( i )));
         end loop;
         
         proportions := ( 5.5, 2.8, 2.3, 10.0, 79.4 ); -- actual defaults
         Put_Line( "Actual Defaults" );
         Make_Needs_Score_Thresholds(
            db,
            wave,
            proportions,
            Model.WSC.Static_Calculator.Rec_Care_Probit'Access,
            thresholds );
         for i in UAP_Array'Range loop
            Put_Line( " prop[ " & i'Img & " ]=" & Format( proportions( i )) & " thresh " & thresholds( i )'Img );
         end loop;
      end loop each_wave;
      
   end Test_Make_Needs_Score_Thresholds;


   --------------------
   -- Register_Tests --
   --------------------
   procedure Register_Tests( t : in out Test_Case ) is
      use AUnit.Test_Cases.Registration;
   begin
      Put_Line( "Static_Tests: register tests" );

      Register_Routine( t, Test_Make_Needs_Score_Thresholds'Access, "Test Make Needs Score Thresholds" );
   end Register_Tests;

   ----------
   -- Name --
   ----------
   function Name ( t : Test_Case ) return Message_String is
   begin
      return Format("Model.WSC.Dynamic_Driver.Tests");
   end Name;

end Model.WSC.Dynamic_Driver.Tests;
