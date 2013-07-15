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
with Ada.Calendar;

with Base_Model_Types;
with DB_Commons;
with Model.WSC.Global_Settings;
with Model.WSC.Household.Database;
with Model.WSC.Household.Examples;
with Model.WSC.Household.IO;
with Model.WSC.Household.Regressions;
with Model.WSC.Household;
with Model.WSC.Parameters.IO;
with Model.WSC.Results.IO;
with Model.WSC.Results;
with Model.WSC.Run_Declarations;
with Model.WSC.Uprate;
with Model.WSC.Household.Transitions;
with Model.WSC.Static_Calculator;
with WSC_Enums;


with Connection_Pool;
with Environment;

with Run_IO;

with Maths_Functions;
with Maths_Functions.Simple_Statistics;
with Statistics_Commons;
with Event_Counter;
with Text_Utils;
with T_Utils;

with GNATColl.Traces;

package body Model.WSC.Results.DAO.Tests is
   
   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;
   use Ada.Text_IO;
   use WSC_Enums;
   use Model.WSC.Parameters;
   use Text_Utils;
   use Ada.Calendar;
   
   package wscm renames Model.WSC.Household;
   default_username : constant Unbounded_String := TuS( "test_user1" );
   default_wsc_run : Model.WSC.Run_Declarations.Run;
   params : Parameters_Array;
   rand_list : M_Randoms.Random_List;
   proportions : UAP_Array;
   thresholds  : UAP_Array;
   
   package MF is new Maths_Functions( Rate );
   
   package MFStats is new MF.Simple_Statistics;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.RESULTS.DAO.TESTS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
   procedure Test_Load_Live( t : in out AUnit.Test_Cases.Test_Case'Class ) is

   use Model.WSC.Household.Database;

      wsc_run : Model.WSC.Run_Declarations.Run := Run_IO.Get_Current_Run_For( default_username );
      rid : Positive := Run_IO.Next_Free_Run_Id( wsc_run.username );
      res : Personal_Result;
      pid : Sernum_Value := 54681391;
      hid : Sernum_Value;
      db  : DB_Type;
      hh   : wscm.Household;
      pers : wscm.Person;
      iteration : constant Iteration_Number := 1;
   begin      
      -- where username = 'test_user1/' and run_id = 'run_2011-12-18_13_50_32_run' and sysno =  2 and pid = '54681391' and wave = 'U' 
      Open( db, Model.WSC.Global_Settings.Physical_Root & "data/wales/actual/",  actual, iteration );
      Open( db, Model.WSC.Global_Settings.Physical_Root & "data/wales/estimated/" & wsc_run.dataset_name & "/",  estimated, iteration  );
      for wave in r .. z loop
         res := Get( wsc_run, wave, 1, iteration, pid );
         Put_Line( "Test_Load_Live: looking for pid " & pid'Img & " wave " & wave'Img );
         if( res.sernum /= -9 )then
            Assert( pid = res.sernum, " pid /= res.pid seeking" & pid'Img & " retrieved = " & res.sernum'Img );
            pers := db.Get_Person( 
                     wave => wave, 
                     pid  => pid);
            Put_Line( wscm.To_String( pers ));         
            hh := db.Get_Household( 
                     wave => wave, 
                     hid  => pers.hid );
            Put_Line( "Test_Load_Live; WAVE " & wave'Img & " pid " & pid'Img );
            Put_Line( wscm.To_String( hh ));         
            Put_Line( To_String( res ));
         else
            Put_Line( "MISSING" );
         end if;                  
      end loop;
      Close( db );
   end Test_Load_Live;
   
   procedure Test_Insert_Records( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.WSC.Household.Examples;
      hh : Model.WSC.Household.Household;
      res : Household_Result;
      uap_thresholds : UAP_Array;
      res_file, inc_file : File_Type;
      is_first, is_last : Boolean;
      wave : Waves := s;
      ok : Boolean;
      year : year_Number := Year_From_Wave( wave );
      iteration : constant Iteration_Number := 1;
   begin
      Create( res_file, Out_File, "results.sql" );
      Create( inc_file, Out_File, "incomes.sql" );
      Delete_For_Run( default_wsc_run );
      
      for h in age_uk_indira .. zero_income loop
         Zero( res, clear_historical => True );
         hh := Get_Household( h );
         Model.WSC.Static_Calculator.Calculate_One_Household( 
                     wave, 
                     hh, 
                     params( year ), 
                     res, 
                     default_wsc_run, 
                     thresholds, 
                     rand_list );

         res.res.num_benefit_units := hh.num_benefit_units;
         res.benefit_units( 1 ).res.num_people := hh.benefit_units( 1 ).num_adults;
         
         Set( default_wsc_run, hh, PRE_SYS, iteration, res );
         Put_Line( "MEANS TEST == HHLD " & h'Img );
         Put_Line( To_String( hh.benefit_units( 1 )));
         Put_Line( To_String( res ));
         is_first := h = age_uk_indira;
         is_last := h = zero_income;
         Dump_As_SQL( 
                  res_file, 
                  inc_file, 
                  1, 
                  iteration,
                  default_wsc_run, 
                  is_first, -- 2nd can't be first  
                  False, 
                  hh, 
                  res );
         Dump_As_SQL( 
                  res_file, 
                  inc_file, 
                  2, 
                  iteration,
                  default_wsc_run, 
                  False, -- 2nd can't be first  
                  is_last, 
                  hh, 
                  res );
      end loop;
      for h in age_uk_indira .. zero_income loop
         hh := Get_Household( h );
         res := Get( default_wsc_run, hh, PRE_SYS, iteration );
      end loop;
      Close( inc_file );
      Close( res_file );
      ok := DB_Commons.Load_Postgres( "results.sql", "results.sql.dump" );
   end Test_Insert_Records;
   
   procedure Set_Up( tc : in out Test_Case) is
      run_id          : Positive;
   begin
      Put_Line( "Model.WSC.Static_Calculator.Tests:: SET UP" );
      -- rand_list.Reset;
      rand_list.Load( Model.WSC.Global_Settings.Physical_Root & "data/randoms/randoms_1.txt" );
      default_wsc_run := Model.WSC.Run_Declarations.Read_Settings( Ada.Command_Line.Argument( 1 ));
      Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 2 ));
      Put_Line( "GLOBAL SETTINGS READ OK" );
      Put_Line( "LOG OUTPUT SET" );
      Model.WSC.Global_Settings.Initialise_Logging;
      Put_Line( "LOG TARGETS SET" );
      Put_Line( "Set_Up: opening " & Model.WSC.Global_Settings.Default_Text_Parameter_File_Name );
      params := Model.WSC.Parameters.IO.Read( 
         Model.WSC.Global_Settings.Default_Text_Parameter_File_Name,
         default_wsc_run.Start_Year,
         default_wsc_run.End_Year );
   end Set_Up;
   
   procedure Shut_Down( tc : in out Test_Case) is
   begin
      Connection_Pool.Shutdown;
   end Shut_Down;
   

   

   --------------------
   -- Register_Tests --
   --------------------
   procedure Register_Tests( tc : in out Test_Case ) is
      use AUnit.Test_Cases.Registration;
   begin
      Put_Line( "Static_Tests: register tests" );
      Register_Routine( tc, Test_Insert_Records'Access, "Test_Insert_Records" );
      Register_Routine( tc, Test_Load_Live'Access, "Test Load Live" );
   end Register_Tests;

   ----------
   -- Name --
   ----------
   function Name ( tc : Test_Case ) return Message_String is
   begin
      return Format( "Model.WSC.Results.DAO.Tests" );
   end Name;

begin

   Connection_Pool.Initialise(
         Environment.Get_Server_Name,
         Environment.Get_Username,
         Environment.Get_Password,
         50 );

end Model.WSC.Results.DAO.Tests;
