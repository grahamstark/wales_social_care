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

with Model.WSC.Run_Settings;
with Model.WSC.Dynamic_Driver;
with Model.WSC.Global_Settings;
with Model.WSC.Globals;
with Model.WSC.Household;
with Model.WSC.Output;
with Model.WSC.Parameters.DAO;
with Model.WSC.Parameters;
with Model.WSC.Results;
with Model.WSC.Run_Declarations;
with Model.WSC.Uprate;
with Model.WSC.Users.IO;

with Base_Model_Types;
with Connection_Pool;
with Environment;
with Run_IO;
with Table_Stats_IO;
with Text_Utils;
with WSC_DB_Data;
with WSC_Enums;
with GNATColl.Traces;

procedure Dynamic_Model_Runner is

   use Model.UK_Format_Utils;
   use Model.WSC.Run_Settings;
   use Model.WSC.Run_Declarations;
   use Model.WSC.Output;
   use Model.WSC.Parameters;
   use Model.WSC.Household;
   use Model.WSC.Dynamic_Driver;
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use WSC_Enums;
   use Ada.Calendar;
   use Ada.Assertions;
   use Text_Utils;
   use Model.WSC.Results;
   
   username        : Unbounded_String;
   wsc_run         : Run; 
   run_id          : Natural;
   run_dir         : Unbounded_String;
   pre_sys         : Parameters_Array;
   post_sys        : Parameters_Array;
   -- output          : Outputs_Array_Access;
   output_for_year : Outputs_Rec;
   state           : State_Type;
   outf            : File_Type;
   root            : Unbounded_String;
   n_iter          : Positive;
   stats           : WSC_DB_Data.Table_Stats;
   monitor         : aliased Model.WSC.Run_Settings.Model_Monitor;
   text_obs        : Model.WSC.Run_Settings.Model_Observer( monitor'Access );
begin
   if( Ada.Command_Line.Argument_Count /= 3 )then
      Put_Line( "use : global_settings_file username num_iterations" );
      return;
   end if;
   Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 1 ));
   Model.WSC.Global_Settings.Initialise_Logging;
   
   username := TuS( Ada.Command_Line.Argument( 2 ));   
   Model.WSC.Globals.Initialise_Globals_And_Pools( 10 );


   wsc_run := Run_IO.Get_New_Run_For( username );
   wsc_run.num_iterations := Positive'Value( Ada.Command_Line.Argument( 3 ));
   Run_IO.Save( wsc_run );
   pre_sys := Model.WSC.Parameters.DAO.Read( wsc_run, wsc_run.start_year, wsc_run.end_year );
   post_sys := Model.WSC.Parameters.DAO.Read( wsc_run, wsc_run.start_year, wsc_run.end_year );
   
   root := Create_Directories_For_Run( wsc_run );
   Run_Model( 
        wsc_run,
        pre_sys,
        post_sys,
        -- output.all,
        state,        
        monitor );
   for wave in u .. am loop
      for row_num in 1 .. 25 loop
         for sysno in 1 .. 2 loop
            stats := Table_Stats_IO.Retrieve_By_PK( wsc_run.run_id, wsc_run.username, TuS( "summary_items" ), row_num, -999, TuS( wave'Img ), sysno );
            Put_Line( WSC_DB_Data.To_string( stats ));
         end loop;
      end loop;
   end loop; 
   Create( outf, Out_File, "tmp/summary_output_dump.txt" );
   Close( outf );
   Model.WSC.Globals.Shutdown_Globals_And_Pools;
end Dynamic_Model_Runner;
