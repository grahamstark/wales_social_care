--
--  $Author: graham_s $
--  $Date: 2011-04-06 15:56:14 +0100 (Wed, 06 Apr 2011) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Format_Utils;

with Model.WSC.Run_Settings;
with Model.WSC.Global_Settings;
with Model.WSC.Household.Transitions_Basic;
with Model.WSC.Run_Declarations;

with Text_Utils;

with WSC_Enums;


procedure Create_Weights is

   use Ada.Text_IO;
   use Model.WSC.Run_Settings;
   use Model.WSC.Run_Declarations;
   use Model.WSC.Household.Transitions_Basic;
   use WSC_Enums;
   use Ada.Strings.Unbounded;
   use Text_Utils;

   wave     : Waves;
   wsc_run : Run; 
   run_id   : String := Get_New_Run_Id;
   run_dir  : Unbounded_String;
begin
   wsc_run := Model.WSC.Run_Declarations.Read_Settings( Ada.Command_Line.Argument( 1 ));
   Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 2 ));
   wave := Waves'Value( Ada.Command_Line.Argument( 3 ));   
   Model.WSC.Global_Settings.Initialise_Logging;
   Put_Line( "Made run dir as " & TS( run_dir ));
   wsc_run.Set_Users_Directory( TuS( "test_user20" ));
   wsc_run.Set_Run_Id( run_id );
   Create_Weights(
      weights_file_name => "tmp/test_weights",
      from_wave => wave,
      wsc_run => wsc_run ); 
end Create_Weights;
