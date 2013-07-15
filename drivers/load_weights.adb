--
--  $Author: graham_s $
--  $Date: 2011-04-06 15:56:14 +0100 (Wed, 06 Apr 2011) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );


with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with WSC_Enums;

with Model.WSC.Household.Transitions_Basic;
with Format_Utils;
with Model.Run_Settings;
with Model.WSC.Run_Declarations;
with Model.WSC.Global_Settings;
with Text_Utils;
with Base_Model_Types;
with Model.WSC.Calculator;

procedure Load_Weights is
   use Base_Model_Types;
   use Ada.Text_IO;
   use Model.Run_Settings;
   use Model.WSC.Run_Declarations;
   use Model.WSC.Household.Transitions_Basic;
   use WSC_Enums;
   use Ada.Strings.Unbounded;
   use Text_Utils;

   weights  : All_R_Weights_Array;
   wave     : Waves := r;
   weight   : Amount;
begin
   Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 2 ));
   wave := Waves'Value( Ada.Command_Line.Argument( 3 ));   
   Model.WSC.Global_Settings.Initialise_Logging;
   Load_R_Weights( "data/wales/weights_from_wave_r.txt", weights );
   Put_Line( "Weights Loaded" );
end Load_Weights;
