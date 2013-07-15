--
--  $Author: graham_s $
--  $Date: 2011-04-06 15:56:14 +0100 (Wed, 06 Apr 2011) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Calendar;

with Gnat.Command_Line;

with Base_Model_Types;
with Connection_Pool;
with Environment;
with Run_IO;
with Text_Utils;

with Model.WSC.Global_Settings;
with Model.WSC.Globals;
with Model.WSC.Run_Declarations;
with Model.WSC.Parameter_System_Declarations;

with WSC_DB_Data;
with GNATColl.Traces;
with WSC_Enums;

use Ada.Calendar;
use Ada.Strings.Unbounded;
use Ada.Text_IO;
use Base_Model_Types;
use Model.WSC.Globals;
use Model.WSC.Run_Declarations;
use Text_Utils;
use WSC_Enums;

procedure Parameter_Uprater is
   username             : Unbounded_String;
   path                 : Unbounded_String;
   wsc_run              : Run;
   use_obr              : Boolean;
   run_id               : Positive;
   uprate_action        : Uprate_Actions;
   uprate_index         : Which_Uprate_Index;
   uprate_amount        : Rate;
   year                 : Year_Number;
   param_editing_buffer : wsc_params.WSC_Editing_Buffer;
begin
   if( Ada.Command_Line.Argument_Count /= 9 )then
      Put_Line( "usage: global_settings_file username run_id path level_change use_obr(True|False) uprate_index uprate_action year" );
      --                    1                   2        3    4       5           6                   7          8       9
      Put_Line( "uprate index: one of:  none, rpi, rpix, earnings, cpi, gdp" );
      Put_Line( "uprate_action: one of:  this_year_these_items_only, this_year_all_items, this_and_subsequent_years_these_items_only, this_and_subsequent_years_all_items " );
      New_Line;
      return;
   end if;
   Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 1 ));
   username := TuS( Ada.Command_Line.Argument( 2 ));
   Put_Line( "username " & TS( username ));
   run_id := Positive'Value( Ada.Command_Line.Argument( 3 ));
   path := TuS( Ada.Command_Line.Argument( 4 ));
   Put_Line( "path " & TS( path ));
   uprate_amount := Rate'Value( Ada.Command_Line.Argument( 5 ));   
   use_obr := Boolean'Value( Ada.Command_Line.Argument( 6 ));
   if( use_obr )then
      uprate_index := Which_Uprate_Index'Value( "uprate_" & Ada.Command_Line.Argument( 7 ));
   end if;
   uprate_action := Uprate_Actions'Value( Ada.Command_Line.Argument( 8 ));
   year := Year_Number'Value( Ada.Command_Line.Argument( 9 ));
   Put_Line( "got year " & Year_Number'Image( year ));
   Put_Line( "uprate_amount " & Amount'Image( uprate_amount ));
   Put_Line( "uprate_action " & Uprate_Actions'Image( uprate_action ));
   Put_Line( "use_obr " & Boolean'Image( use_obr ));
   
   Model.WSC.Globals.Initialise_Globals_And_Pools( 2 );
   wsc_run := Run_IO.Retrieve_By_PK( run_id, username );
   Put_Line( "got run; uprating" );
   param_editing_buffer := Model.WSC.Parameter_System_Declarations.Get_Loaded_Input_Buffer( wsc_run );
   Model.WSC.Globals.Uprate( wsc_run, param_editing_buffer, path, uprate_action, uprate_index, year, uprate_amount );
   Connection_Pool.Shutdown; 
end Parameter_Uprater;
