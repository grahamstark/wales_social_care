--
--  $Author: graham_s $
--  $Date: 2011-04-06 15:56:14 +0100 (Wed, 06 Apr 2011) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Connection_Pool;
with Environment;
with Model.WSC.Global_Settings;
with Run_IO;
with Text_Utils;
with WSC_DB_Data;

with Model.WSC.Run_Declarations;
with Model.WSC.Global_Settings;
with Ada.Exceptions;

use Model.WSC.Run_Declarations;
use Text_Utils;
use Ada.Strings.Unbounded;
use Ada.Text_IO;
use Ada.Exceptions;


procedure Run_Creator is
   id        : Integer;
   wsc_run   : Run;
   username  : Unbounded_String;
   title     : Unbounded_String;
   start_dir : Unbounded_String;
begin
   if( Ada.Command_Line.Argument_Count /= 3 )then
      Put_Line( "create a new run as a copy of the last run; use: global_settings username 'some new title' " );
      return;
   end if;   
   Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 1 ));
   Model.WSC.Global_Settings.Initialise_Logging;
   
   username := TuS( Ada.Command_Line.Argument( 2 ));
   title := TuS( Ada.Command_Line.Argument( 3 ));
   Connection_Pool.Initialise(
         Environment.Get_Server_Name,
         Environment.Get_Username,
         Environment.Get_Password,
         50 );
   id := Run_IO.Highest_Run_Id( username );
   Put_Line( "Highest for user " & TS( username ) & " = " & Integer'Image( id ));
   wsc_run := Run_IO.Get_New_Run_For( username );
   wsc_run.title := title;
   start_dir :=  wsc_run.Create_Directories_For_Run;
   Put_Line( "created directories as " & TS( start_dir ));
   Run_IO.Save( wsc_run );
   Connection_Pool.Shutdown; 
end Run_Creator;
