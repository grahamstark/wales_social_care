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
with WSC_Emailer;
with Model.WSC.Users;
with Model.WSC.Run_Declarations;
with GNATColl.Traces;
with Text_Utils;

procedure Run_Emailer is

use Text_Utils;
use Ada.Strings.Unbounded;
use Ada.Text_IO;
use Model.WSC.Users;
use Model.WSC.Run_Declarations;

   run_id    : Integer;
   wsc_run   : Run;
   username  : Unbounded_String;
   title     : Unbounded_String;
   start_dir : Unbounded_String;
   res       : WSC_Emailer.Email_Result;
begin
   if( Ada.Command_Line.Argument_Count /= 3 )then
      Put_Line( "test email of run end; use: global_settings username run_id " );
      return;
   end if;   
   Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 1 ));
   
   -- initialise Logger
   
   username := TuS( Ada.Command_Line.Argument( 2 ));
   Connection_Pool.Initialise(
         Environment.Get_Server_Name,
         Environment.Get_Username,
         Environment.Get_Password,
         5 );
   run_id := Integer'Value( Ada.Command_Line.Argument( 3 ));
   wsc_run := Run_IO.Retrieve_By_PK( run_id, username );
   res := WSC_Emailer.Send_Run_End_Email( wsc_run );
   Put_Line( "email result " & res'Img );
   Connection_Pool.Shutdown; 
end Run_Emailer;
