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

use Model.WSC.Run_Declarations;
use Text_Utils;
use Ada.Strings.Unbounded;
use Ada.Text_IO;



procedure Run_Tester is
   username : Unbounded_String;
   id       : Integer;
   params   : Wsc_Db_Data.Key_Value_Parameter_List.Vector;
   wsc_run  : Run;
   
   use Wsc_Db_Data;
   use Wsc_Db_Data.Key_Value_Parameter_List;
   
   procedure Dump( c : Cursor ) is
      kv : Key_Value_Parameter := Element( c );
   begin
      Put_Line( TS( kv.key ) & " | " & TS( kv.val ));
   end Dump;
   
begin
   Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 1 ));
   Model.WSC.Global_Settings.Initialise_Logging;
   username := TuS( Ada.Command_Line.Argument( 2 ));
   Connection_Pool.Initialise(
         Environment.Get_Server_Name,
         Environment.Get_Username,
         Environment.Get_Password,
         50 );
   id := Run_IO.Highest_Run_Id( username );
   Put_Line( "Highest for user " & TS( username ) & " = " & Integer'Image( id ));
   wsc_run := Run_IO.Get_New_Run_For( username );
   params := Run_IO.Retrieve_Associated_Key_Value_Parameters( wsc_run );
   Iterate( params, Dump'Access );
   Connection_Pool.Shutdown; 
end Run_Tester;
