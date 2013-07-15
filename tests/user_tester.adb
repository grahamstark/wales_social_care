--
--  $Author: graham_s $
--  $Date: 2011-04-06 15:56:14 +0100 (Wed, 06 Apr 2011) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Model.WSC.Global_Settings;
with Connection_Pool;
with Environment;

with Connection_Pool;
with Text_Utils;
with WSC_DB_Data;
with User_Type_IO;
use Ada.Strings.Unbounded;
use Text_Utils;
use Ada.Text_IO;


procedure User_Tester is
   username        : Unbounded_String;
   user            : Wsc_Db_Data.User_Type;
begin
   Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 1 ));
   Model.WSC.Global_Settings.Initialise_Logging;
   username := TuS( Ada.Command_Line.Argument( 2 ));
   Connection_Pool.Initialise(
         Environment.Get_Server_Name,
         Environment.Get_Username,
         Environment.Get_Password,
         50 );
   user := User_Type_IO.Retrieve_By_PK( username );
   User_Type_IO.Update_Last_Used( user );
   Put_Line( WSC_DB_Data.To_String( user ));
   Connection_Pool.Shutdown; 
end User_Tester;
