--
--
-- supply a username, run_id and an SQL template and this prog executes 
-- it directly after subsititing the username and run_id" );
--
pragma License( Modified_GPL );

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with DB_Commons.ODBC;
with Connection_Pool;
with Environment;
with Text_Utils;
with Templates_Parser;
with Web_Utils;



procedure SQL_Tester is

   use Text_Utils;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Templates_Parser;
   use DB_Commons.ODBC;
   
   connection   : Database_Connection;
   command      : constant String := Ada.Command_Line.Argument( 1 );
begin
   Put_Line( "at start; command is " & command );
   Connection_Pool.Initialise(
         Environment.Get_Server_Name,
         Environment.Get_Username,
         Environment.Get_Password,
         1 );
         Put_Line( "pool initialised" );
   connection := Connection_Pool.Lease;
   Put_Line( "leased a connection" );
   DB_Commons.ODBC.Execute_Script( connection, command );
   Put_Line( "executed script" );
   Connection_Pool.Return_Connection( connection );
   Put_Line( "connection returned" );
   Connection_Pool.Shutdown; 
   Put_Line( "pool sshut down; returining" );
end SQL_Tester;
