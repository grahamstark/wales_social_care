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



procedure SQL_Driver is

   use Text_Utils;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Templates_Parser;
   use DB_Commons.ODBC;
   
   username : Unbounded_String;
   id       : Integer;
   template_name : Unbounded_String;
   connection : Database_Connection;
   translations          : Translate_Set;
   sql_commands : Unbounded_String;
begin
   if( Ada.Command_Line.Argument_Count /= 3 )then
      Put_Line( "supply a username, run_id and an SQL template and this prog executes it directly after subsititing the username and run_id" );
      Put_Line( "use: username run_id command_template" );
      return;
   end if;
   
   username := TuS( Ada.Command_Line.Argument( 1 ));
   Connection_Pool.Initialise(
         Environment.Get_Server_Name,
         Environment.Get_Username,
         Environment.Get_Password,
         1 );
   connection := Connection_Pool.Lease;
   id := Positive'Value( Ada.Command_Line.Argument( 2 ));
   template_name := TuS( Ada.Command_Line.Argument( 3 ));
   Put_Line( "Highest for user " & TS( username ) & " = " & Integer'Image( id ));
   Insert( translations, Assoc( "USERNAME", username ));
   Insert( translations, Assoc( "RUN-ID", id ));
   sql_commands := Web_Utils.Parse_Template( template_name, translations ); 
   DB_Commons.ODBC.Execute_Script( connection, TS( sql_commands ));  
   Connection_Pool.Return_Connection( connection );
   Connection_Pool.Shutdown; 
end SQL_Driver;
