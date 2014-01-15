with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Text_Utils;
with Model.WSC.Users;
with Model.WSC.Users.IO;
with GNAT.MD5;
with Model.WSC.Global_Settings;
with Model.WSC.Globals;
with User_Type_IO;

procedure Create_User is
   
   use Model.WSC.Users;
   use Ada.Strings.Unbounded;
   use Model.WSC.Users.IO;
   use Text_Utils;
   use Ada.Text_IO;
   
   
begin
   if( Ada.Command_Line.Argument_Count < 3 )then
      Put_Line( "usage <global config file> <username> <password> [optional description] [optional title]" );
   else
      Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 1 ));
      declare
         sep : constant String := Model.WSC.Global_Settings.Dir_Separator;
         user : User_Type;
      begin
         Model.WSC.Globals.Initialise_Globals_And_Pools( 1 );
         user.username := TuS( Ada.Command_Line.Argument( 2 ));
         user.password := TuS( Ada.Command_Line.Argument( 3 ));
         -- note: this makes MD5 hash of the password 
         -- TuS( GNAT.MD5.Digest( Ada.Command_Line.Argument( 3 )));
         if( Ada.Command_Line.Argument_Count >= 4 )then
            user.description  := TuS( Ada.Command_Line.Argument( 4 ));
         else
            user.description  := user.username;
         end if;
         if( Ada.Command_Line.Argument_Count >= 5 )then
            user.title  := TuS( Ada.Command_Line.Argument( 5 ));
         else
            user.title := user.description;
         end if;
         User_Type_IO.Save( user );
         Create_User_Files( Model.WSC.Global_Settings.Working_Root, sep, user );
         Model.WSC.Globals.Shutdown_Globals_And_Pools;
      end;
   end if;
end Create_User;

