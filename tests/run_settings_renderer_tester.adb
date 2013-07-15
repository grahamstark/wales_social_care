--
--  $Author: graham_s $
--  $Date: 2011-04-06 15:56:14 +0100 (Wed, 06 Apr 2011) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );


with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Templates_Parser;


with Model.WSC.Global_Settings;
with Model.WSC.Globals;
with Model.WSC.Parameter_System_Declarations;
with Model.WSC.Run_Declarations;
with Model.WSC.Run_Declarations;
with Model.WSC.Users;

with Connection_Pool;
with Environment;
with Run_IO;
with Text_Utils;
with User_Type_IO;
with WSC_DB_Data;

with Web_Utils;
with WSC_Enums;

procedure Run_Settings_Renderer_Tester is

   use Templates_Parser;
   use Model.WSC.Run_Declarations;
   use Text_Utils;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Model.WSC.Users;
   use WSC_Enums;
   
   package globals               renames Model.WSC.Globals;
   package wsc_params            renames Model.WSC.Parameter_System_Declarations;
   package runsett               renames Model.WSC.Run_Declarations;
      
   -- procedure Dump( c : Cursor ) is
      -- kv : Key_Value_Parameter := Element( c );
   -- begin
      -- Put_Line( TS( kv.key ) & " | " & TS( kv.val ));
   -- end Dump;
   
   username           : Unbounded_String;
   id                 : Integer;
   wsc_run            : Run;
   user               : User_Type;
   rs_buffer          : wsc_params.WSC_Editing_Buffer;
   extra_translations : Translate_Set;
   full_translations  : Translate_Set;
   error_message      : Unbounded_String;
   settings_sys       : wsc_params.WSC_Parameter_Editing_System.Parameter_System_Rec;
   breadcrumb         : constant Unbounded_String := TuS( "BC" );
   menu               : constant Unbounded_String := TuS( "MENU" );
   path_str           : constant Unbounded_String := TuS( "wsc_run" );
   title              : constant Unbounded_String := TuS( "TITLE" );
   html_cells         : Vector_Tag;
   dump_page          : Unbounded_String;
   f                  : File_Type;
begin
   Create( f, Out_File, "tmp/run_settings_renderer_tester.dump" );
   Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 1 ));
   Model.WSC.Global_Settings.Initialise_Logging;
   
   username := TuS( Ada.Command_Line.Argument( 2 ));
   
   user := User_Type_IO.Retrieve_By_PK( username ); 
   
   settings_sys := wsc_params.Get_WSC_Run_Settings;
   id := Run_IO.Highest_Run_Id( username );
   Put_Line( "Highest for user " & TS( username ) & " = " & Integer'Image( id ));
   wsc_run := Run_IO.Get_New_Run_For( username );
   for t in Uprate_Targets loop
      wsc_run.uprate_assumptions( t ).element := cpi;
   end loop;
   Run_IO.Save( wsc_run );
   rs_buffer := Model.WSC.Parameter_System_Declarations.Get_Loaded_Run_Settings_Buffer( wsc_run );
   wsc_params.WSC_Renderer.Create_HTML_Inputs( html_cells, rs_buffer, settings_sys, settings_sys, path_str );
   Insert( extra_translations, Assoc( "HTML-CELLS", html_cells ));
   dump_page := Web_Utils.Parse_Template( TuS( "web/wsc/templates/dump_page" ), extra_translations );  
   
   full_translations := wsc_params.WSC_Renderer.Create_Complete_Translations( 
      title               => title,
      buff                => rs_buffer,
      breadcrumb          => breadcrumb,
      model_menu          => menu,
      base_sys            => settings_sys, 
      sys                 => settings_sys, 
      parameter_prefix    => path_str,
      main_error_message  => error_message,
      job_is_running      => False,
      user                => user,
      extra_translations  => extra_translations );
   
   Put_Line( f, ts( dump_page ));
   Close( f );
   Connection_Pool.Shutdown; 
end Run_Settings_Renderer_Tester;
