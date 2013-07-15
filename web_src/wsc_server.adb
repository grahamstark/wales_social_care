--
-- $Revision $
-- $Author $
-- $Date $
--
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with AWS.Config.Set; 
with AWS.Config; 
with AWS.Dispatchers.Callback;
with AWS.Mime;
with AWS.Server.Log;
with AWS.Server;
with AWS.Services.Dispatchers.URI;
with AWS.Services.Page_Server;
with AWS.Services;
with Ada.Command_Line;

with Callbacks.Wales;
with Callbacks;
with Model.WSC.Globals;
with Model.WSC.Parameter_System_Declarations;

with Connection_Pool;

with Model.WSC.Run_Declarations;
with Model.WSC.Global_Settings;
with Model.WSC.Globals;
with GNATColl.Traces;

with Text_Utils;
with Web_Utils;
with Signal_Handler;

procedure WSC_Server is
   
   use AWS.Services;
   use AWS.Config;
   use AWS.Config.Set;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "WSC_SERVER" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;
   
   package globals renames Model.WSC.Globals;

   my_dispatcher : AWS.Services.Dispatchers.URI.Handler;
   my_config     : AWS.Config.Object := AWS.Config.Get_Current;
   
   package awsc renames AWS.Config; 
   
   use Text_Utils;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   
   SEP : constant String := Model.WSC.Global_Settings.Dir_Separator;
    
   default_handler : AWS.Dispatchers.Callback.Handler;

   STATIC_FILE_REGEP : constant String :=  
                                   ".*\.css|" & 
                                   ".*\.js|" &
                                   ".*\.png|" & 
                                   ".*\.html|" & 
                                   ".*\.gif|" &
                                   ".*\.pdf|" &
                                   ".*\.zip";
begin
   if( Ada.Command_Line.Argument_Count = 1 ) then  
      Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 1 ));
   else
      Model.WSC.Global_Settings.Read_Settings( "etc/global_settings.txt" );
   end if;
   Model.WSC.Global_Settings.Initialise_Logging;
   Model.WSC.Globals.Initialise_Globals_And_Pools( connection_pool_size => 30 );
   default_handler := AWS.Dispatchers.Callback.Create( Callbacks.Wales.Index_Page_Callback'Access );
   
   -- add a mime type for SVG
   AWS.Mime.Add_Extension( "svg", globals.MIME_TYPE_IMAGE_SVG );
   
   awsc.Set.Server_Name( my_config, "WSC Server" );
   awsc.Set.Server_Port( my_config, Model.WSC.Global_Settings.port );
   awsc.Set.WWW_Root( my_config, Model.WSC.Global_Settings.Physical_Root & "web" );
   awsc.Set.Session( my_config, true );
   awsc.Set.Session_Lifetime( Duration( 48*60*60 ) ); -- 48 hours
   awsc.Set.Max_Connection(my_config,  100 );
   awsc.Set.Accept_Queue_Size( my_config, 60 );
   awsc.Set.Free_Slots_Keep_Alive_Limit(my_config,  80 );
   declare
      WSC_Web_Root : constant String := Model.WSC.Global_Settings.WSC_Web_Root;
   begin
      Log
         ("Call me on port" &
         Positive'Image( Model.WSC.Global_Settings.port ) & "; serving web root |" & WSC_Web_Root &
          "| press ctl-break to stop me ...");
      Dispatchers.URI.Register_Default_Callback( my_dispatcher, default_handler );
      Dispatchers.URI.Register_Regexp( my_dispatcher, STATIC_FILE_REGEP,
                                      Callbacks.Serve_Static_Resource'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, WSC_Web_Root & "progress.*",      Callbacks.Wales.Run_Progress_Callback'Access );   
       -- Dispatchers.URI.Register_Regexp( my_dispatcher, WSC_Web_Root & "save_file.*",     Callbacks.Serve_File_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, WSC_Web_Root & "logout.*",        Callbacks.Logout_Callback'Access );
      
      -- Dispatchers.URI.Register_Regexp( my_dispatcher, WSC_Web_Root & "download_run",    Callbacks.Wales.Download_Run_Callback'Access );
      
      Dispatchers.URI.Register_Regexp( my_dispatcher, WSC_Web_Root & ".*example_popup.*", Callbacks.Wales.Example_Popup_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, WSC_Web_Root & "/*output_page.*|stats_page.*", Callbacks.Wales.Output_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, WSC_Web_Root & "/*chart_popup.*", Callbacks.Wales.Chart_Popup_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, WSC_Web_Root & "/*dump_params_page.*", Callbacks.Wales.Param_Dump_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, WSC_Web_Root & "/*logout", Callbacks.Logout_Callback'Access );
      
      Dispatchers.URI.Register_Regexp( my_dispatcher, WSC_Web_Root & "/*run_settings_page.*", Callbacks.Wales.Run_Settings_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, WSC_Web_Root & "/*parameters_page.*", Callbacks.Wales.Parameter_Page_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, WSC_Web_Root & "/*array_update.*",  Callbacks.Wales.Array_Update_Callback'Access );
      Dispatchers.URI.Register_Regexp( my_dispatcher, WSC_Web_Root & "/*csv.*",       Callbacks.Wales.CSV_File_Callback'Access );
   end;
   
   AWS.Server.Log.Start( 
      Web_Server => Signal_Handler.aws_web_server,
      Filename_Prefix => "log/request_log",
      Auto_Flush => False  );
   
   Log( "started the logger" );
   AWS.Server.Start( 
      Signal_Handler.aws_web_server,
      Dispatcher => my_dispatcher,
      Config     => my_config );
   Log( "started the server" );
   
    AWS.Server.Wait( AWS.Server.forever );
   Log( "server shutting down" );
end WSC_Server;
