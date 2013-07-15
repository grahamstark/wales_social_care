with AWS.Session;
with AWS.Log;
with Ada.Strings.Unbounded;
with Ada.Calendar;
with HTML_Utils;

with Breadcrumbs;
with Model.WSC.Run_Declarations;
with Model.WSC.Parameters;
with Model.WSC.Parameter_System_Declarations;
with Model.WSC.Output;
with Model.WSC.Users;
with Model.Run_Settings;
-- with Model.WSC.Run_Results;
with Model.WSC.Results;
with Parameter_System_IO_Commons;
with WSC_Enums;

package Model.WSC.Globals is
   
   use Ada.Strings.Unbounded;
   use Ada.Calendar;
   use WSC_Enums;
   use Parameter_System_IO_Commons;

   package runsett renames Model.WSC.Run_Declarations;
   package wsc_params renames Model.WSC.Parameter_System_Declarations;
   
   type Apply_Actions is (   
      this_year_only,
      this_and_subsequent_years,
      all_years );

   type Reset_Actions is ( 
      this_year_these_items_only,
      this_year_all_items,
      all_years_these_items_only,
      all_years_all_items );
   
    type Uprate_Actions is (   
      this_year_these_items_only,
      this_year_all_items,
      this_and_subsequent_years_these_items_only,
      this_and_subsequent_years_all_items );
      

   
   DELIMITER : constant Character := '.';
   
   DEFAULT_USER         : constant String := "default";
   DEFAULT_RUN          : constant String := "default";
   
   DEFAULT_PARAMETERS   : constant String := "wsc.prm"; -- TODO NOT NEEDED
   
   -- mime type for svg see: /etc/mime.types, and AWS.Mime.ads
   MIME_TYPE_IMAGE_SVG : constant String := "image/svg+xml";
   MIME_TYPE_TEXT_CSV  : constant String := "text/comma-separated-values";

   --
   -- constants for identfiying stuff bound to a session
   --
   SESSION_PARAMETER_EDITING_BUFFER    : constant String := "session-param-buffer";
   SESSION_RUN_SETTINGS_BUFFER : constant String := "session-run-settings-buffer";
   
   SESSION_USER_ID             : constant String := "session-user-id";
   -- SESSION_RUN_ID              : constant String := "session-run-id";
   SESSION_ABORTING            : constant String := "session-aborting";
   SESSION_BREADCRUMBS         : constant String := "session-breadcrumbs";
   SESSION_RUN_JUST_ENDED      : constant String := "session-run-just-ended";
   
   AUTHENTICATION_DOMAIN    : constant String := "WSC/WSC Models";

   package Breadcrumbs_Session_Package is new AWS.Session.Generic_Data(
      Breadcrumbs.Trail_Type,
      Breadcrumbs.Empty_Trail );
   package Buffer_Session_Package is new AWS.Session.Generic_Data(
      Model.WSC.Parameter_System_Declarations.WSC_Editing_Buffer,
      Model.WSC.Parameter_System_Declarations.WSC_Parameter_Editing_System_IO.Get_Null_Buffer );
      
   package WSC_HTML_Utils is new HTML_Utils(
      Rate         => Rate, 
      Counter_Type => Counter_Type
   );
   
   procedure Initialise_Globals_And_Pools( connection_pool_size : Positive );
   procedure Shutdown_Globals_And_Pools;
   
   function Get_Default_Run return runsett.Run;

   function Validate( 
      username : Unbounded_String; 
      password : Unbounded_String ) return Model.WSC.Users.User_Type;

   procedure Map_And_Set_Parameters( 
      wsc_run              : runsett.Run;
      param_editing_buffer : in out wsc_params.WSC_Editing_Buffer;
      current_year         : Year_Number; 
      which                : Buffer_Retrieval_Type; 
      start_year           : Year_Number; 
      end_year             : Year_Number );

   procedure Reset( 
      wsc_run              : runsett.Run;
      target_subsystem     : wsc_params.WSC_Parameter_Editing_System.Parameter_System_Rec;
      path_str             : Unbounded_String;
      default_system       : wsc_params.WSC_Parameter_Editing_System.Parameter_System_Rec;
      param_editing_buffer : in out wsc_params.WSC_Editing_Buffer;
      action               : Reset_Actions; 
      year                 : Year_Number );
      
   procedure Uprate( 
      wsc_run              : runsett.Run;
      param_editing_buffer : in out wsc_params.WSC_Editing_Buffer;
      path_str             : Unbounded_String; 
      action               : Uprate_Actions; 
      index                : Which_Uprate_Index; 
      year                 : Year_Number; 
      uprate_amount        : Rate );   
end Model.WSC.Globals;
