------------------------------------------------------------------------------
--                                                                          --
--  Handlers for each of OSCR's callbacks, plus some support functions      --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
--                                                                          --

--
--
-- pragma Elaborate_All( EU.BE.Model.Runner.Web );
-- tmp tmp tmp
-- wiTh highest_previous_personal_income_io;
-- wiTh maxima_and_totals_io;
-- with db_user_io;
with AWS.Config;
with AWS.Messages;
with AWS.Mime;
with AWS.Resources;
with AWS.Resources;
with AWS.Response.Set;
with AWS.Response;
with AWS.Server;

with Ada.Containers;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Text_IO;

with GNAT.Regexp;

with Base_Model_Types;
with Disaggregated_Data_Table_Cell_description_io;
with Disaggregated_Data_Table_Cell_io;
with Disaggregated_Data_Table_Description_io;
with Disaggregated_Data_Table_Io;
with Html_Utils;
with Key_Value_DAO;
with Key_Value_Parameter_IO;
with Keyed_Text_Buffer;
with Model.Run_Settings;
with Model.WSC.Dynamic_Driver.Web_Runner;
with Model.WSC.Parameters.DAO;

with Parameter_System.Input_Buffer;
with Parameter_System;
with Parameter_System_IO_Commons;
with Parameter_System_IO_Commons;

with Personal_Income_io;
with Personal_results_io;
with Probit_threshold_io;
with Run_IO;

with State_io;
with T_Utils.Web_IO;
with Table_stats_io;
with Tabulator_Commons;
with Uprate_assumption_io;
with User_Type_IO;
with Utils;
with WSC_DB_Data;
with WSC_Enums;
with WSC_Web_Enums;
with Web_Utils;
with Weighting_Commons;
with GNATColl.Traces;
with Text_Utils;

package body Callbacks is

   use type Parameter_System_IO_Commons.Buffer_Retrieval_Type;
   
   use Ada.Text_IO;
   use WSC_Web_Enums;
   use Text_Utils;
   use Ada.Strings.Unbounded;
   use Model.WSC.Parameter_System_Declarations;
   use Model.WSC.Run_Declarations;
   use Model.Run_Settings;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "CALLBACKS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   procedure Map_Buffer_To_Settings( rs_buffer : wsc_params.WSC_Editing_Buffer; wsc_run : in out runsett.Run ) is
      use Model.WSC.Run_Declarations;
      use Weighting_Commons;
      use Parameter_System_IO_Commons;
      use WSC_Enums;
      kvb : Keyed_Text_Buffer.Text_Buffer;
   begin
      kvb := rs_buffer.To_Text_Buffer( get_all );
      Log( "kvb = " & To_String( kvb ));
      Model.WSC.Run_Declarations.Update_From_Text_Buffer( wsc_run, kvb );
      Run_IO.Save( wsc_run );      
   end Map_Buffer_To_Settings;
   
   function Serve_Static_Resource( Request : in AWS.Status.Data ) return AWS.Response.Data is
      use Ada.Strings;
      WWW_Root : constant String := AWS.Config.WWW_Root( AWS.Server.Config( AWS.Server.Get_Current.all ));
      URI      : constant String := AWS.Status.URI( Request );
      root     : constant String := Model.WSC.Global_Settings.WSC_Web_Root;      
      filename : constant String := WWW_Root & "wsc" & URI ( root'Last .. URI'Last );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
     -- filename :  constant String := WWW_Root & root & URI;
   begin
      Log(  "serving |" & filename & "| URI was |" & URI & "| root |" & root & "| WWW_Root |" & WWW_Root & "|" );
      if AWS.Resources.Is_Regular_File( filename ) then
         return AWS.Response.File( 
            Content_Type => AWS.MIME.Content_Type( filename ),
            Filename     => filename );
      else          
         return AWS.Response.Acknowledge
              (AWS.Messages.S404,
               "<p>The page '"
               --  Replace HTML control characters to the HTML inactive symbols
               --  to avoid correct HTML pages initiated from the client side.
               --  See http://www.securityfocus.com/bid/7596
               & Fixed.Translate (URI, Maps.To_Mapping ("<>&", "{}@"))
               & "' was not found.");
      end if;         
   end Serve_Static_Resource;
   
   function Handle_Login_With_Authentication( request : in AWS.Status.Data ) return users.Login_Result is
      use type users.User_Type;

      username   : constant Unbounded_String := TuS( AWS.Status.Authorization_Name( request ));
      password   : constant Unbounded_String := TuS( AWS.Status.Authorization_Password( request ));
      session_id : constant AWS.Session.Id := AWS.Status.Session( request );
      this_user  : users.User_Type;
      result     : users.Login_Result;
      root       : constant String := Model.WSC.Global_Settings.WSC_Web_Root;
      dir        : Unbounded_String;
   begin
      if( username = TuS( "" ))then
          result.response := AWS.Response.Authenticate( globals.AUTHENTICATION_DOMAIN, AWS.Response.Basic );
      end if;
      this_user := globals.Validate( username, password );
      if( this_user = users.INVALID_USER )then
          users.User_Session_Data.Set( session_id, globals.SESSION_USER_ID, Null_Unbounded_String );
          result.response := AWS.Response.Authenticate( globals.AUTHENTICATION_DOMAIN, AWS.Response.Basic );
      else
         result.user := this_user;
         result.validated := True;
         Log( "validated user " & To_String( username ));
         if( users.User_Session_Data.Get( session_id, globals.SESSION_USER_ID ) = Null_Unbounded_String )then
            Log( "creating session for user " & To_String( username ));
            --
            --  no user in the session; put the user record in the session
            --
            users.User_Session_Data.Set( session_id, globals.SESSION_USER_ID, username );
            users.User_Session_Data.Set( session_id, globals.SESSION_USER_ID, result.user.username );
            Log( " created session data for user OK" );
            result.new_session := True;
            result.response := AWS.Response.URL( Location => root );
         end if;
      end if;
      return result;
   end Handle_Login_With_Authentication;
   
   function Is_Job_Running( request : in AWS.Status.Data ) return Boolean is
   use runsett;
      session_id     : constant AWS.Session.Id := AWS.Status.Session( request );
      username       : Unbounded_String := users.User_Session_Data.Get( session_id, globals.SESSION_USER_ID );
   begin
      return Run_IO.Has_Run_In_State( username, running_or_queued );
   end Is_Job_Running;
   
   function Get_Std_Translations( request : in AWS.Status.Data; user : users.User_Type ) return Templates_Parser.Translate_Set is
   use Templates_Parser;
   use Model.WSC.Formatting;
      URI    : constant String := AWS.Status.URI( Request );
      translations : Translate_Set;
      description : Unbounded_String := ( if user.description = Null_Unbounded_String then user.username else user.description );
   begin
      Insert( translations, Assoc( "LANG", Model.WSC.Formatting.Lang_Str( user.lang )));
      Insert( translations, Assoc( "TEMPLATE_ROOT",  Model.WSC.Global_Settings.template_components_path ));
      Insert( translations, Assoc( "SEP", Model.WSC.Global_Settings.Dir_Separator ));
      Insert( translations, Assoc( "ROOT", Model.WSC.Global_Settings.WSC_Web_Root ));
      Insert( translations, Assoc( "USERNAME", user.username ));
      Insert( translations, Assoc( "USER-DESCRIPTION", description ));
      
      Insert( translations, Assoc( "RANDOM_STRING", Utils.Random_String ));
      Insert( translations, Assoc( "URI", URI ));
      Insert( translations, Assoc( "PAGE-TITLE", "" ));
      return translations;
   end Get_Std_Translations;

 
   function Logout_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      session_id : constant AWS.Session.Id := AWS.Status.Session( request );
      d : AWS.Response.Data;
   begin
      if( users.User_Session_Data.Get( session_id, globals.SESSION_USER_ID ) = Null_Unbounded_String )then
         --
         -- already logged out - to homepage 
         -- stop us going round in circles
         --
         return AWS.Response.URL( Location => Model.WSC.Global_Settings.WSC_Web_Root);
      end if;
      AWS.Response.Set.Clear_Session( d );
      AWS.Session.Delete( session_id );
      return AWS.Response.Authenticate( globals.AUTHENTICATION_DOMAIN ); 
      --
   end Logout_Callback;

   function Popup_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use Model.WSC.Users;
      logresult        : constant users.Login_Result := Handle_Login_With_Authentication( request );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
      translations : Translate_Set;
      params       : constant AWS.Parameters.List := AWS.Status.Parameters( Request );
      root         : constant String := Model.WSC.Global_Settings.WSC_Web_Root;
   begin
      --
      -- fixme: this is overkill
      --
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      translations := Get_Std_Translations( request, logresult.user );
      return Web_Utils.Build_Input_Page(
         Model.WSC.Global_Settings.template_components_path & "popup",
         translations );

   end Popup_Callback;
   
   function Run_Settings_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      logresult        : users.Login_Result := Handle_Login_With_Authentication( request );
   use Templates_Parser;  
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
      URI                : constant String := AWS.Status.URI( Request );
      params : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      translations : Translate_Set;
      path               : Unbounded_String_List := Split( URI, '/' );
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      -- Handle_Language( path, logresult.user, session_id );
      translations := Get_Std_Translations( request, logresult.user );
      return Web_Utils.Build_Input_Page(
         Model.WSC.Global_Settings.template_components_path & "input_run_settings",
         translations );
   end Run_Settings_Callback;

 
end Callbacks;
