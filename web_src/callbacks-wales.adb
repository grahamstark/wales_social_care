------------------------------------------------------------------------------
--                                                                          --
--  Handlers for each of WSCS's main callbacks, plus some support functions --
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

with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Text_IO;
with Ada.Containers;
with GNAT.Regexp;

with AWS.Config;
with AWS.Messages;
with AWS.Mime;
with AWS.Resources;
with AWS.Resources;
with AWS.Response.Set;
with AWS.Response;
with AWS.Server;

with Base_Model_Types;
with Html_Utils;
with Keyed_Text_Buffer;
with Parameter_System;
with T_Utils.Web_IO;
with Tabulator_Commons;
with Templates_Parser;
with Text_Utils;
with Utils;
with Web_Utils;
with Parameter_System_IO_Commons;
with General_Chart_Constants;
with Time_Series_Chart_Generator;

with Model.Run_Settings;
-- with Model.WSC.Datasets;
with Model.WSC.Dynamic_Driver.Web_Runner;
with Model.WSC.Formatting;
with Model.WSC.Global_Settings;
with Model.WSC.Globals;
with Model.WSC.Household.Web_IO;
with Model.WSC.Household.Database;
with Model.WSC.Household.Complete_Web_IO; 
with Model.WSC.Household;
with Model.WSC.Main_Menu;
with Model.WSC.Output.Web_IO;
with Model.WSC.Output;
with Model.WSC.Output.DAO;
with Model.WSC.Uprate;
with Model.WSC.Parameter_System_Declarations;
with Model.WSC.Parameters.IO;
with Model.WSC.Parameters.DAO;
with Model.WSC.Parameters;
with Model.WSC.Results.Web_IO;
with Model.WSC.Results;
with Model.WSC.Run_Declarations;
with Model.WSC.Users.IO;
with Model.WSC.Users;

with Breadcrumbs;
with WSC_Enums;
with WSC_Web_Enums;
with Colours;
with Standard_Colours;
with GNATColl.Traces;
with WSC_Emailer;
with Model.WSC.Results.DAO;
with Key_Value_Parameter_IO;
with Run_IO;
with DB_Commons;
with State_IO;
with Dataset_IO;
with Gain_Lose_IO;
with WSC_DB_Data;
-- pragma Elaborate_All( Model.WSC.Dynamic_Driver.Web_Runner );

package body Callbacks.Wales is

   use Parameter_System_IO_Commons;
   use Breadcrumbs;
   use Ada.Text_IO;
   use Text_Utils;
   use Ada.Calendar;
   use Ada.Strings.Unbounded;
   use WSC_Enums;
   use WSC_Web_Enums;
   use Model.Run_Settings;
   use type Ada.Containers.Count_Type;
   use Model.WSC.Run_Declarations;

   package dao renames Model.WSC.Results.DAO;
    
   function Get_Help( request : in AWS.Status.Data ) return AWS.Response.Data is
      html : Unbounded_String;
   begin     
      return AWS.Response.Build( "text/html", html );   
   end Get_Help;
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "CALLBACKS.WALES" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;
   
   function Dont_Show_Output( request : in AWS.Status.Data; user : users.User_Type ) return Boolean is
      use Model.WSC.Users.IO;
      use Model.WSC.Dynamic_Driver;
      job_is_running  : constant Boolean := Is_Job_Running( request );
      logresult       : users.Login_Result := Handle_Login_With_Authentication( request );
      session_id      : constant AWS.Session.Id := AWS.Status.Session( request );
      wsc_run         : runsett.Run;
      SEP             : constant String := euws.Dir_Separator;
      -- prev_runs       : constant Run_Results_List := Get_And_Save_Runs_List( request );
      -- FIXME the above is VERY inefficient - 
      -- better to check the output in the session and the saved runlist, if any
      -- Previous_Runs_Session.Get( session_id, globals.SESSION_PREV_RUNS );
      run_count       : constant Natural := 1; -- TODO Natural( prev_runs.Length );
   begin
      wsc_run := Run_IO.Get_Current_Run_For( logresult.user.username, displayed );
      return ( wsc_run = NULL_RUN ); 
   end Dont_Show_Output;
 
   function Run_Progress_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use runsett;
   use Model.WSC.Dynamic_Driver;
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
      state_string     : Unbounded_String := Null_Unbounded_String;
      logresult        : users.Login_Result := Handle_Login_With_Authentication( request );
      run_state        : State_Type;  
      wsc_run          : Run;
      run_just_ended   : Boolean := AWS.Session.Get( session_id, globals.SESSION_RUN_JUST_ENDED );
      run_just_aborted : Boolean := AWS.Session.Get( session_id, globals.SESSION_ABORTING );
      root_str         : constant String := Model.WSC.Global_Settings.WSC_Web_Root;
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      if run_just_ended then
         Log( "Run_Progress_Callback; run_just_ended" );
         AWS.Session.Set( session_id, globals.SESSION_RUN_JUST_ENDED, False );        
         state_string := state_string & "<script>" & LINE_BREAK; 
         state_string := state_string & "      updater.stop();"  & LINE_BREAK; 
         state_string := state_string & "      $( 'run_submit_button' ).disabled=false;" & LINE_BREAK;
         -- re-enable the output link
         -- see: http://stackoverflow.com/questions/893208/changing-the-onclick-event-delegate-of-an-html-button
         -- commented out since (a) something was slowing down the js at the point of completion and 
         -- (b) we don't really need it as we're redirecting to the output page anyway, which has enabled links.
         -- state_string := state_string & "      $( 'output_page_link' ).stopObserving( 'click' );" & LINE_BREAK;
         state_string := state_string & "      location.replace('" & root_str & "output_page/');" & LINE_BREAK;
         state_string := state_string & "</script>" & LINE_BREAK;
      elsif run_just_aborted then
         Log( "Run_Progress_Callback; run_just_aborted" );
         state_string := state_string & "<script>" & LINE_BREAK; 
         state_string := state_string & "      location.replace('" & root_str & "/');" & LINE_BREAK;
         state_string := state_string & "      updater.stop();"  & LINE_BREAK; 
         state_string := state_string & "      $( 'run_submit_button' ).disabled=false;" & LINE_BREAK;
         state_string := state_string & "</script>" & LINE_BREAK;
      else
         wsc_run := Run_IO.Get_Current_Run_For( logresult.user.username, running_or_queued );
         if( wsc_run /= NULL_RUN )then
            Log( "Run_Progress_Callback: got run with run id " & wsc_run.run_id'Img );
            run_state := Run_IO.Retrieve_State_For_Run( wsc_run );
            state_string := web_runner.Get_State_Of_Run_As_HTML( wsc_run, run_state );
         end if;
      end if;
      return AWS.Response.Build( "text/html", state_string );
   end Run_Progress_Callback;
   
   function Index_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use Model.WSC.Main_Menu;
   use Model.WSC.Formatting.Translations;

      type Actions is ( edit, copy, delete, display, no_action );
      
      procedure Fill_Out_Run_Lists( translations : in out Translate_Set; username : Unbounded_String ) is
         run_names         : Vector_Tag;
         run_numbers       : Vector_Tag;
         run_descripts     : Vector_Tag;
         run_times         : Vector_Tag;
         is_owners_run     : Vector_Tag;
         is_edited_run     : Vector_Tag;
         is_copyable       : Vector_Tag;
         is_completed_run  : Vector_Tag;
         is_displayed_run  : Vector_Tag;
         is_running        : Vector_Tag;
         base_runs         : Run_List.Vector := Run_IO.Get_Canditate_Base_Runs_For( username );
         n                 : Natural := Natural( base_runs.Length );
         num_owned_runs    : Natural := 0;
      begin
         Run_IO.Set_Highest_Run_As_Active_If_None_Active( username ); -- make sure something is tagged as edited if
                                                       -- nothing currently is but there is a candidate
         for i in 1 .. n loop
            declare
               one_run : Run := base_runs.Element( i );
               id  : Integer := one_run.run_id;
               title : constant String := TS( one_run.Make_Title( as_html => False ));
               is_default : Boolean;
               is_owners  : Boolean := True;
               state      : State_Type := BLANK_STATE_TYPE;
            begin
               is_default := False;
               --
               -- we only have the comparison id to change, and we want both
               -- this user's old runs and the default runs to be choices, 
               -- so we'll add 1,000,000 to default run ids
               -- FIXME we actuall want to add comparison-user to the run
               -- FIXME actually we shouldn't have comparison runs except in the output page as in MEMSA
               
               if( one_run.username = Model.WSC.Users.DEFAULT_USERNAME )then
                  id := 1_000_000 + id ;
                  is_owners := False;
                  is_copyable := is_copyable & True;
               else
                  if( one_run.status = edited )then
                     is_default := True;
                  end if;
                  num_owned_runs := num_owned_runs + 1;
                  state := State_IO.Retrieve_By_PK( username, one_run.run_id );
                  --
                  -- copy completed runs, editing other completed runs
                  --
                  if( state.phase = complete )then
                     is_copyable := is_copyable & True;
                  else
                     is_copyable := is_copyable & False;
                  end if;
               end if;
               is_completed_run := is_completed_run & ( state.phase = complete );
               run_numbers := run_numbers & id;
               run_names := run_names & one_run.title;
               run_times := run_times & "";
               run_descripts := run_descripts & title;
               is_edited_run := is_edited_run & is_default;
               is_displayed_run := is_displayed_run & ( one_run.status = displayed );
               is_owners_run := is_owners_run & is_owners;
               is_running := is_running & ( one_run.status = running_or_queued );
            end;
         end loop;
         Insert( translations, Assoc( "IS-RUNNING", is_running ));
         Insert( translations, Assoc( "IS-COPYABLE", is_copyable ));
         Insert( translations, Assoc( "ALLOW-DELETE", num_owned_runs > 1 ));
         Insert( translations, Assoc( "RUN-NAME", run_names ));
         Insert( translations, Assoc( "RUN-NUMBER", run_numbers ));
         Insert( translations, Assoc( "RUN-TIME", run_times ));
         Insert( translations, Assoc( "RUN-DESCRIPTION", run_descripts ));
         Insert( translations, Assoc( "IS-OWNERS-RUN", is_owners_run ));
         Insert( translations, Assoc( "IS-EDITED-RUN", is_edited_run ));
         Insert( translations, Assoc( "IS-COMPLETED-RUN", is_completed_run ));
         Insert( translations, Assoc( "IS-DISPLAYED-RUN", is_displayed_run ));
      end Fill_Out_Run_Lists;
   
      function To_String( action : Actions ) return String is
      begin
          case action is
            when no_action => return "No Action";
            when copy => return "Copy";
            when display => return "Display";
            when delete => return "Delete";
            when edit => return "Edit";
         end case;
      end To_String;
      
      function Extract_Action( cgi_values : AWS.Parameters.List ) return Actions is
      use globals.WSC_HTML_Utils;
      begin
         if( Contains_Value( cgi_values, "Edit" )) then 
            return edit;
         end if;
         if( Contains_Value( cgi_values, "Copy" )) then 
            return copy;
         end if;
         if( Contains_Value( cgi_values, "Display" )) then 
            return display;
         end if;
         if( Contains_Value( cgi_values, "Delete" )) then 
            return delete;
         end if;
         return no_action;
      end Extract_Action;
      
      procedure Edit_Runs( username : Unbounded_String; action : Actions; run_number : Integer ) is
         lrn        : Natural          := ( if run_number > 1_000_000 then run_number - 1_000_000 else run_number );
         owner      : Unbounded_String;
         target_run : Run;
         new_run    : Run;
      begin
         if run_number > 1_000_000 then 
            owner := TuS( "default" );
         else 
            owner := username;
         end if;
         target_run := Run_IO.Retrieve_By_PK( lrn, owner );
         case action is
            when no_action => null;
            when copy => 
               Run_IO.Clear_Status_Flag_For( username, edited ); 
               new_run := Run_IO.Make_New_Copy( target_run, username );
               new_run.comparison_run_id := run_number;
               Run_IO.Save( new_run );   
            when delete => 
               if( run_number < 1_000_000 )then
                  Run_IO.Delete( target_run );
                  Run_IO.Set_Edited_Run_To_Highest( username );
                end if;
            when edit => 
               target_run.status := edited;
               Run_IO.Clear_Status_Flag_For( username, edited ); 
               Run_IO.Save( target_run );
            when display => 
               Run_IO.Clear_Status_Flag_For( username, displayed ); 
               target_run.status := displayed;
               Run_IO.Save( target_run );
         end case;
      end Edit_Runs;
      
      URI            : constant String := AWS.Status.URI( Request );
      cgi_values     : constant AWS.Parameters.List := AWS.Status.Parameters( request );
      translations   : Translate_Set;
      logresult      : users.Login_Result := Handle_Login_With_Authentication( request );
      session_id     : constant AWS.Session.Id := AWS.Status.Session( request );
      root           : constant String := euws.WSC_Web_Root;
      path           : Unbounded_String_List := Split( URI, '/' );
      action         : constant Actions := Extract_Action( cgi_values );
      disable_output : Boolean;
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      Log( "got action as " & action'Img );

      if( action /= no_action )then
         declare
            rstring    : constant String := globals.WSC_HTML_Utils.Reverse_Table_Lookup( cgi_values, To_String( action ));
            run_number : constant Natural := Natural'Value( rstring );
         begin
            Log( "Index page callback; action |" & action'Img & "| rstring |" & rstring & " username " & TS( logresult.user.username ));
            Edit_Runs( logresult.user.username, action, run_number );
         end;
      end if;
      
      translations := Get_Std_Translations( request, logresult.user ); 
      Fill_Out_Run_Lists( translations, logresult.user.username );
      disable_output := Dont_Show_Output( request, logresult.user ); 
      -- Handle_Language( path, logresult.user, session_id );
      Insert( translations, Assoc( "MODEL-MENU", Get_Main_Menu( home_page, disable_output, logresult.user.lang )));
      
      
      return Web_Utils.Build_Input_Page(
         euws.template_components_path & "index",
         translations );
   end Index_Page_Callback;
   
   procedure Update_Data_Lists( rs_buffer : in out wsc_params.WSC_Editing_Buffer; wsc_run : runsett.Run ) is
      use wsc_params.WSC_Parameter_Editing_System; 
      package d renames DB_Commons;
      key         : constant Unbounded_String := TuS( "wsc_run.dataset_name" );
      enum        : Enumerated_Type_Rec := rs_buffer.Get_Enum( key ); 
      c           : d.Criteria;
      datasets    : constant Dataset_List.Vector := Dataset_IO.Retrieve( c );
      n           : constant Natural := Natural( datasets.Length );
   begin
      enum.Clear_All_Elements;
      for i in 1 .. n loop
         declare
            one_db     : constant Dataset := datasets.Element( i );
            is_default : constant Boolean := one_db.name = wsc_run.dataset_name;
            title      : constant String  := TS( one_db.title & " : " & one_db.creator & " : " & one_db.name );
         begin
            enum.Add_Element( i, TS( one_db.name ), i, is_default, title ); 
         end;
      end loop;
      rs_buffer.Replace_Enum( key, enum );
   end Update_Data_Lists;

   procedure Update_Run_Lists( 
      user      : Model.WSC.Users.User_Type; 
      rs_buffer : in out wsc_params.WSC_Editing_Buffer; 
      wsc_run   : runsett.Run ) is

      use wsc_params.WSC_Parameter_Editing_System;

      base_runs : Run_List.Vector := Run_IO.Get_Canditate_Base_Runs_For( user.username );
      n         : Natural := Natural( base_runs.Length );
      key       : constant Unbounded_String := TuS( "wsc_run.comparison_run_id" );
      enum      : Enumerated_Type_Rec := rs_buffer.Get_Enum( key ); 
      default_pos : Natural := 1;
   begin
      enum.Clear_All_Elements;
      Log( "Update_Run_Lists; n=" & n'Img );
      for i in 1 .. n loop
         declare
            one_run : Run := base_runs.Element( i );
            id  : Integer := one_run.run_id;
            title : constant String := TS( one_run.Make_Title( as_html => False ));
            is_default : Boolean;
         begin
            is_default := False;
            --
            -- we only have the comparison id to change, and we want both
            -- this user's old runs and the default runs to be choices, 
            -- so we'll add 1,000,000 to default run ids
            --
            if( one_run.username = Model.WSC.Users.DEFAULT_USERNAME )then
               id := 1_000_000 + id ;
            else
               if( one_run.status = edited )then
                  is_default := True;
                  default_pos := i;
               end if;
            end if;
            declare 
               name : constant String := Positive'Image( id )( 2 .. Positive'Image( id )'Length );
            begin
               Log( "Update_Run_Lists adding element " & name & " " & title );
               enum.Add_Element( 
                  after        => 9999, 
                  name         => name, 
                  value        => id,
                  is_default   => is_default,
                  default_text => title );
            end;
         end;
      end loop;
      rs_buffer.Replace_Enum( key, enum );
   end Update_Run_Lists;
   
   function Run_Settings_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      logresult        : users.Login_Result := Handle_Login_With_Authentication( request );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
   
      use Templates_Parser; 
      use Model.WSC.Formatting.Translations;
      use Model.WSC.Main_Menu;
      
      type Action_Type is ( no_action, save, set_base, create_dataset );
      
      function Extract_Action( cgi_values : AWS.Parameters.List ) return Action_Type is
      begin
         if globals.WSC_HTML_Utils.Contains_Key( cgi_values, "save" )then
            return save;
         elsif globals.WSC_HTML_Utils.Contains_Key( cgi_values, "set_base" )then
            return set_base;
         elsif globals.WSC_HTML_Utils.Contains_Key( cgi_values, "create_dataset" )then
            return create_dataset;
         else
            return no_action;
         end if;
      end Extract_Action;
   
      URI                : constant String := AWS.Status.URI( request );
      wsc_run            : runsett.Run;
      rs_buffer          : wsc_params.WSC_Editing_Buffer;
      cgi_values         : constant AWS.Parameters.List := AWS.Status.Parameters( request );
      settings_sys       : wsc_params.WSC_Parameter_Editing_System.Parameter_System_Rec := wsc_params.Get_WSC_Run_Settings;
      extra_translations : Translate_Set;
      full_translations : Translate_Set;
      error_message      : Unbounded_String;
      main_error_message : Unbounded_String;
      job_is_running     : Boolean := Is_Job_Running( request );
      input_page         : Unbounded_String;
      run_state          : Model.Run_Settings.State_Type;
      path_str           : constant Unbounded_String := settings_sys.instance_name;
      output_disabled    : Boolean := Dont_Show_Output( request, logresult.user );
      action             : Action_Type := Extract_Action( cgi_values );
      trail              : Trail_Type;
      saved_comparison_run_id  : Integer;
      model_params       : Model.WSC.Parameters.Parameters_Array;
    begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      wsc_run := Run_IO.Get_Current_Run_For( logresult.user.username, edited );
      rs_buffer := Model.WSC.Parameter_System_Declarations.Get_Loaded_Run_Settings_Buffer( wsc_run );
      Log( "Got action as " & action'Img );
      extra_translations := Get_Std_Translations( request, logresult.user );
      saved_comparison_run_id := wsc_run.comparison_run_id;
      Update_Run_Lists( logresult.user, rs_buffer, wsc_run );
      Update_Data_Lists( rs_buffer, wsc_run );
      trail := Trail_Session.Get( session_id, globals.SESSION_BREADCRUMBS );
      trail.Add(
         "Run Settings Page",
         URI,
         run_settings_page );
      if( action /= no_action )then
         rs_buffer.Load( cgi_values );               
         if rs_buffer.Get_Num_Errors = 0 then
            error_message := TuS( "" );
            Map_Buffer_To_Settings( rs_buffer, wsc_run );
            Log( "taking action " & action'Img );
            case action is
               when no_action => null;
               when save => Run_IO.Save( wsc_run ); -- already saved;
               when set_base =>
                  Log( "wsc_run.comparison_run_id " & wsc_run.comparison_run_id'Img & 
                       " saved_comparison_run_id " & saved_comparison_run_id'Img );
                  if( wsc_run.comparison_run_id /= saved_comparison_run_id )then
                     --
                     -- we've added 1,000,000 to default comaparison IDs, so...
                     --
                     if( wsc_run.comparison_run_id > 1_000_000 )then
                        wsc_run.comparison_run_id := wsc_run.comparison_run_id - 1_000_000;
                        wsc_run.comparison_username := Model.WSC.Users.DEFAULT_USERNAME;
                     else
                        wsc_run.comparison_username := wsc_run.username;
                     end if;   
                     Log( "making new base from id " & wsc_run.comparison_run_id'Img & " username " & TS( wsc_run.comparison_username ));
                     declare
                        run_to_copy : Run := Run_IO.Retrieve_By_PK( wsc_run.comparison_run_id, wsc_run.comparison_username );
                     begin
                        wsc_run := Run_IO.Make_New_Copy( run_to_copy, wsc_run.username );
                        wsc_run.title := TuS( "Copy of " ) & wsc_run.title; 
                        rs_buffer := Model.WSC.Parameter_System_Declarations.Get_Loaded_Run_Settings_Buffer( wsc_run );
                        Update_Run_Lists( logresult.user, rs_buffer, wsc_run );
                     end;   
                  end if;
               when create_dataset => 
                  Run_IO.Save( wsc_run );
                  run_state := Run_IO.Retrieve_State_For_Run( wsc_run );
                  run_state.phase := queued;
                  wsc_run.type_of_run := data_creation;
                  State_IO.Save( run_state );
                  model_params := Model.WSC.Parameters.DAO.Read( wsc_run, wsc_run.start_year, wsc_run.end_year );
                  -- Log( "creating run; wsc_run.Working_Root=" & TS( output_dir ));
                  web_runner.Submit_Run( 
                     session_id, 
                     wsc_run,
                     model_params );
                  job_is_running := True;
            end case;
         else
            error_message := TuS( "<div class='error_section'>" ) &
               Lookup( "submit_error_message", logresult.user.lang ) & 
               "</div>";
            end if;
      end if;
      Insert( extra_translations, Assoc( "MAIN-ERROR-MESSAGE", error_message ));
      Insert( extra_translations, Assoc( "SIMPLE-SUBMIT", True ));
      Insert( extra_translations, Assoc( "JOB-IS-RUNNING", job_is_running ));
      Insert( extra_translations, Assoc( "IS-INPUT-PAGE", True ));
      full_translations := wsc_params.WSC_Renderer.Create_Complete_Translations( 
         title               => wsc_run.Make_Title,
         buff                => rs_buffer,
         breadcrumb          => trail.To_String,
         model_menu          => Get_Main_Menu( run_settings_page, output_disabled, logresult.user.lang ),
         base_sys            => settings_sys, 
         sys                 => settings_sys, 
         parameter_prefix    => path_str,
         main_error_message  => error_message,
         job_is_running      => job_is_running,
         user                => logresult.user,
         extra_translations  => extra_translations );
      input_page := Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "run_settings", full_translations );         
      Run_IO.Save( wsc_run );
      Trail_Session.Set( session_id, globals.SESSION_BREADCRUMBS, trail );
      return AWS.Response.Build( "text/html", input_page );
   end Run_Settings_Callback;

   function Example_Popup_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      use Templates_Parser;
      use Model.WSC.Results;
      use Model.WSC.Results.Web_IO;
      use Model.WSC.Household;
      use Model.WSC.Household.Web_IO;
      use Model.WSC.Household.Database;
      use Model.WSC.Parameters;

      URI              : constant String := AWS.Status.URI( request );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
      parameters       : constant AWS.Parameters.List :=  AWS.Status.Parameters( Request );
      logresult        : users.Login_Result := Handle_Login_With_Authentication( request );
      hid              : Sernum_Value;
      wave             : Waves;
      translations     : Translate_Set;
      pid              : Sernum_Value;
      iteration        : Iteration_Number; 
      db               : Model.WSC.Household.Database.DB_Type;
      wsc_run          : runsett.Run;
      s                : Unbounded_String;
   begin
      Log( "EXAMPLE_CALLBACK entered" );
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      wsc_run := Run_IO.Get_Current_Run_For( logresult.user.username, displayed );
      if( wsc_run /= NULL_RUN )then
         translations := Get_Std_Translations( request, logresult.user );
         Model.WSC.Household.Web_IO.Get_Person_Ids_From_URL( parameters, wave, hid, pid, iteration );
         Open( db, Model.WSC.Global_Settings.Physical_Root & "data/wales/actual/",  actual, iteration );
         Open( db, Model.WSC.Global_Settings.Physical_Root & "data/wales/estimated/" & TS( wsc_run.dataset_name ) & "/",  estimated, iteration );
         s := TuS( Model.WSC.Household.Complete_Web_IO.Write_Household( db, wsc_run, hid, pid, iteration, wave, translations ));
         Close( db );
      end if;
      return AWS.Response.Build( "text/html", s );
   end Example_Popup_Callback;
   
   function CSV_File_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      wsc_run    : Run;
      URI          : constant String := AWS.Status.URI( request );
      logresult    : users.Login_Result := Handle_Login_With_Authentication( request );
   begin
      Log( "CSV Callback Entered" );
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      wsc_run := Run_IO.Get_Current_Run_For( logresult.user.username, displayed );
      if( wsc_run /= NULL_RUN )then
         if( Ada.Strings.Fixed.Index( URI, "summary" ) > 0 )then
            declare
               filename : constant String := 
                  Censor_String( TS( wsc_run.Make_Title( as_html=> False )) & "_" & Utils.Random_String ) & ".csv";
               csv : constant Unbounded_String := Model.WSC.Output.Web_IO.Convert_Summary_Results_To_CSV( wsc_run );
            begin
                Write_Whole_File( "/tmp/" & filename, csv );
                return AWS.Response.File(
                     Content_Type  => "text/csv",
                     Filename      => "/tmp/" & filename ,
                     Once          => True,
                     Disposition   => AWS.Response.Attachment,
                     User_Filename => filename );         
            end;
         elsif( Ada.Strings.Fixed.Index( URI, "population" ) > 0 )then
            declare
               target_dir : constant String := Model.WSC.Global_Settings.Physical_Root & "/data/wales/estimated/"  & TS( wsc_run.dataset_name ) & "/";
            begin
               log( "downloading data_creation_events.csv from dir " & target_dir ); 
                return AWS.Response.File(
                     Content_Type  => "text/csv",
                     Filename      => target_dir & "data_creation_events.csv",
                     Disposition   => AWS.Response.Attachment,
                     User_Filename => "data_creation_events.csv" );         
            end;
         end if;
      end if;
      return AWS.Response.Build( "text/html", "" ); 
   end CSV_File_Callback;

   function Chart_Popup_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
      use Templates_Parser;
      use Model.WSC.Output.Web_IO;
      use General_Chart_Constants;
      
      type Popup_Page_Type is ( no_popup, gallery_popup, single_chart_popup );
      
      --
      -- FIXME NOT NEEDED ANYMORE?
      --
      function Get_Which_Popup( path : Unbounded_String_List ) return Popup_Page_Type is
          popup : Popup_Page_Type := no_popup;
          ps   : Unbounded_String;
      begin
         ps := path.Element( 1 );
         Log( "got path element 1 as " & TS( ps ));
         if( ps = TuS( "gallery_popup" ))then
            popup := gallery_popup;
         elsif( ps = TuS( "single_chart_popup" ))then
            popup := single_chart_popup;
         end if;
         Log( "got popup as " & Popup_Page_Type'Image( popup ));
         return popup;
      end Get_Which_Popup;
      
      wsc_run    : Run;
      URI          : constant String := AWS.Status.URI( request );
      html         : Unbounded_String := TuS( "NOT IMPLEMENTED YET" );
      path         : Unbounded_String_List := Split( URI, '/' );
      logresult    : users.Login_Result := Handle_Login_With_Authentication( request );
      which_popup  : Popup_Page_Type;
      translations : Translate_Set;
   begin
      Log( "Popup Callback Entered" );
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      translations := Get_Std_Translations( request, logresult.user );
      wsc_run := Run_IO.Get_Current_Run_For( logresult.user.username, displayed );
      if( wsc_run /= NULL_RUN )then
         Insert( translations, Assoc( "TITLE", wsc_run.Make_Title ));
         Insert( translations, Assoc( "RUN-DESCRIPTION", wsc_run.Make_Title ));
         Insert( translations, Assoc( "MULTI-ITERATIONS", wsc_run.num_iterations > 1 ));
         path.Delete( 1, 2 ); -- strip "WSC" and "output_page" from the url
         which_popup := Get_Which_Popup( path );
         case which_popup is
            when no_popup | gallery_popup => null;
            when single_chart_popup =>
               declare
                  item_str       : constant String := Censor_String( TS( path.Element( 2 )));
                  item           : constant Summary_Items_Type := Summary_Items_Type'Value( item_str );
                  p_or_p_str     : constant String := Censor_String( TS( path.Element( 3 )));
                  p_or_p         : constant Pre_Or_Post := Pre_Or_Post'Value( p_or_p_str );
                  is_svg         : constant Boolean := False;
                  do_comparisons : constant Boolean := ( item > non_residential_clients );
                  chart          : constant Unbounded_String := 
                     Draw_Time_Series_Fan_Chart( wsc_run, item, p_or_p, large, is_svg );
                  table          : constant Unbounded_String := Get_Single_Table( wsc_run, item, do_comparisons );
              begin
                 Insert( translations, Assoc( "IS-OUTPUT-PAGE", True ));
                 Insert( translations, Assoc( "TITLE", Pretty_Print( item )));
                 Insert( translations, Assoc( "CHART", chart ));
                 Insert( translations, Assoc( "TABLE", table ));
                 Insert( translations, Assoc( "DO-COMPARISONS", do_comparisons ));
                 if do_comparisons then
                    declare
                       use Colours;
                       use Standard_Colours;
                       col1 : constant String := To_String( STD_COLOURS( light_turquoise ));
                       col2 : constant String := To_String( STD_COLOURS( pale_orange ));
                    begin
                       Insert( translations, Assoc( "PRE-COLOUR", col1 ));
                       Insert( translations, Assoc( "POST-COLOUR", col2 ));
                    end;
                 end if;
                 html := Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
                     euws.Dir_Separator & "chart_popup", translations );
              end;
         end case;
      end if;
      return AWS.Response.Build( "text/html", html ); -- application/xhtml+xml
   end Chart_Popup_Callback;

   function Make_Examples_List_Page( wsc_run : Run; wave : Waves; iteration : Positive ) return Unbounded_String is
   use Templates_Parser;
   use Model.WSC.Household;
   use WSC_DB_Data;
   use DB_Commons;

      function  Pers_Locs_To_Tag( locs : Personal_Results_Location_List ) return Vector_Tag is
         tags : Vector_Tag;
         n    : constant Natural := Natural( locs.Length );
      begin
         for i in 1 .. n loop
            declare
               loc : Personal_Results_Location := locs.Element( i );
            begin
               tags := tags & Model.WSC.Household.Web_IO.Personal_Url( loc.wave, loc.hid, loc.pid, iteration );
            end;
         end loop;
         return tags;
      end Pers_Locs_To_Tag;
      
      function  Pers_Locs_To_Hids( locs : Personal_Results_Location_List ) return Vector_Tag is
         tags : Vector_Tag;
         n    : constant Natural := Natural( locs.Length );
      begin
         for i in 1 .. n loop
            declare
               loc : Personal_Results_Location := locs.Element( i );
            begin
               tags := tags & loc.hid'Img;
            end;
         end loop;
         return tags;
      end Pers_Locs_To_Hids;
      
      
      function Get_List( which : Incomes_Type; op : operation_type ) return Unbounded_String is
         gain_lose_v  : Gain_Lose_List.Vector;
         pers_loc_v   : Personal_Results_Location_List;
         examples     : Vector_Tag;
         hids         : Vector_Tag;
         translations : Translate_Set;
         caption      : constant String := ( if op = gt then "Decreasing" elsif op = lt then "Increasing" else "No Change" );
      begin
         gain_lose_v := Gain_Lose_IO.Retrieve_Income(
            wsc_run.run_id,
            wsc_run.username,
            iteration,
            wave,
            which,
            op );
         pers_loc_v := Gain_Lose_To_Personal_Results( gain_lose_v );
         examples := Pers_Locs_To_Tag( pers_loc_v );
         Log( "Make_Examples_List_Page; Get_List; got " & Size( examples )'Img & 
            " examples for op " & op'Img & " which " & which'Img );
         hids := Pers_Locs_To_Hids( pers_loc_v );
         Insert( translations, Assoc( "HID", hids ));
         Insert( translations, Assoc( "EXAMPLES", examples ));
         Insert( translations, Assoc( "CAPTION", caption ));
         Insert( translations, Assoc( "WSC-WEB-ROOT", Model.WSC.Global_Settings.WSC_Web_Root ));
         return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
            euws.Dir_Separator & "example_list", translations );
      end Get_List;

      --
      -- note that gt ('>') means 'losing' since we have pre [op] post in the DB comparison
      --
      function Get_List( which : Results_Field_Name; op : operation_type ) return Unbounded_String is
         gain_lose_v  : Gain_Lose_List.Vector;
         pers_loc_v   : Personal_Results_Location_List;
         examples          : Vector_Tag;
         hids         : Vector_Tag;
         translations : Translate_Set;
         caption      : constant String := ( if op = gt then "Decreasing" elsif op = lt then "Increasing" else "No Change" );
      begin
         gain_lose_v := Gain_Lose_IO.Retrieve_Result(
            wsc_run.run_id,
            wsc_run.username,
            iteration,
            wave,
            which,
            op );
         pers_loc_v := Gain_Lose_To_Personal_Results( gain_lose_v );
         hids := Pers_Locs_To_Hids( pers_loc_v );
         examples := Pers_Locs_To_Tag( pers_loc_v );
         Insert( translations, Assoc( "HID", hids ));
         Insert( translations, Assoc( "EXAMPLES", examples ));
         Insert( translations, Assoc( "CAPTION", caption ));
         return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
            euws.Dir_Separator & "example_list", translations );
      end Get_List;
      
      html : Unbounded_String;
      translations : Translate_Set;
      pers         : Person;
   begin
      Insert( translations, Assoc( "NET-INCOME-GAINERS", Get_List( net_income, lt )));  -- lt: pre value less than post value, so gaining
      Insert( translations, Assoc( "NET-INCOME-LOSERS", Get_List( net_income, gt )));
      Insert( translations, Assoc( "NET-INCOME-NO-CHANGE", Get_List( net_income, eq )));

      Insert( translations, Assoc( "LA-CONTRIBUTIONS-INCREASE", Get_List( la_contributions, lt )));
      Insert( translations, Assoc( "LA-CONTRIBUTIONS-DECREASE", Get_List( la_contributions, gt )));
      Insert( translations, Assoc( "LA-CONTRIBUTIONS-NO-CHANGE", Get_List( la_contributions, eq )));
      
      Insert( translations, Assoc( "CLIENT-INCREASE", Get_List( client_contributions, lt )));
      Insert( translations, Assoc( "CLIENT-DECREASE", Get_List( client_contributions, gt )));
      Insert( translations, Assoc( "CLIENT-NO-CHANGE", Get_List( client_contributions, eq )));
      
      Insert( translations, Assoc( "PENSION-INCREASE", Get_List( ni_retir_pension, lt )));
      Insert( translations, Assoc( "PENSION-DECREASE", Get_List( ni_retir_pension, gt )));
      Insert( translations, Assoc( "PENSION-NO-CHANGE", Get_List( ni_retir_pension, eq )));
      
      Insert( translations, Assoc( "DLA-INCREASE", Get_List( disab_livng_allwnce, lt )));
      Insert( translations, Assoc( "DLA-DECREASE", Get_List( disab_livng_allwnce, gt )));
      Insert( translations, Assoc( "DLA-NO-CHANGE", Get_List( disab_livng_allwnce, eq )));

      Insert( translations, Assoc( "ATTENDANCE-INCREASE", Get_List( attendance_allow, lt )));
      Insert( translations, Assoc( "ATTENDANCE-DECREASE", Get_List( attendance_allow, gt )));
      Insert( translations, Assoc( "ATTENDANCE-NO-CHANGE", Get_List( attendance_allow, eq )));
      
      Insert( translations, Assoc( "PENSION-CREDIT-INCREASE", Get_List( pension_credit, lt )));
      Insert( translations, Assoc( "PENSION-CREDIT-DECREASE", Get_List( pension_credit, gt )));
      Insert( translations, Assoc( "PENSION-CREDIT-NO-CHANGE", Get_List( pension_credit, eq )));
      
      Insert( translations, Assoc( "WAVE", wave'Img ));
      Insert( translations, Assoc( "ITERATION", iteration'Img ));
      
      html := Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "example_lists_page", translations );
      return html;
   end Make_Examples_List_Page;
   
   
   function Output_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use Model.WSC.Output.Web_IO;
   use Model.WSC.Output;
   use Model.WSC.Results;
   use globals.WSC_HTML_Utils;
   use Tabulator_Commons;
   use Model.WSC.Formatting.Translations;
   
      URI              : constant String := AWS.Status.URI( request );
      cgi_values       : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
      logresult        : users.Login_Result := Handle_Login_With_Authentication( request );
      session_id       : constant AWS.Session.Id := AWS.Status.Session( request );
      wsc_run          : runsett.Run;

      function Get_Which_Year( 
         which_page       : Output_Page_Type;
         path : Unbounded_String_List ) return Year_Number is
         ps : Unbounded_String;
      begin
         if( Integer( path.Length ) < 1 )then
            if( which_page = summary_page )then
               return Year_Number'First; -- first is used as a signal that we want the all years summary
            else
               return wsc_run.Start_Year;
            end if;
         else
             ps := path.Element( 1 );             
             return Year_Number'Value( TS( ps ));
         end if;
      end Get_Which_Year;

      function Get_Which_Page( path : Unbounded_String_List ) return Output_Page_Type is
          page : Output_Page_Type := summary_page;
       begin
          if( Integer( path.Length ) < 2 )then
             page := summary_page;
             Log( "path < 2 " );
          else
             declare
                ps : String := Censor_String( TS( path.Element( 2 )));
             begin
                Log( "ps |" & ps & "|" );
                if ps = "summary_page" then
                   page := summary_page;
                elsif ps = "stats_page" then
                   page := stats_page;
                elsif ps = "gain_lose_page" then
                   page := gain_lose_page;
                elsif ps = "budget_page" then
                   page := budget_page;
                elsif ps = "inequality_page" then
                   page := inequality_page;   
                elsif ps = "poverty_page" then
                   page := poverty_page;   
                elsif ps = "examples_page" then
                   page := examples_page;   
                end if; 
             end;
          end if;
          return page;
       end Get_Which_Page;
   
      filename          : Unbounded_String;
      translations      : Translate_Set;
      output            : Outputs_Rec;
      table             : Unbounded_String := Null_Unbounded_String;
      gallery           : Unbounded_String := Null_Unbounded_String;
      controls          : Unbounded_String := Null_Unbounded_String;
      html              : Unbounded_String := TuS( "NOT IMPLEMENTED YET" );
      which_page        : Output_Page_Type;
      path              : Unbounded_String_List := Split( URI, '/' );
      this_year         : Year_Number := Year_Number'First; -- wsc_run.Start_Year;
      this_years_output : Outputs_Rec;
      trail             : Trail_Type;
      wave              : Waves;
      iteration         : Positive := 1; -- FIXME add this somewhere
   begin
      Log( "Output Callback Entered" );
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      wsc_run := Run_IO.Get_Current_Run_For( logresult.user.username, displayed );
      if( wsc_run = NULL_RUN )then
         Log( "output callback; got null run " );
         return AWS.Response.URL( Model.WSC.Global_Settings.WSC_Web_Root );
      end if;
      trail := Trail_Session.Get( session_id, globals.SESSION_BREADCRUMBS );
      
      path.Delete( 1, 2 ); -- strip "WSC" and "output_page" from the url
      which_page := Get_Which_Page( path );
      
      Log( "got page as " & which_page'Img & " path len " );
      -- Handle_Language( path, logresult.user, session_id );
      translations := Get_Std_Translations( request, logresult.user );
      if( which_page = summary_page ) and ( this_year = Year_Number'First ) then
         trail.Add(
            "Output:&nbsp;Summary Page",
            URI,
            Model.WSC.Main_Menu.output_page,
            Year_Number'First );
      else
         trail.Add(
            "Output:&nbsp;" & Prettify_Image( which_page'Img ),
            URI,
            Model.WSC.Main_Menu.output_page,
            this_year );
      end if;
      if( False )then  -- FIXME: any completed run
         html := Get_Output_Page(
                     title           => wsc_run.Make_Title,
                     translations    => translations,
                     which_page      => which_page,
                     breadcrumb      => trail.To_String,
                     start_year      => wsc_run.Start_Year,
                     end_year        => wsc_run.End_Year,
                     which_year      => this_year,
                     control_section => Null_Unbounded_String,
                     gallery         => Null_Unbounded_String,
                     content         => TuS( Lookup( "You have no completed model runs as yet", logresult.user.lang )), 
                     lang            => logresult.user.lang );
      else
          this_year := Get_Which_Year( which_page, path );
          
          if( this_year >= wsc_run.Start_Year ) and ( which_page = summary_page )then
             which_page := gain_lose_page;
          end if;
          
          case which_page is
          when stats_page =>
             --
             -- NOT USED??
             --
             declare                
                sysno : Integer; -- System_Nunber 
             begin
                sysno := Integer'Value( TS( path.Element( 3 )));                
                wave := Wave_From_Year( this_year );
                table := Get_Stats_Summary_Page( wsc_run, sysno, wave );
                html := Get_Output_Page( 
                            title           => wsc_run.Make_Title,
                            translations    => translations,
                            breadcrumb      => trail.To_String,
                            start_year      => wsc_run.Start_Year,
                            end_year        => wsc_run.End_Year,
                            which_year      => this_year,
                            which_page      => which_page,
                            control_section => controls,
                            gallery         => gallery,
                            content         => table,
                            lang            => en );
                end;
          when summary_page =>
             declare
                do_differences : Boolean := False;
                sysno          : Integer := 1;
             begin
                gallery := Null_Unbounded_String;
                if( Contains_Key( cgi_values, "Redraw" ))then 
                   do_differences := AWS.Parameters.Get( cgi_values, "do_differences", 1 ) = "on";
                   sysno          := Integer'Value( AWS.Parameters.Get( cgi_values, "sysno", 1 ));
                end if;
                controls := Null_Unbounded_String; 
                declare
                   fname : constant String := wsc_run.Qualified_Output_Directory( 1 ) & "main_output.html";
                begin
                   if( Ada.Directories.Exists( fname ))then
                      table := Text_Utils.Read_Whole_File( fname );
                   else
                      table := Get_All_Years_Summary( wsc_run );
                      Text_Utils.Write_Whole_File( fname, table );
                   end if;
                end;
                html := Get_Output_Page(      
                            title           => wsc_run.Make_Title,
                            translations    => translations,
                            breadcrumb      => trail.To_String,
                            start_year      => wsc_run.Start_Year,
                            end_year        => wsc_run.End_Year,
                            which_year      => this_year,
                            which_page      => which_page,
                            control_section => controls,
                            gallery         => gallery,
                            content         => table,
                            lang            => en );
             end;
          when gain_lose_page  =>
             declare
                breakdown    : Breakdown_Target   := Disaggregated_Breakdown_Target'First;
                comp_cell    : Compare_Cell       := current_cell;
                cell_op      : Cell_Compare_Type  := counter;
                value_To_Use : Summary_Items_Type := disposable_income;
             begin
                if( Contains_Key( cgi_values, "Redraw" ))then 
                   breakdown  := Breakdown_Target'Value( AWS.Parameters.Get( cgi_values, "breakdown", 1 ));
                   comp_cell  := Compare_Cell'Value(AWS.Parameters.Get( cgi_values, "comp_cell", 1 ));               
                   -- cell_op    := Cell_Compare_Type'Value( AWS.Parameters.Get( cgi_values, "cell_op", 1 ));          
                end if;
                wave := Wave_From_Year( this_year );
                table := Get_Gain_Lose_Table( 
                  wsc_run,
                  wave,
                  breakdown );
                controls := Get_Gain_Lose_Control_Section(
                   comp_cell => comp_cell,
                   breakdown => breakdown,
                   lang      => logresult.user.lang ); 
                html := Get_Output_Page(  
                         title           => wsc_run.Make_Title,
                         translations    => translations,
                         which_page      => which_page,
                         breadcrumb      => trail.To_String,
                         start_year      => wsc_run.Start_Year,
                         end_year        => wsc_run.End_Year,
                         which_year      => this_year,
                         control_section => controls,
                         gallery         => gallery,
                         content         => table,
                         lang            => en );
             end;
          when budget_page     => 
            declare
                print_counts   : Boolean := False;
                do_differences : Boolean := False;
                sysno          : Integer := 1;
                breakdown      : Breakdown_Target := Disaggregated_Breakdown_Target'First;
                col            : Costs_Type;
            begin
                if( Contains_Key( cgi_values, "Redraw" ))then 
                   print_counts   := AWS.Parameters.Get( cgi_values, "print_counts", 1 ) = "on";
                   do_differences := AWS.Parameters.Get( cgi_values, "do_differences", 1 ) = "on";
                   sysno          := Integer'Value( AWS.Parameters.Get( cgi_values, "sysno", 1 ));
                   breakdown      := Breakdown_Target'Value( AWS.Parameters.Get( cgi_values, "breakdown", 1 ));
                end if;
                wave := Wave_From_Year( this_year );
                table    := Get_Budget_Table(
                   wsc_run        => wsc_run,
                   wave           => wave,
                   sysno          => sysno,
                   breakdown      => breakdown,
                   print_counts   => print_counts,
                   do_differences => do_differences ); 
                controls := Get_Budget_Control_Section(
                   sysno          => sysno,
                   breakdown      => breakdown,
                   print_counts   => print_counts,
                   do_differences => do_differences,
                   lang           => logresult.user.lang );
                html := Get_Output_Page(      
                         title           => wsc_run.Make_Title,
                         translations    => translations,
                         which_page      => which_page,
                         breadcrumb      => trail.To_String,
                         start_year      => wsc_run.Start_Year,
                         end_year        => wsc_run.End_Year,
                         which_year      => this_year,
                         control_section => controls,
                         gallery         => gallery,
                         content         => table,
                         lang            => en );
            end;
          when inequality_page => null; -- TODO
          when poverty_page    => null; -- TODO
          when examples_page => 
             wave := Wave_From_Year( this_year );
             table := Make_Examples_List_Page( wsc_run, wave, iteration );
             html := Get_Output_Page(  
                      title           => wsc_run.Make_Title,
                      translations    => translations,
                      which_page      => which_page,
                      breadcrumb      => trail.To_String,
                      start_year      => wsc_run.Start_Year,
                      end_year        => wsc_run.End_Year,
                      which_year      => this_year,
                      control_section => controls,
                      gallery         => gallery,
                      content         => table,
                      lang            => en );
             
          end case;
       end if;
       Trail_Session.Set( session_id, globals.SESSION_BREADCRUMBS, trail );
       return AWS.Response.Build( "text/html", html ); -- application/xhtml+xml
   end Output_Callback;
   
   function Array_Update_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use Ada.Characters.Handling;
      cgi_values         : constant AWS.Parameters.List := AWS.Status.Parameters( request );
      
      type Actions is ( insert_below, insert_above, delete, no_action );
      
      function Extract_Action( action_string : String ) return Actions is
         action : Actions := no_action;
      begin
         if( action_string = "insert_below" )then 
            action := insert_below;
         elsif( action_string = "insert_above" )then 
            action := insert_above;
         elsif( action_string = "delete" )then 
            action := delete;
         end if;
         return action;
      end Extract_Action;
      
      GLOBAL_EDITING_SYSTEM   : constant wsc_params.WSC_Parameter_Editing_System.Parameter_System_Rec := wsc_params.Get_WSC_Parameter_Editing_System;
      session_id         : constant AWS.Session.Id := AWS.Status.Session( request );
      outs               : Unbounded_String;
      buff               : wsc_params.WSC_Editing_Buffer;
      array_target       : constant Unbounded_String := TuS( AWS.Parameters.Get( cgi_values, "target_params" ));
      row_string         : constant String := AWS.Parameters.Get( cgi_values, "row" );
      action_string      : constant String := AWS.Parameters.Get( cgi_values, "action" );
      action             : constant Actions := Extract_Action( action_string );
      ajax_target_key    : constant Unbounded_String := TuS( AWS.Parameters.Get( cgi_values, "ajax_target_key" ));
      row                : Natural := Natural'Value( row_string );
      nb                 : constant Natural := buff.Get_Current_Collection_Size( array_target );
      logresult          : users.Login_Result := Handle_Login_With_Authentication( request );
      max_len            : constant Natural := buff.Maximum_Collection_Size( array_target );
      wsc_run            : runsett.Run;
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      wsc_run  := Run_IO.Get_Current_Run_For( logresult.user.username, edited );
      if( wsc_run /= NULL_RUN )then
         buff := Model.WSC.Parameter_System_Declarations.Get_Loaded_Input_Buffer( wsc_run );
         Log(  "action is " & Actions'Image( action ));
         Log(  "max len " & Natural'Image( max_len ));
         Log(  "nb " & Natural'Image( nb ));
         Log(  "row " & Natural'Image( row ));
         Log(  "array_target " & TS( array_target ));
         case action is
         when insert_below => 
            if ( nb < max_len )then
               buff.Add( array_target, row+1 );
            end if;
         when insert_above =>
            if( nb < max_len )then
               buff.Add( array_target, row );
            end if;
         when delete       =>
            if( nb > 1 )then
               buff.Delete( array_target, row );
            end if;
         when no_action    =>
            Log( "unrecognised action " & action_string );
         end case;
         Buffer_Session.Set( session_id, globals.SESSION_PARAMETER_EDITING_BUFFER, buff );
         outs := wsc_params.WSC_Renderer.Make_Indexed_Block(
            GLOBAL_EDITING_SYSTEM,
            array_target,
            buff, 
            ajax_target_key );
         Log( "created outs as " & TS( outs ));
      end if;
      return AWS.Response.Build( "text/html", outs );
   end Array_Update_Callback;


   function Build_Help_Page( 
      path               : Unbounded_String_List ) return Unbounded_String is
   use Templates_Parser;
   use Model.WSC.Formatting.Translations;
   use Model.WSC.Formatting;
      GLOBAL_EDITING_SYSTEM  : constant wsc_params.WSC_Parameter_Editing_System.Parameter_System_Rec := wsc_params.Get_WSC_Parameter_Editing_System;
      SEP                    : constant String := euws.Dir_Separator;
      TOP_LEVEL_PREFIX       : constant Unbounded_String := GLOBAL_EDITING_SYSTEM.instance_name;
      
      help_sys     : wsc_params.WSC_Parameter_Editing_System.Parameter_System_Rec;
      desc         : Unbounded_String;
      translations : Translate_Set;
      help_list    : Vector_Tag;
      link_list    : Vector_Tag;
      title_list   : Vector_Tag;      
      path_str     : Unbounded_String;
      title        : Unbounded_String;
      text         : Unbounded_String;
      link         : Unbounded_String;
      path_copy    : Unbounded_String_List := path;
      path_length  : Natural := Natural( path_copy.length );
   begin
      Log( "Build_Help_Page; path_length = " & Natural'Image( path_length ));
      -- FIXME HELP SYS
      for i in reverse 1 .. path_length loop
         path_str :=  TOP_LEVEL_PREFIX & "." & TuS( Join( path_copy, '.' ));
         Log( "getting system " & TS( path_str ));
         help_sys := GLOBAL_EDITING_SYSTEM.Get( TS( path_str ));
         text := TuS( help_sys.Description( wsc_params.WSC_Parameter_Editing_System.description, en ));
         title := TuS( help_sys.Description( wsc_params.WSC_Parameter_Editing_System.label, en ));
         link := path_str;
         
         help_list := help_list & text;
         link_list := link_list & link;
         title_list := title_list & title;
         
         path_copy.Delete( i );
      end loop;
      Insert( translations, Assoc( "TEMPLATE_ROOT", euws.template_components_path ));
      Insert( translations, Assoc( "LANG", Lang_Str( en )));
      Insert( translations, Assoc( "ROOT", euws.WSC_Web_Root ));
      Insert( translations, Assoc( "SEP", euws.Dir_Separator ));
      Insert( translations, Assoc( "PAGE-TITLE", "WSC&nbsp;" & Lookup( "Help", en )));
      Insert( translations, Assoc( "CONTENTS", help_list ));
      Insert( translations, Assoc( "LINKS", link_list ));
      Insert( translations, Assoc( "TITLES", title_list ));
      return TuS( Web_Utils.Parse_Template( 
         euws.template_components_path & 
         euws.dir_separator & "help_popup", 
         translations ));      
   end Build_Help_Page;

   type Actions is ( apply, run_model, uprate, reset, abort_it, no_action );

   function Extract_Action( cgi_values : AWS.Parameters.List ) return Actions is
   use globals.WSC_HTML_Utils;
      action : Actions := no_action;
   begin
      Log( "Extract_Action; cgi is " & Dump( cgi_values )); -- fixme take this out
      if( Contains_Key( cgi_values, "apply" ))then 
         action :=  apply;
      elsif( Contains_Key( cgi_values, "run" ))then 
         action :=  run_model;
      elsif( Contains_Key( cgi_values, "uprate" ))then 
         action :=  uprate;
      elsif( Contains_Key( cgi_values, "reset" ))then 
         action :=  reset;
      elsif( Contains_Key( cgi_values, "abort" ))then 
         action :=  abort_it;
      end if;
      Log( "Extract_Action; got action as " & action'Img );
      return action;
   end Extract_Action;
     
   function Param_Dump_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use Model.WSC.Main_Menu;
   use Model.WSC.Formatting.Translations;
   use Ada.Containers;
   use Base_Model_Types;
   use Model.WSC.Parameters;
   use Model.WSC.Globals;
      wsc_run               : runsett.Run;
      session_id            : constant AWS.Session.Id := AWS.Status.Session( request );
      param_editing_buffer  : wsc_params.WSC_Editing_Buffer;
      GLOBAL_EDITING_SYSTEM : constant wsc_params.WSC_Parameter_Editing_System.Parameter_System_Rec :=
         wsc_params.Get_WSC_Parameter_Editing_System;
      logresult             : users.Login_Result := Handle_Login_With_Authentication( request );
      input_page            : Unbounded_String;
      output_disabled       : Boolean;
      year_cells            : Templates_Parser.Vector_Tag;
      translations          : Translate_Set;
      main_menu             : Unbounded_String;
   begin
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      wsc_run :=  Run_IO.Get_Current_Run_For( logresult.user.username, edited );
      if( wsc_run /= NULL_RUN )then
         translations := Get_Std_Translations( request, logresult.user );
         output_disabled := Dont_Show_Output( request, logresult.user );
         main_menu := Get_Main_Menu( dump_params_page, output_disabled, logresult.user.lang );
         param_editing_buffer :=  Model.WSC.Parameter_System_Declarations.Get_Loaded_Input_Buffer( wsc_run );
         param_editing_buffer.Set_Language( logresult.user.lang ); 
         for year in wsc_run.start_year .. wsc_run.end_year loop
            param_editing_buffer.Set_Current_Year( year );
            year_cells := year_cells & 
               wsc_params.WSC_Renderer.Create_Static_Page(
                  buff =>  param_editing_buffer,
                  sys  => GLOBAL_EDITING_SYSTEM, 
                  parameter_prefix  => TuS( "" ));
         end loop;
         Insert( translations, Assoc( "MAIN-MENU", main_menu ));
         Insert( translations, Assoc( "YEAR-PAGES", year_cells ));
         input_page := Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
            euws.Dir_Separator & "static_input", translations );
      end if;
      return AWS.Response.Build( "text/html", input_page );
   end Param_Dump_Callback;
 
   function Parameter_Page_Callback( request : in AWS.Status.Data ) return AWS.Response.Data is
   use Templates_Parser;
   use Model.WSC.Main_Menu;
   use Model.WSC.Formatting.Translations;
   use Ada.Containers;
   use Model.WSC.Parameters;
   use Base_Model_Types;
   use Model.WSC.Globals;
   
      GLOBAL_EDITING_SYSTEM : constant wsc_params.WSC_Parameter_Editing_System.Parameter_System_Rec :=
         wsc_params.Get_WSC_Parameter_Editing_System;
      num_errors            : Natural := 0;
      param_editing_buffer  : wsc_params.WSC_Editing_Buffer;
      target_subsystem      : wsc_params.WSC_Parameter_Editing_System.Parameter_System_Rec;
      cgi_values            : constant AWS.Parameters.List := AWS.Status.Parameters( Request );
      path_str              : Unbounded_String;
      TOP_LEVEL_PREFIX      : constant Unbounded_String := GLOBAL_EDITING_SYSTEM.instance_name;
      wsc_run               : runsett.Run;
      session_id            : constant AWS.Session.Id := AWS.Status.Session( request );
      model_params          : Parameters_Array;
      
      procedure Apply( action : Apply_Actions; year : Year_Number ) is
         num_errors : Natural;
         start_year, end_year : Year_Number;
      begin
         Log( "Apply action: " & Apply_Actions'Image( action ) & " year: " & Year_Number'Image( year ));
         param_editing_buffer.Set_Current_Year( year );
         case action is
            when this_year_only =>
               start_year := year;
               end_year := year;
            when this_and_subsequent_years =>
               start_year := year;
               end_year := wsc_run.end_year;
            when all_years =>
               start_year := wsc_run.start_year;
               end_year := wsc_run.end_year;
         end case;
         for y in start_year .. end_year loop
            param_editing_buffer.Load( cgi_values, y );               
         end loop;
         num_errors := param_editing_buffer.Get_Num_Errors;
         if( num_errors = 0 )then
            Map_And_Set_Parameters( wsc_run, param_editing_buffer, year, get_changed_values, start_year, end_year );
         end if;
         param_editing_buffer.Set_Current_Year( year );
      end Apply;

      apply_act          : Apply_Actions;
      SEP                : constant String := euws.Dir_Separator;
      URI                : constant String := AWS.Status.URI( Request );
      logresult          : users.Login_Result := Handle_Login_With_Authentication( request );
      which_page         : constant String := AWS.Parameters.Get( cgi_values, "which_page", 1 );
      
      action             : Actions := Extract_Action( cgi_values );
      depth              : Natural := 0;
      error_message      : Unbounded_String;
      extra_translations : Translate_Set;
      input_page         : Unbounded_String;
      job_is_running     : Boolean := Is_Job_Running( request );
      main_error_message : Unbounded_String;
      model_menu         : Unbounded_String;
      path               : Unbounded_String_List := Split( URI, '/' );
      prefix             : Unbounded_String := TuS( Model.WSC.Global_Settings.WSC_Web_Root & "parameters_page/" );
      run_state          : Model.Run_Settings.State_Type;
      output_disabled    : Boolean;
      in_help_mode       : Boolean := False;
      year               : Year_Number;
      fully_qualified_path_Str : Unbounded_String;
      year_str           : Unbounded_String;
      trail              : Trail_Type; 
      output_dir         : Unbounded_String;
   begin
      Log( "entered parameters page" );
      if(( not logresult.validated ) or ( logresult.new_session )) then
         return logresult.response;
      end if;
      
      wsc_run := Run_IO.Get_Current_Run_For( logresult.user.username, edited );
      Log( "Parameter_Page_Callback; got run id as " & wsc_run.run_id'Img );
      if( wsc_run = NULL_RUN )then
         Log( "parameter_page_callback; got NULL run; back to root page" );
         return AWS.Response.URL( Model.WSC.Global_Settings.WSC_Web_Root );
      end if;
      run_state := Run_IO.Retrieve_State_For_Run( wsc_run );
      trail := Trail_Session.Get( session_id, globals.SESSION_BREADCRUMBS );
      output_disabled := Dont_Show_Output( request, logresult.user );
      Log( AWS.Parameters.URI_Format( cgi_values ));
      param_editing_buffer := Model.WSC.Parameter_System_Declarations.Get_Loaded_Input_Buffer( wsc_run );
      param_editing_buffer.Set_Language( logresult.user.lang ); 
      Log( "Parameter_Page_Callback; got wsc_run as " & wsc_run.To_String );
      extra_translations := Get_Std_Translations( request, logresult.user );

      if( path.length > 2 )then
         year_str := path.Element( 3 );
         year := Year_Number'Value( TS( year_str ));
      else
         year := wsc_run.start_year;
      end if;
      param_editing_buffer.Set_Current_Year( year );
      path.Delete( 1, 3 );
      Log( "Parameter_Page_Callback; got year as " & Year_Number'Image( year ));
      if( path.Length > 0 ) and then (path.Element( Natural( path.Length )) = TuS( "help" ))then
          path.Delete( Natural( path.Length ));
          in_help_mode := True;
      end if;
      -- the path this page represents in the menu, e.g. the 1st tax page and so on
      GLOBAL_EDITING_SYSTEM.Complete_Path_To_Left( path, depth );
      -- use this to complete the top menu    
      model_menu := model_menu & Get_Main_Menu( parameters_page, output_disabled, logresult.user.lang );
      model_menu := model_menu & LINE_BREAK;
      model_menu := model_menu & wsc_params.WSC_Renderer.Make_Parameter_Menu( 
         wsc_run.start_year,
         wsc_run.end_year,
         year, 
         GLOBAL_EDITING_SYSTEM,         
         path, 
         prefix, 
         logresult.user.lang, 
         logresult.user );      
      model_menu := model_menu & LINE_BREAK;
      --
      -- this gets the xml parameters system description of the component we're on
      --
      fully_qualified_path_Str := TOP_LEVEL_PREFIX & "." & TuS( Join( path, '.' ));
      path_str := "." & TuS( Join( path, '.' ));
      target_subsystem := GLOBAL_EDITING_SYSTEM.Get( TS( fully_qualified_path_Str ));
      
      trail.Add(
         target_subsystem.Description( wsc_params.WSC_Parameter_Editing_System.label ),
         URI,
         parameters_page,
         year );
      
      Log(  "fully_qualified_path_Str " & TS( fully_qualified_path_Str ));
      if( in_help_mode )then
         return AWS.Response.Build( "text/html", Build_Help_Page( path ));
      end if;
      Log( "Parameter_Page_Callback entered" );
      Log( "URI = " & URI );
      Log( "action = " & Actions'Image( action ));
      Log( "session_id " & AWS.Session.Image( session_id ));
      output_dir := Model.WSC.Run_Declarations.Create_Directories_For_Run( wsc_run );
      -- initial error check - so we don't allow running on a new page 
      -- if there are errors elsewhere
      num_errors := param_editing_buffer.Get_Num_Errors;
      case action is
      when run_model =>
         apply_act := Apply_Actions'Value( AWS.Parameters.Get( cgi_values, "apply_type", 1 ));
         Apply( apply_act, year );
         num_errors := param_editing_buffer.Get_Num_Errors;
         Log( "run entered; num_errors=" & Natural'Image( num_errors ) & " job_is_running " & Boolean'Image( job_is_running ));
         if( not job_is_running ) and num_errors = 0 then
            declare
               new_run : Run;
            begin
               run_state.phase := queued;
               State_IO.Save( run_state );
               --
               wsc_run.type_of_run := simulation;
               wsc_run.status := running_or_queued;
               model_params := Model.WSC.Parameters.DAO.Read( wsc_run, wsc_run.start_year, wsc_run.end_year );
               Run_IO.Save( wsc_run );
               --
               --
               Log( "creating run; wsc_run.Working_Root=" & TS( output_dir ));
               web_runner.Submit_Run( 
                  session_id, 
                  wsc_run,
                  model_params );
               job_is_running := True;
               --
               -- make a new run and make it the edited run
               --
               wsc_run := Run_IO.Make_New_Copy( wsc_run, logresult.user.username );
               wsc_run.status := edited;
               Run_IO.Clear_Status_Flag_For( logresult.user.username, edited ); 
               Run_IO.Save( wsc_run );
            end;
         end if;
      when reset =>
         Log( "reset entered" );
         declare
            reset_act : Reset_Actions := Reset_Actions'Value( AWS.Parameters.Get( cgi_values, "reset_type", 1 ));
         begin
            Reset( wsc_run, target_subsystem, path_str, GLOBAL_EDITING_SYSTEM, param_editing_buffer, reset_act, year );
         end;
      when apply =>
         Log( "apply_type " & AWS.Parameters.Get( cgi_values, "apply_type", 1 ));
         apply_act := Apply_Actions'Value( AWS.Parameters.Get( cgi_values, "apply_type", 1 ));
         Apply( apply_act, year );
      when abort_it =>
         Log( "aborting" );
         AWS.Session.Set( session_id, globals.SESSION_ABORTING, True );
      when uprate => 
         Log( "uprate entered" );
         declare
            use Globals;
            use Utils;
            ms : constant String := AWS.Parameters.Get( cgi_values, "uprate_amount", 1 );
            m  : Rate;
            message : Unbounded_String;
            error : Utils.Error_Type;
            pstr : Unbounded_String := Unbounded_Slice( path_str, 2, Length( path_str )); -- FIXME: FIX THESE F**ING INDEXES
            uprate_act : Uprate_Actions := Uprate_Actions'Value( AWS.Parameters.Get( cgi_values, "uprate_type", 1 ));
            uprate_index : Which_Uprate_Index := Which_Uprate_Index'Value( AWS.Parameters.Get( cgi_values, "uprate_index", 1 ));
         begin
            Log( " ms = |" & ms );
            -- load so we save any other changes the user made before pressing uprate
            Apply( apply_act, year );
            Model.WSC.Formatting.Web_Validate(
               ms,
               logresult.user.lang,
               m,
               message,
               error,
               -1_000.0,
               1_000.0 );
            if( error = No_Error )then
               Model.WSC.Globals.Uprate( wsc_run, param_editing_buffer, pstr, uprate_act, uprate_index, year, m );
               Insert( extra_translations, Assoc( "UPRATE-ERROR", "" ));
            else
               message := TuS( LINE_BREAK & "<br/><span class='input_error_message'>" ) & message & "</span>" & LINE_BREAK;
               Insert( extra_translations, Assoc( "UPRATE-ERROR", message ));
               Insert( extra_translations, Assoc( "UPRATE-FIELD-CLASS", " class='input_error' " ));
            end if;   
         end;
      when no_action => null;   
      end case;
      -- new error count on the way out ...
      num_errors := param_editing_buffer.Get_Num_Errors;
      Log( "got error count as " & Natural'Image( num_errors ));
      if( num_errors /= 0 ) then
         main_error_message := TuS( "<div class='error_section'>" ) &
               Lookup( "submit_error_message", logresult.user.lang ) & 
               "</div>";
      else
         main_error_message := TuS( "" );
      end if;
      Insert( extra_translations, Assoc( "MAIN-ERROR-MESSAGE", main_error_message ));
      if( job_is_running or ( num_errors > 0 ))then
         Insert( extra_translations, Assoc( "DISABLE-RUN", " disabled='disabled' " ));    
      end if;
      Insert( extra_translations, Assoc( "JOB-IS-RUNNING", job_is_running ));
      Insert( extra_translations, Assoc( "IS-INPUT-PAGE", True ));
      
      -- redo the path str without the prefix  MESSY
      
      input_page := wsc_params.WSC_Renderer.Create_Input_Page(
         title               => wsc_run.Make_Title,
         buff                => param_editing_buffer,
         model_menu          => model_menu,
         breadcrumb          => trail.To_String,
         base_sys            => GLOBAL_EDITING_SYSTEM, 
         sys                 => target_subsystem, 
         parameter_prefix    => path_str,
         main_error_message  => error_message,
         job_is_running      => job_is_running,
         user                => logresult.user,
         extra_translations  => extra_translations );
      Trail_Session.Set( session_id, globals.SESSION_BREADCRUMBS, trail );
      Run_IO.Save( wsc_run );
      return AWS.Response.Build( "text/html", input_page );
   end Parameter_Page_Callback;

end Callbacks.Wales;
