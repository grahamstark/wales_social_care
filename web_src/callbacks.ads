------------------------------------------------------------------------------
--                                                                          --
--  Handlers for each of WSC's callbacks, plus some support functions   --
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
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;

with Text_Utils;

with AWS.Session;
with AWS.Response;
with AWS.Parameters;
with AWS.Status;
with Templates_Parser;

-- with Model.WSC.Run_Results;

with Model.WSC.Formatting;
with Model.WSC.Global_Settings;
with Model.WSC.Globals;
with Model.WSC.Parameter_System_Declarations;
with Model.WSC.Run_Declarations;
with WSC_Enums;
with Model.WSC.Output;
with Model.WSC.Users;
with Model.WSC.Users.IO;
with Model.WSC.Dynamic_Driver;
with Model.WSC.Dynamic_Driver.Web_Runner;

with Parameter_System.Input_Buffer.WSC_Renderer;

--
-- Callbacks common to WSC , plus
-- some private support routines for handling logins, etc.
-- Main model handlers should be child packages of this.
--
package Callbacks is
   
   use Ada.Strings.Unbounded;
   use Text_Utils;
   
   function Serve_Static_Resource( Request : in AWS.Status.Data ) return AWS.Response.Data;
   -- function Serve_File_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Logout_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
   function Run_Settings_Callback( request : in AWS.Status.Data ) return AWS.Response.Data;
    
private  
   
   package chars                 renames Ada.Characters.Handling;
   package users                 renames Model.WSC.Users;
   package globals               renames Model.WSC.Globals;
   package wsc_params            renames Model.WSC.Parameter_System_Declarations;
   package runsett               renames Model.WSC.Run_Declarations;
   package outp                  renames Model.WSC.Output;
   package web_runner            renames Model.WSC.Dynamic_Driver.Web_Runner;
   package euws                  renames Model.WSC.Global_Settings;
   
   -- package Output_Session        renames globals.Output_Session_Package;
   -- package Run_State_Session     renames globals.Run_State_Session_Package;
   -- package Run_Settings_Session  renames globals.Run_Settings_Session_Package;
   package Buffer_Session        renames globals.Buffer_Session_Package;
   -- package Parameters_Session    renames globals.Parameters_Session_Package;
   -- package Previous_Runs_Session renames globals.Previous_Runs_Session_Package;   
   -- package Results_DB_Session    renames globals.Results_DB_Session_Package;
   package Trail_Session         renames globals.Breadcrumbs_Session_Package;
   -- subtype Param_Buff is wsc_params.WSC_Parameter_Editing_System_IO.Buffer;

   function Handle_Login_With_Authentication( 
      request : in AWS.Status.Data ) return users.Login_Result;

   function Is_Job_Running( 
      request : in AWS.Status.Data ) return Boolean;
   
   function Get_Std_Translations( 
      request : in AWS.Status.Data; 
      user : users.User_Type ) return Templates_Parser.Translate_Set;
   
   procedure Map_Buffer_To_Settings( 
      rs_buffer : wsc_params.WSC_Editing_Buffer; 
      wsc_run : in out runsett.Run );


end Callbacks;
