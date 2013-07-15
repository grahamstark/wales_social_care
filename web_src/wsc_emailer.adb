with Ada.Directories;
with Ada.Text_IO;

with GNATCOLL.Email;
with GNATCOLL.Traces;

with Gnat.OS_Lib;
with Templates_Parser;
with System;
with Text_Utils;
with Ada.Strings.Unbounded;
with Utils;
with User_Type_IO;

with Model.WSC.Global_Settings;

package body WSC_Emailer is

   use Text_Utils;
   use Ada.Strings.Unbounded;
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "WSC_EMAILER" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;
   
   procedure Send_Email( title : String; to : String; complete_email : String; result : out Email_Result ) is
   use System;
   use Ada.Text_IO;
   use Gnat.OS_Lib;
      args          : Gnat.OS_Lib.Argument_List( 1 .. 3 );
      f             : File_Type;
      tmp_file_name : constant String := "/tmp/email_" & Utils.Random_String & "_" & Utils.Random_String & ".email";
      pid           : Integer;
      success       : Boolean;
   begin
      Create( f, Out_File, tmp_file_name );
      Put_Line( f, complete_email );
      Close( f );
      args( 1 ) := new String'( title );
      args( 2 ) := new String'( to );
      args( 3 ) := new String'( tmp_file_name );
      Log( "sending to " & to );
      Spawn(
         Program_Name => "/home/graham_s/bin/email", 
         Args         => args, 
         Output_File  => tmp_file_name & ".errs",
         Return_Code  => pid,
         Success      => success,
         Err_to_Out   => True );
      result := ( if success then ok else email_failed );
      if( success )then
         Ada.Directories.Delete_File( tmp_file_name );
         Ada.Directories.Delete_File( tmp_file_name & ".errs" );
      end if;
      Log( "PID " & pid'Img );   
      Free( args( 1 ));
      Free( args( 2 ));
      Free( args( 3 ));
   end Send_Email;
   
   function Make_Run_End_Email( title : String; user : User_Type; wsc_run : Run ) return String is
   use GNATCOLL.Email;
   
      function Make_Job_Completed_Message return String is
         use Templates_Parser;
         translations : Translate_Set;
         s            : Unbounded_String;
      begin
         Insert( translations, Assoc( "USER", user.username ));
         Insert( translations, Assoc( "RUN-ID", wsc_run.run_id ));
         Insert( translations, Assoc( "TITLE", wsc_run.title ));
         s := Templates_Parser.Parse( Model.WSC.Global_Settings.template_components_path & "run_end_email.tmpl", translations );
         return TS( s );
      end Make_Job_Completed_Message;
   
      mess : Message := New_Message;
      mess_as_string : Unbounded_String;
   begin
      mess.Set_Default_Headers( 
         From_Email => "grahams@virtual-worlds-research.com",
         Subject    => title
      );
      mess.Set_Text_Payload(
         Payload => Make_Job_Completed_Message
      );
      mess.To_String( Result => mess_as_string );
      return TS( mess_as_string );
   end Make_Run_End_Email;
   
   function Send_Run_End_Email( wsc_run : Run ) return Email_Result is
      result  : Email_Result;
      user : User_Type := User_Type_IO.Retrieve_By_PK( wsc_run.username ); 
      title : constant String := "Your Welsh Social Care Simulation (id: " & wsc_run.run_id'Img & ") Has Completed";
   begin
      if( user.email = NULL_UNBOUNDED_STRING )then
         result := no_email_address;
      else
         declare
            message : String := Make_Run_End_Email( title, user, wsc_run );
            to      : String := TS( user.email );
         begin
            Send_Email( title, to, message, result );
         end;
      end if;
      return result;
   end Send_Run_End_Email;
   
end WSC_Emailer;

