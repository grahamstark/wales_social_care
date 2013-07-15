with Ada.Calendar.Formatting;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Model.Run_Settings;

with Model.WSC.Formatting;
with Model.WSC.Globals;
with Model.WSC.Output;
with Model.WSC.Parameter_System_Declarations;
with Model.WSC.Parameters.IO;

with Text_Utils;
with Utils;
with GNATColl.Traces;
with Run_IO;

--
--
-- 
--
package body Model.WSC.Run_Results is

   use Ada.Strings.Unbounded;
   use Text_Utils;
   use Ada.Text_IO;
   use Ada.Calendar;
   use Model.WSC.Formatting;
   use Ada.Exceptions;
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.RUN_RESULTS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
   
   
   function Get_Run_Settings_Used_For_Run( list : Run_Results_List; which : Integer ) return Run is
   begin
      if( which = 0 )then
         null;         
      else
         return list.Element( which ).settings;
      end if;
   end Get_Run_Settings_Used_For_Run;
   
   function Get_Parameter_System_Used_For_Run( list : Run_Results_List; wsc_run : Run ) return Parameters_Array is
      p : Parameters_Array;
   begin
      p := Model.WSC.Parameters.IO.Read( 
         wsc_run.Qualified_Run_Directory & "/params/wsc.prm", 
         wsc_run.start_year, 
         wsc_run.end_year );
      return p;
   end Get_Parameter_System_Used_For_Run;

 
   function To_Select_Options( l : Run_Results_List; currently_selected : Natural ) return String is
      s : Unbounded_String;
      r : Run_Results_Record;
      n : constant Natural := Natural( l.length );
   begin
      for i in 1 .. n loop
         s := s & "<option value='" & Format( i ) & "' ";
         if( i = currently_selected )then
            s := s & " selected='selected' ";
         end if;
         s := s & ">";
         r := l.Element( i );
         s := s & r.wsc_run.title;
         s := s & " : ";
         s := s & Ada.Calendar.Formatting.Image( r.date );
         s := s & "</option>" & LINE_BREAK;
      end loop;
      return TS( s );
   end To_Select_Options;
   
   function To_String( r : Run_Results_Record ) return String is
   use Text_Utils;
      s : Unbounded_String;
   begin
      s := s & "Time : " & Ada.Calendar.Formatting.Image( r.date ) & LINE_BREAK;
      s := s & "Success: " & Boolean'Image( r.success ) & LINE_BREAK;
      s := s & "Run Number: " & r.run_number & LINE_BREAK;
      return TS( s );
   end To_String;
   
   function Compare_Run_Results( r1, r2 : Run_Results_Record ) return boolean is
   use Ada.Calendar;
   begin
      return ( r1.date < r2.date );
   end Compare_Run_Results;
   
   package Job_Sorter is new Run_Results_Package.Generic_Sorting( "<" => Compare_Run_Results ); 
   
   function Get_Previous_Runs( ctl : in Run ) return Run_Results_List is
   use Ada.Directories;
   use Ada.Text_IO;
   use Text_Utils;
   use Model.WSC.Run_Declarations;
   use Model.Run_Settings;

      l : Run_Results_List;
      local_ctl : Run := Ctl.copy;
      
      procedure Find_Entry( directory_entry : Directory_Entry_Type ) is
         file  : File_Type;
         basename    : constant String := Base_Name( Simple_Name( directory_entry ));
         fullname    : constant String := Full_Name( directory_entry );
         r           : Run_Results_Record;
      begin
         local_ctl.Set_Run_Id( basename );
         r.date := Modification_Time( fullname );
         r.directory := TuS( basename );
         r.run_number :=TuS( basename );
         r.success := True;
         r.settings := Read_Settings( fullname & "/params/wsc_run.txt" );
         r.state   := Read_State( fullname & "/params/run_state.txt" );
         l.Append( r );
      exception
         when error : others => 
            Log( "failed to open " & basename & Exception_Information( error ));
      end Find_Entry;
      
   begin
      Log( "Past_Runs: looking for dir |" & local_ctl.Qualified_Users_Directory & "|" );
      if( not Exists( local_ctl.Qualified_Users_Directory ))then
         return l;
      end if;
      Search( local_ctl.Qualified_Users_Directory, "run_*", ( Directory => True, others => False ), Find_Entry'Access );
      Job_Sorter.Sort( l );
      return l;
      exception
         when Ada.IO_Exceptions.Name_Error => null; -- don't care if there's no such directory.
   end Get_Previous_Runs;
   
   function Last_Successful_Run( run_results : Run_Results_List ) return Natural is
      l : Natural := Natural( run_results.length );
   begin
      for i in reverse 1 .. l loop
         if( run_results.Element( i ).success )then
            return i;
         end if;
      end loop;
      return 0;
   end Last_Successful_Run;
      
end Model.WSC.Run_Results;
