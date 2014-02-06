with Ada.Calendar.Formatting;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATColl.Traces;

with Model.WSC.Run_Settings;

with Model.WSC.Formatting;
with Model.WSC.Globals;
with Model.WSC.Output;
with Model.WSC.Parameter_System_Declarations;
with Model.WSC.Global_Settings;

with Text_Utils;
with Utils;

package body Model.WSC.Datasets is

   use Ada.Strings.Unbounded;
   use Text_Utils;
   use Ada.Text_IO;
   use Ada.Calendar;
   use Model.WSC.Formatting;
   use Ada.Exceptions;
 
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.DATASETS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
   
   function To_String( r : Datasets_Record ) return String is
   use Text_Utils;
      s : Unbounded_String;
   begin
      s := s & "Time : " & Ada.Calendar.Formatting.Image( r.date ) & LINE_BREAK;
      s := s & "Directory: " & r.directory  & LINE_BREAK;
      s := s & "Name: " & r.name & LINE_BREAK;
      return TS( s );
   end To_String;
   
   function Compare_Datasets( r1, r2 : Datasets_Record ) return boolean is
   use Ada.Calendar;
   begin
      return ( r1.date < r2.date );
   end Compare_Datasets;
   
   package Job_Sorter is new Datasets_Package.Generic_Sorting( "<" => Compare_Datasets ); 
   
   function Get_Datasets( ctl : in Run ) return Datasets_List is
   use Ada.Directories;
   use Ada.Text_IO;
   use Text_Utils;
   use Model.WSC.Run_Declarations;
   use Model.WSC.Run_Settings;

      l : Datasets_List;
      local_ctl : Run := Ctl.copy;
      
      procedure Find_Entry( directory_entry : Directory_Entry_Type ) is
         file  : File_Type;
         basename    : constant String := Base_Name( Simple_Name( directory_entry ));
         fullname    : constant String := Full_Name( directory_entry );
         r           : Datasets_Record;
      begin
         local_ctl.Set_Run_Id( basename );
         r.date := Modification_Time( fullname );
         r.directory := TuS( basename );
         r.name :=TuS( basename );
         r.settings := Read_Settings( fullname & "/wsc_run.txt" );
         l.Append( r );
      exception
         when error : others => 
            Log( "Model.WSC.Datasets failed to open " & basename & Exception_Information( error ));
      end Find_Entry;
      
   begin
      Log( "Model.WSC.Datasets: Past_Runs: searching dir |" & Model.WSC.Global_Settings.Physical_Root & "/created_datasets/" & "|" );
      if( not Exists( local_ctl.Qualified_Users_Directory ))then
         return l;
      end if;
      Search( Model.WSC.Global_Settings.Physical_Root & "/created_datasets/", "db_*", ( Directory => True, others => False ), Find_Entry'Access );
      Job_Sorter.Sort( l );
      return l;
      exception
         when Ada.IO_Exceptions.Name_Error => null; -- don't care if there's no such directory.
   end Get_Datasets;
   
 
end Model.WSC.Datasets;
