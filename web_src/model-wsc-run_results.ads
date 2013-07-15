with AWS.Session;
with Ada.Strings.Unbounded;
with Model.WSC.Run_Declarations;
with Model.Run_Settings;
with WSC_Enums;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Model.WSC.Parameters;
--
-- NOT USED NOT USED NOT USED
-- replaced by database routines
--
package Model.WSC.Run_Results is
   
   use Ada.Strings.Unbounded;
   use Model.WSC.Run_Declarations;
   use Model.Run_Settings;
   use WSC_Enums;
   use Model.WSC.Parameters;
   
  type Run_Results_Record is record
      date       : Ada.Calendar.Time;
      success    : Boolean;
      directory  : Unbounded_String;
      state      : State_Type;
      run_number : Unbounded_String;
      wsc_run : Run;
   end record;
   
   function To_String( r : Run_Results_Record ) return String;
   
   package Run_Results_Package is new Ada.Containers.Vectors( Index_Type=>Positive, Element_Type=>Run_Results_Record );
   subtype Run_Results_List is Run_Results_Package.Vector;
   
   function Get_Run_Settings_Used_For_Run( list : Run_Results_List; which : Integer ) return Run;
   function Get_Parameter_System_Used_For_Run( list : Run_Results_List; wsc_run : Run ) return Parameters_Array;
   
   NO_RUN_RESULTS : constant Run_Results_List := Run_Results_Package.Empty_Vector;
   function Get_Previous_Runs( ctl : in Run ) return Run_Results_List;
   function Last_Successful_Run( run_results : Run_Results_List ) return Natural;
   function To_Select_Options( l : Run_Results_List; currently_selected : Natural ) return String;

end Model.WSC.Run_Results;
