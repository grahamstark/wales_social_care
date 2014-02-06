with AWS.Session;
with Ada.Strings.Unbounded;
with Model.WSC.Run_Declarations;
with Model.WSC.Run_Settings;
with WSC_Enums;
with Ada.Calendar;
with Ada.Containers.Vectors;

package Model.WSC.Dynamic_Driver.Web_Runner is

   use Ada.Strings.Unbounded;
   use Model.WSC.Run_Declarations;
   use Model.WSC.Run_Settings;
   use WSC_Enums;
    
   function Get_State_Of_Run_As_HTML( wsc_run : Run; run_state : State_Type ) return Unbounded_String;
   
   procedure Submit_Run( 
      session_id   : AWS.Session.Id; 
      wsc_run  : Run ;
      params       : Parameters_Array );
   function Get_New_Run_Id return String;

   
end Model.WSC.Dynamic_Driver.Web_Runner;
