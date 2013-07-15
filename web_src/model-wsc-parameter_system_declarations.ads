with Ada.Calendar;
with Ada.Strings.Unbounded;

with Parameter_System.Input_Buffer.WSC_Renderer;
with Parameter_System.Input_Buffer.Utils;
with Parameter_System.Input_Buffer;
with Parameter_System.XML;
with Parameter_System;

with Model.WSC.Parameters;

with WSC_Enums;
with Model.WSC.Formatting;
with Model.WSC.Run_Declarations;
with Keyed_Text_Buffer;

package Model.WSC.Parameter_System_Declarations is  

   use WSC_Enums;
   use Ada.Calendar;
   use Ada.Strings.Unbounded;
   use Model.WSC.Run_Declarations;

   YEAR_PREFIX : constant Unbounded_String := To_Unbounded_String( "y" );
   DELIMITER : constant Character := '.';
   
   
   --
   -- packages for the input buffer system
   --
   package WSC_Parameter_Editing_System is new Parameter_System( 
      Float_Type   => Amount, 
      Counter_Type => Counter_Type, 
      Languages    => Languages );
      
   package WSC_Parameter_Editing_System_XML is new WSC_Parameter_Editing_System.XML;
   
   --
   -- FIXME needing uprate here is really crappy design.
   --
   procedure Uprate( 
            c    : in out Amount; 
            mult : Amount; 
            rec  : WSC_Parameter_Editing_System.Parameter_Rec; 
            which_operation : Integer := 0 );
            
   package WSC_Parameter_Editing_System_IO is new WSC_Parameter_Editing_System.Input_Buffer(
      Uprate,
      Model.WSC.Formatting.Web_Format,
      Model.WSC.Formatting.Web_Format,
      Model.WSC.Formatting.Web_Format,
      Model.WSC.Formatting.Web_Validate,
      Model.WSC.Formatting.Web_Validate,
      Model.WSC.Formatting.Translations.Lookup
   );
      
   subtype WSC_Editing_Buffer is WSC_Parameter_Editing_System_IO.Buffer;
   
   package WSC_Utils is new WSC_Parameter_Editing_System_IO.Utils;
   package WSC_Renderer is new WSC_Parameter_Editing_System_IO.WSC_Renderer;
   function Get_WSC_Parameter_Editing_System return WSC_Parameter_Editing_System.Parameter_System_Rec;
   
   function Get_Default_Parameter_Keyed_Text_Buffer return Keyed_Text_Buffer.Text_Buffer; 
   -- function Get_Parameter_Keyed_Text_Buffer( wsc_run : Run ) return Keyed_Text_Buffer.Text_Buffer; 
   -- function Get_Run_Settings_Keyed_Text_Buffer( wsc_run : Run ) return Keyed_Text_Buffer.Text_Buffer;
   
   function Get_WSC_Run_Settings return WSC_Parameter_Editing_System.Parameter_System_Rec;
   function Get_Default_Model_Parameters( wsc_run : Run ) return Model.WSC.Parameters.Parameters_Array;
 

   
   function Get_Loaded_Input_Buffer( 
      wsc_run : Run;
      lang     : Languages := en ) return WSC_Editing_Buffer;
      
   function Get_Loaded_Run_Settings_Buffer( wsc_run : Run ) return WSC_Editing_Buffer;
      
   function Get_Null_WSC_Editing_Buffer return WSC_Editing_Buffer;   
   function Get_Comparison_Run(  wsc_run : Run ) return Run;
   
end Model.WSC.Parameter_System_Declarations;

