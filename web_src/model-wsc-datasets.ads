with AWS.Session;
with Ada.Strings.Unbounded;
with Model.WSC.Run_Declarations;
with Model.Run_Settings;
with WSC_Enums;
with Ada.Calendar;
with Ada.Containers.Vectors;
 
package Model.WSC.Datasets is
   
   use Ada.Strings.Unbounded;
   use Model.WSC.Run_Declarations;
   use Model.Run_Settings;
   use WSC_Enums;
   
  type Datasets_Record is record
      date       : Ada.Calendar.Time;
      name       : Unbounded_String;
      directory  : Unbounded_String;
      wsc_run : Run;
   end record;
   
   function To_String( r : Datasets_Record ) return String;
   
   package Datasets_Package is new Ada.Containers.Vectors( Index_Type=>Positive, Element_Type=>Datasets_Record );
   subtype Datasets_List is Datasets_Package.Vector;
   
   NO_DATASETS : constant Datasets_List := Datasets_Package.Empty_Vector;
   function Get_Datasets( ctl : in Run ) return Datasets_List;
   

end Model.WSC.Datasets;
