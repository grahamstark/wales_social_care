with Ada.Containers.Ordered_Maps;
with Ada.Text_IO;
with Model.WSC.Run_Declarations;
with Model.WSC.Parameters;

package Model.WSC.Results.IO is

   use Model.WSC.Run_Declarations;
   use Model.WSC.Parameters;
-- function Compare_UID_Lt( left, right: Sernum_Value ) return Boolean;
 
   package Individual_Results_Package is new Ada.Containers.Ordered_Maps(
      Key_Type     => Sernum_Value,
      Element_Type => Personal_Result,
      "="          => Compare_Results_Equal,
      "<"          => "<" );
   
   subtype Individual_Results_Map is Individual_Results_Package.Map;
   subtype Individual_Results_Cursor is Individual_Results_Package.Cursor;

   type Individual_Results_Map_Array is array( Simulation_Years ) of Individual_Results_Map;
   
   procedure Dump( filename : String; map : Individual_Results_Map );
   procedure Restore( filename : String; map : out Individual_Results_Map );
     
end Model.WSC.Results.IO;
