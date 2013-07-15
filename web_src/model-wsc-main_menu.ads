with Ada.Strings.Unbounded;
with Model.WSC.Formatting;
with WSC_Enums;

package Model.WSC.Main_Menu is
  
  use Ada.Strings.Unbounded;
  use Model.WSC.Formatting;
  use WSC_Enums;
  
  type Section_Type is ( home_page, parameters_page, run_settings_page, output_page, dump_params_page );
  
  function Get_Main_Menu( 
        which_page      : Section_Type;
        output_disabled : Boolean;
        lang            : Languages ) return Unbounded_String; 

end Model.WSC.Main_Menu; 
