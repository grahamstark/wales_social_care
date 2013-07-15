with Ada.Strings.Unbounded;
with Templates_Parser;
with Text_Utils;
with Model.WSC.Users;
generic

package Parameter_System.Input_Buffer.WSC_Renderer is
   
   use Templates_Parser;
   use Text_Utils;
   
   procedure Create_HTML_Inputs( 
      html_cells       : in out Templates_Parser.Vector_Tag;
      buff             : Buffer;
      base_sys         : Parameter_System_Rec; 
      sys              : Parameter_System_Rec; 
      parameter_prefix : Unbounded_String;
      print_static_only : Boolean := False; 
      print_all         : Boolean := False );
      
   type HTML_Kind is ( html5, xhtml1 );
   
   type Ajax_Action_Type is ( 
      insert_above, 
      insert_below, 
      delete, 
      save, 
      copy,
      error_check );

   procedure Set_HTML_Type( t : HTML_Kind );

   function Create_Static_Page(
      buff                : Buffer;
      sys                 : Parameter_System_Rec; 
      parameter_prefix   : Unbounded_String ) return Unbounded_String;
  
   --
   -- FIXME private!
   --
   function Make_Validator_Code( 
      key : Unbounded_String; 
      param : Parameter_Rec ) return Unbounded_String;
   
   --
   -- testing only!
   --
   function Make_Ajax_Call_Indexed( 
      action          : Ajax_Action_Type;
      key             : Unbounded_String;
      row             : Natural;
      lang            : Languages;
      ajax_target_key : Unbounded_String) return Unbounded_String;

         
   function Make_Parameter_Menu(
      start_year           : Year_Number;
      end_year             : Year_Number;
      this_year            : Year_Number;
      base_sys             : Parameter_System_Rec; 
      current_path  : Unbounded_String_List;
      prefix               : Unbounded_String;      
      lang                 : Languages;
      user                 : Model.WSC.Users.User_Type ) return Unbounded_String;   
      
   function Make_Indexed_Block(
      complete_sys      : Parameter_System_Rec;
      key               : Unbounded_String;
      buff              : Buffer;
      ajax_target_key   : Unbounded_String;
      print_static_only : Boolean := False; 
      print_all         : Boolean := False  ) return Unbounded_String;
      
   function Create_Complete_Translations( 
      title               : Unbounded_String;
      buff                : Buffer;
      model_menu          : Unbounded_String;
      breadcrumb          : Unbounded_String;
      base_sys            : Parameter_System_Rec; 
      sys                 : Parameter_System_Rec; 
      parameter_prefix    : Unbounded_String;
      main_error_message  : Unbounded_String;
      job_is_running      : Boolean;
      user                :Model.WSC.Users.User_Type;
      extra_translations  : Templates_Parser.Translate_Set ) return Translate_Set;
      
   function Create_Input_Page(
      title               : Unbounded_String;
      buff                : Buffer;
      model_menu          : Unbounded_String;
      breadcrumb          : Unbounded_String;
      base_sys            : Parameter_System_Rec; 
      sys                 : Parameter_System_Rec;
      parameter_prefix    : Unbounded_String;
      main_error_message  : Unbounded_String;
      job_is_running      : Boolean;
      user                :Model.WSC.Users.User_Type;
      extra_translations  : Templates_Parser.Translate_Set ) return Unbounded_String;
   
end Parameter_System.Input_Buffer.WSC_Renderer;
