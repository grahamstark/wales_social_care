with Text_Utils;
with Model.WSC.Global_Settings;

package body Model.WSC.Main_Menu is
   use Text_Utils;
   use Translations;
   
  function Get_Main_Menu( 
        which_page      : Section_Type;
        output_disabled : Boolean;
        lang            : Languages ) return Unbounded_String is
        s               : Unbounded_String;
        root            : constant String := Model.WSC.Global_Settings.WSC_Web_Root;
   begin
       s := s & "<div id='modelNavTop'>" & LINE_BREAK;
       s := s & "  <ul>" & LINE_BREAK; 
       for b in Section_Type loop
         declare
            k : String := Censor_String( Section_Type'Image( b ));
            text : String := Lookup( k, lang );
         begin
            -- FIXME this has 'wsc' wired in instead of 'server_root' from config
            if( b /= which_page )then
               if ( b = output_page and output_disabled )then
                  s := s & "      <li id='" & k & "_link' >" & text & "</li>" & LINE_BREAK;
               elsif( b /= home_page )then
                  s := s & "      <li><a href='" & root & k & "/' id='" & k & "_link' " & ">" & text & "</a></li>" & LINE_BREAK;
               else
                  s := s & "      <li><a href='" & root & "' " & " id='" & k & "_link' >" & text & "</a></li>" & LINE_BREAK;
               end if;
            else
               s := s & "      <li class='on' id='" & k & "_link' >" & text & "</li>" & LINE_BREAK;
            end if;
         end;
       end loop;
       s := s & "   </ul>"& LINE_BREAK;
       s := s & "</div>"& LINE_BREAK;
       return s;  
   end Get_Main_Menu;

end Model.WSC.Main_Menu; 
