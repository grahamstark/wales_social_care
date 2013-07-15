with Text_Utils;
with Web_Utils;
with Templates_Parser;
with Ada.Text_IO;
with Ada.Command_Line;

procedure Templates_Test_Driver is

use Templates_Parser;

   translations : Translate_Set;
   labels  : Vector_Tag;
   values  : Vector_Tag;
   pres    : Vector_Tag;
   posts   : Vector_Tag;
   diffs   : Vector_Tag;
begin
   for i in 1 .. 100 loop
      labels := labels &  i'Img;
      values := values &  i'Img;
      pres := pres & i'Img;
      if( i mod 10 ) /= 0 then
         posts := posts & i'Img;
         diffs := diffs & "0";
      else
         posts := posts & "2000.0";
         diffs := diffs & "1000.0";
      end if;
   end loop;
   
   Insert( translations, Assoc( "LABEL", labels ));
   Insert( translations, Assoc( "VALUE", values ));
   Insert( translations, Assoc( "PRE", pres ));
   Insert( translations, Assoc( "POST", posts ));
   Insert( translations, Assoc( "DIFF", diffs ));
   declare
      table : constant String :=
         Web_Utils.Parse_Template( Ada.Command_Line.Argument( 1 ), translations );  
         final_translations : Translate_Set;
   begin
      Insert( final_translations, Assoc( "TABLE", table ));
      Ada.Text_IO.Put_Line( Web_Utils.Parse_Template( "web/wsc/templates/enclosure", final_translations ));  
   end;
end Templates_Test_Driver;

