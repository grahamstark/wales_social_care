--
-- Created by ada_generator.py on 2012-02-09 16:45:15.869409
-- 

with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body Base_Types is
   
   TRIM_DELETE  : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set( "@" & ASCII.NUL );
   TRIM_NOTHING  : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set( "" );
   

   function Slice_To_Unbounded( s : String; start : Positive; stop : Natural ) return Unbounded_String is
   begin
      if( stop /= 0 )then
         return To_Unbounded_String( Slice( To_Unbounded_String( s ), start, stop ) );
      else
         declare
            st : constant String := Ada.Strings.Fixed.Trim( s, TRIM_NOTHING, TRIM_DELETE );
         begin
            return To_Unbounded_String( st );
         end;
      end if;
   end Slice_To_Unbounded;

 

end Base_Types;
