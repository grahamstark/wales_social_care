with Text_Utils;
with Ada.Containers;

package body Breadcrumbs is

   use Text_Utils;
   use Ada.Containers;
   
   function Empty_Trail return Trail_Type is
      t : Trail_Type;
   begin
      return t;
   end Empty_Trail;
   
   function Capacity( trail : Trail_Type ) return Positive is
   begin
      return trail.capacity;
   end Capacity;
   
   procedure Set_Capacity( trail : in out Trail_Type; n : Positive )is
   begin
      trail.capacity := n;
   end Set_Capacity;

   function To_String( trail : Trail_Type ) return Unbounded_String is
      use Breadcrumb_Package;
      s : Unbounded_String;
      p : Count_Type := 0;
      
      procedure Draw_One( c : Cursor ) is
         o : One_Entry := Element( c );
      begin
         p := p + 1;
         s := s & "<a href='" & o.url & "'>";
         s := s & o.title;
         if( o.year /= Year_Number'First )then
            s := s & "[" & o.year'Img & "]";
         end if;
         s := s & "</a>";
         if( p < trail.Entries.Length )then
            s := s & "&nbsp;&bull;&nbsp;";
         end if;
      end Draw_One;
      
   begin
      s := s & "<div class='breadcrumb'>Recent Pages:&nbsp;";
      trail.entries.Reverse_Iterate( Draw_One'Access );
      s := s & "</div>";
      return s;
   end To_String;
   
   procedure Add( 
      trail   : in out Trail_Type;
      title   : String;
      url     : String;
      section : Section_Type;
      year    : Year_Number := Year_Number'First ) is
      o : One_Entry;
   begin
      o.url := TuS( url );
      if trail.entries.length > 0 and then trail.entries.Last_Element.url = o.url then
         return; -- not going straight back where we are currently
      end if;
      o.title := TuS( title );
      o.section := section;
      o.year := year;
      if( trail.capacity <= Natural( trail.entries.Length ))then
         trail.entries.Delete( 1 );
      end if;
      trail.entries.Append( o );
   end Add;

end Breadcrumbs;
