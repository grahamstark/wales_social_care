with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Model.WSC.Main_Menu;

package Breadcrumbs is

   use Ada.Strings.Unbounded;
   use Ada.Calendar;
   use Model.WSC.Main_Menu;

   type Trail_Type is tagged private;
   
   function Capacity( trail : Trail_Type ) return Positive;
   procedure Set_Capacity( trail : in out Trail_Type; n : Positive );

   function To_String( trail : Trail_Type ) return Unbounded_String;
   function Empty_Trail return Trail_Type;
   
   procedure Add( 
      trail   : in out Trail_Type;
      title   : String;
      url     : String;
      section : Section_Type;
      year    : Year_Number := Year_Number'First );
   
private  

   type One_Entry is record
      title   : Unbounded_String := Null_Unbounded_String;
      section : Section_Type     := Section_Type'First;
      url     : Unbounded_String := Null_Unbounded_String;
      year    : Year_Number      := Year_Number'First;
   end record;
   
   package Breadcrumb_Package is new Ada.Containers.Vectors( Element_Type => One_Entry, Index_Type => Positive );
   subtype Breadcrumb_List is Breadcrumb_Package.Vector;
   
   type Trail_Type is tagged record
      capacity : Positive := 5;
      entries  : Breadcrumb_List;
   end record;
   
end Breadcrumbs;
