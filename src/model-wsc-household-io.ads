with Ada.Containers.Ordered_Maps;
with Ada.Direct_IO;
--
-- NO LONGER USED
--
package Model.WSC.Household.IO is
   

   type Person_Pointer is record
      pid             : Sernum_Value;
      buno            : Benefit_Unit_Number;
      adno            : Adult_Count := 0;
      chno            : Child_Count := 0;
      hdsp            : Head_Or_Spouse := Neither;
      record_location : Positive;
   end record;
   
   -- needed: list of every household in wales
   -- map pid, wave -> pos
   
   type Person_Pointer_Array is array ( Person_Number ) of Person_Pointer;

   type Household_Skel is record
      num_people      : Person_Count;
      people          : Person_Pointer_Array;
      record_location : Positive;
   end record;
   
   subtype Household_Number is Positive range 1 .. 6_000;
   
   type Household_Skel_Array is array( Waves, Household_Number ) of Household_Skel; 
   
   package Person_IO is new Ada.Direct_IO( Person );
   package HH_Data_IO is new Ada.Direct_IO( Household_Data );
   package HH_Skel_IO is new Ada.Direct_IO( Household_Skel_Array );
   
end Model.WSC.Household.IO;
