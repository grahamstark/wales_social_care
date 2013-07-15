with Ada.Text_IO;
with Ada.Assertions;
with Base_Model_Types;
with List_Of_Randoms;

procedure Random_List_Generator is
use Ada.Text_IO;
use Ada.Assertions;
use Base_Model_Types;
 
   package M_Randoms is new List_Of_Randoms(
   Real => Rate,
   Capacity => 20_000 );

   r : M_Randoms.Random_List;
   
begin

   for i in 1 .. 300 loop
      r.Reset;
      r.Store( "tmp/randoms_" & i'Img( 2 .. i'Img'Length ) & ".txt" );
   end loop;
   
end  Random_List_Generator;   

