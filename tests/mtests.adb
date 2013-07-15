with Ada.Numerics.Generic_Real_Arrays;

procedure MTests is
  
   package Matrix_Functions is new Ada.Numerics.Generic_Real_Arrays( Real => Long_Float );
       
   use Matrix_Functions; 
   n_obs : constant := 20;
   n_k   : constant := 4;
   subtype Mat is Real_Matrix( 1 .. n_k, 1 .. n_k );
   subtype Vec is Real_Vector( 1 .. n_k );
   
   v1 , v2 : Vec;
   m : Mat;
begin
   m := v1 * v2;
end MTests;
