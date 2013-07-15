with Random_Normal_Draw;
with Ada.Text_IO;
with Base_Model_Types;

procedure Random_Tests is
   use Base_Model_Types;
   package A_Norm is new Random_Normal_Draw( Amount );
   use Ada.Text_IO;
   
   f : File_Type;
begin
   Create( f, Out_File, "/home/graham_s/gretl/1_000_000_random_normals_mean_0_sd_1.txt" );
   A_Norm.Reset;
   Put_Line( f, "random_normal_var_mean_0_sd_1" );
   for i in 1 .. 1_000_000 loop
      Put_Line( f, Amount'Image( A_Norm.Draw( 0.0, 1.0 ) ));
   end loop;
   Close( f );
end Random_Tests;
