--
--  $Author: graham_s $
--  $Date: 2011-04-06 15:56:14 +0100 (Wed, 06 Apr 2011) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );


with Maths_Functions;

with Model.WSC.BHPS_Data_Creation_Libs;

with BHPS.Binary_IO.Conversion_To_Binary;
with T_Utils;
with Text_Utils;
with Base_Model_Types;
with GNAT.Regpat;
with Ada.Text_IO;
with Indexed_Sequential_IO;
with Ada.Numerics.Discrete_Random;
with Model.WSC.Uprate;
with Maths_Functions.Weights_Generator;

package body WSC_Tests is

   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;
   use Ada.Text_IO;
   use BHPS.Print;
   use Base_Model_Types;
   
   package NMath is new Ada.Numerics.Generic_Elementary_Functions( Amount );
   use NMath;
   package MMath is new Maths_Functions( Amount );
   use MMath;   
   package WG is new MMath.Weights_Generator;

   subtype Obs_Number is Positive range 1 .. 20;
   subtype Constraint_Number is Positive range 1 .. 4;
   subtype Mat is Matrix( Constraint_Number'Range, Constraint_Number'Range );
   subtype Vec is Vector( Constraint_Number'Range );
   subtype Col is Vector( Obs_Number'Range );
   subtype OBS_T is Matrix( Obs_Number'Range, Constraint_Number'Range );
   obs : constant Obs_T := (      
   ( 1.0, 1.0, 0.0, 0.0 ),
   ( 0.0, 1.0, 0.0, 0.0 ),
   ( 1.0, 0.0, 2.0, 0.0 ),
   ( 0.0, 0.0, 6.0, 1.0 ),
   ( 1.0, 0.0, 4.0, 1.0 ),
   ( 1.0, 1.0, 0.0, 0.0 ),
   ( 1.0, 0.0, 5.0, 0.0 ),
   ( 0.0, 0.0, 6.0, 1.0 ),
   ( 0.0, 1.0, 0.0, 0.0 ),
   ( 0.0, 0.0, 3.0, 1.0 ),
   ( 1.0, 0.0, 2.0, 0.0 ),
   ( 1.0, 1.0, 0.0, 1.0 ),
   ( 1.0, 0.0, 3.0, 1.0 ),
   ( 1.0, 0.0, 4.0, 0.0 ),
   ( 0.0, 0.0, 5.0, 0.0 ),
   ( 0.0, 1.0, 0.0, 1.0 ),
   ( 1.0, 0.0, 2.0, 1.0 ),
   ( 0.0, 0.0, 6.0, 0.0 ),
   ( 1.0, 0.0, 4.0, 1.0 ),
   ( 0.0, 1.0, 0.0, 0.0 ));

   initial_weights : constant Col := ( 
      3.0,
      3.0,
      5.0,
      4.0,
      2.0,
      5.0,
      5.0,
      4.0,
      3.0,
      3.0,
      5.0,
      4.0,
      4.0,
      3.0,
      5.0,
      3.0,
      4.0,
      5.0,
      4.0,
      3.0 );
      
   actual_final_weights : constant Col := ( 
      2.753,
      2.109,
      5.945,
      4.005,
      2.484,
      4.589,
      5.752,
      4.005,
      2.109,
      3.120,
      5.945,
      3.985,
      5.019,
      3.490,
      4.678,
      2.345,
      5.070,
      4.614,
      4.967,
      2.109 );

   target_populations : constant Vec := ( 50.0, 20.0, 230.0, 35.0 );
   
   
   procedure Test_Calmar_Iterative( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- reproduces the basic test case in Creedy NEW ZEALAND TREASURY WORKING PAPER 03/17
      -- using the package
      -- 
      use type WG.Distance_Function_Type;
      type CA is array( 1 .. 10 ) of Col;
      weights        : CA := ( others => ( others => 0.0 ));
      r_u            : Amount;
      r_l            : Amount;
      tol_x          : constant Amount := 0.001;
      tol_f          : constant Amount := 0.001;
      max_iterations : constant := 30;
      iterations     : Natural;
      error          : Eval_Error_Type;
      c, n           : Natural := 1;
    begin
       for it in WG.Distance_Function_Type loop
          n := 1;          
          if( it = WG.constrained_chi_square ) or it = WG.d_and_s_constrained then
             n := 3;
          end if;
          for i in 1 .. n loop
             if( it = WG.constrained_chi_square )then
                if( i = 1 ) then
                   r_u := 3.0;
                   r_l := 0.2;
                elsif( i = 2 )then
                   r_u := 1.3;
                   r_l := 0.8;
                else
                   r_u := 1.25;
                   r_l := 0.8;
                end if;
             elsif( it = WG.d_and_s_constrained )then
                if( i = 1 ) then
                   r_u := 4.0;
                   r_l := 0.2;
                elsif( i = 2 )then
                   r_u := 5.0;
                   r_l := 0.2;
                else
                   r_u := 5.0;
                   r_l := 0.2;
                end if;
             end if;
             WG.Do_Reweighting(
               obs, 
               it,
               initial_weights,
               target_populations,
               tol_x,
               tol_f,
               max_iterations,
               r_u,
               r_l,
               weights( c ),
               iterations,
               error );   
            Put_Line( "CALLED FOM WG :: " & it'Img & " col = " & c'Img );
            Put_Line( "ERROR " & error'Img & " iterations " & iterations'Img );
            c := c + 1;
         end loop;
      end loop;
      for k in Obs_Number'Range loop
         Put( k'Img & "," & initial_weights( k )'Img & ",");
         for c in 1..10 loop
            Put( weights( c )( k )'Img & "," );
         end loop;
         New_Line;
      end loop;
   end Test_Calmar_Iterative; 
   
[ ... ] 

end WSC_Tests;
