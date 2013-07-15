--
--  $Author: graham_s $
--  $Date: 2011-04-06 15:56:14 +0100 (Wed, 06 Apr 2011) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );

with Ada.Command_Line;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with AUnit.Assertions;
   
with Maths_Functions;
with Base_Model_Types;

with Text_Utils;

package body Maths_And_Probit_Tests is

   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;
   use Ada.Text_IO;
   use Text_Utils;
   use Base_Model_Types;
   
   package Maths_Funcs is new Maths_Functions( Amount );
   
   -- log_file : File_Type;

   procedure Set_Up (T : in out Test_Case) is
   begin
      -- 
      Maths_Funcs.Do_Debug( True );
      -- Open( log_file, Out_File, "tmp/dump.csv" );
   end Set_Up;

   procedure Shut_Down( T : in out Test_Case) is
   begin
      -- Close( log_file );
      null;
   end Shut_Down;
   
   
   function L_Evaluate_Probit( 
      v                    : Real; 
      add_random_component : Boolean := False; 
      threshold            : Real := 0.5 ) return Boolean is
      local_thresh : Real  := threshold;
      prob         : Real;
      err          : Real := 0.0;
   begin
      if( add_random_component )then
         err := Maths_Funcs.Random_Normal_Generator.Draw( mean => 0.0, standard_deviation => 1.0 );
      end if;
      prob := Maths_Funcs.Cumulative_Normal( v + err );
      return prob >= local_thresh;
   end L_Evaluate_Probit;

   
   procedure Probits_Tests( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      n : constant Positive := 10_000;
      m : Rate;
      c : Rate := 0.0;
      c2 : Rate;
      c3 : Rate;
      v : Rate := 0.0;
      scale : Rate := 0.0;
      ok : Boolean;
      thresh : Rate := 0.0;
   begin
      for i in 1 .. n loop
         m := Maths_Funcs.Random_0_To_1;
         if( m > 0.5 )then
            c := c + 1.0;
         end if;
      end loop;
      c := c / Rate( n );
      Put_Line( "propn > 0.5; " & c'Img );
      
      scale := 0.0;
      for j in 1 .. 30 loop
         c := 0.0;
         c2 := 0.0;
         c3 := 0.0;
         for i in 1 .. n loop
            ok := Maths_Funcs.Evaluate_Probit( v, True );
            if( ok )then
               c := c + 1.0;
            end if;
            ok := L_Evaluate_Probit( v, True, thresh );
            if( ok )then
               c2 := c2 + 1.0;
            end if;
            ok := Maths_Funcs.Evaluate_Probit( v, thresh, True );
            if( ok )then
               c3 := c3 + 1.0;
            end if;
         end loop;
         c := c / Rate( n );
         c2 := c2 / Rate( n );
         c3 := c3 / Rate( n );
         Put_Line( "Scale = " & scale'Img & " c = " & c'Img & " c2 " & c2'Img & " c3 " & c3'Img );
         thresh := thresh + 0.1;
         scale := scale + 0.1;
      end loop;
   end Probits_Tests;
   
   
   

   --------------------
   -- Register_Tests --
   --------------------
   procedure Register_Tests( t : in out Test_Case ) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine( t, Probits_Tests'Access, "Probits_Tests" );
   end Register_Tests;

   ----------
   -- Name --
   ----------
   function Name ( t : Test_Case ) return Message_String is
   begin
      return Format("Maths_And_Probit_Tests");
   end Name;

end Maths_And_Probit_Tests;
