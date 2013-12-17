--
--  $Author: graham_s $
--  $Date: 2011-04-06 15:56:14 +0100 (Wed, 06 Apr 2011) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );

with AUnit.Assertions;   
with Ada.Direct_IO;
with Ada.Command_Line;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Strings.Unbounded;
with Ada.Calendar;

with BHPS;
with BHPS_Enums;
with BHPS.Print;
with BHPS.Binary_IO;
with BHPS.XLookup;
with BHPS_Indexes;
with BHPS.State_Changes;

with Submit_Actions;

with Model.WSC;
with WSC_Enums;
with Model.WSC.Household;
with Model.WSC.Household.IO;
with Model.WSC.Household.Regressions;
with Model.WSC.Household.Database;
with Model.WSC.Household.Transitions;
with Model.WSC.Household.Transitions_Basic;
with Model.WSC.Global_Settings;
with Model.WSC.Run_Declarations;
with Model.WSC.Gain_Lose;
with Model.WSC.Inequality;
with Model.WSC.Budget;
with Model.WSC.Output.Budget_Web_IO;
with Model.WSC.Output.Gain_Lose_Web_IO;
with Model.WSC.Output.Web_IO;

with Line_Extractor;

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
with Text_Utils;
with Weighting_Commons;

package body WSC_Tests is
   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;
   use Ada.Text_IO;
   use BHPS.Print;
   use Base_Model_Types;
   use Text_Utils;
   
   package NMath is new Ada.Numerics.Generic_Elementary_Functions( Amount );
   use NMath;
   package MMath is new Maths_Functions( Amount );
   use MMath;   
   package WG is new MMath.Weights_Generator(    
      Num_Constraints => 4,
      Num_Observations => 20 );
   
   obs : constant WG.Dataset := (      
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

   initial_weights : constant WG.Col_Vector := ( 
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
      
   actual_final_weights : constant WG.Col_Vector := ( 
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

   target_populations : constant WG.Row_Vector := ( 50.0, 20.0, 230.0, 35.0 );
   
   package By_Region is new T_Utils( 
      T=>BHPS_Enums.Region2_Type, 
      Rate_Type=>Base_Model_Types.Rate, 
      Amount_Type=>Base_Model_Types.Amount, 
      Counter_Type=>Base_Model_Types.Counter_Type );
   
   default_settings : Model.WSC.Run_Declarations.Run;
   
   
   
   procedure Set_Up (T : in out Test_Case) is
   begin
      default_settings := Model.WSC.Run_Declarations.Read_Settings( Ada.Command_Line.Argument( 1 ));
      Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 2 ));
      -- FIXME initialise logger
   end Set_Up;
   
      
   procedure Shut_Down( T : in out Test_Case) is
   begin
      null;
   end Shut_Down;

   -- procedure Test_Create_Hessians( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      -- hessian   : Matrix( Constraint_Number'Range, Constraint_Number'Range );
      -- lambdas   : Vec;
      -- d_lam     : Amount := -1.0;
      -- f_lambdas : Vec;
   -- begin
      -- for v in 1 .. 50 loop
         -- d_lam := d_lam + 0.1;
         -- lambdas := ( others => d_lam );
         -- Put_Line( "LAMBDA " & d_lam'Img );
         -- for t in Distance_Function_Type loop
            -- hessian := Create_Hessian( lambdas, t, 0.4, 2.0 );
            -- Put_Line( t'Img );
            -- Put_Line( To_String( hessian ));
         -- end loop;
         -- Put_Line( "LIBRARY VERSION " );
         -- for t in WG.Distance_Function_Type loop
               -- --
            -- WG.Evaluate_Function_And_Hessian( 
               -- data               => obs,
               -- lambdas            => lambdas,
               -- which_function     => t,
               -- initial_weights    => initial_weights,
               -- target_populations => target_populations,
               -- ru                 => 0.4,
               -- rl                 => 1.8,
               -- f_lambdas          => f_lambdas,
               -- hessian            => hessian );
            -- Put_Line( t'Img );
            -- Put_Line( To_String( hessian ));
            -- Put_Line( To_String( f_lambdas ));
         -- end loop;
      -- end loop;
   -- end Test_Create_Hessians;
   procedure Test_Year_Keys( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Line_Extractor;
   use Ada.Calendar;
      year_prefix     : Unbounded_String;
      has_year        : Boolean;
      year            : Year_Number;
      base_key        : Unbounded_String;
      is_indexed_key  : Boolean;
      index           : Natural;
      postfix         : Unbounded_String;       
      k1 : String := Make_Key_With_Year( "system", 2010, "incsupp", 22, "some_var" );
      k2 : String := Make_Key_With_Year( "systemx", 2012, "incsuppx" );
      k3 : String := "HELLO";
   begin
      Put_Line( "made k1 as |" & k1 & "| " );
      Parse_By_Year_Indexed_Key( 
         key             => TuS( k1 ),
         year_prefix     => year_prefix,
         has_year        => has_year,
         year            => year,
         base_key        => base_key,
         is_indexed_key  => is_indexed_key,
         index           => index,
         postfix         => postfix );  
      Assert( year_prefix = TuS( "system" ), "year prefix was " & TS( year_prefix ));
      Assert( year = 2010, " year was " & year'Img );
      Assert( has_year, " has year " & has_year'Img );
      Assert( base_key = TuS( "incsupp" ), "base key " & TS( base_key ));
      Assert( is_indexed_key, " is_indexed_key " & is_indexed_key'Img );
      Assert( index = 22, " index " & index'Img );
      Assert( postfix = TuS( "some_var" ), " postfix " & TS( postfix ));
      Put_Line( "made k2 as |" & k2 & "| " );
      Parse_By_Year_Indexed_Key( 
         key             => TuS( k2 ),
         year_prefix     => year_prefix,
         has_year        => has_year,
         year            => year,
         base_key        => base_key,
         is_indexed_key  => is_indexed_key,
         index           => index,
         postfix         => postfix );  
      Assert( year_prefix = TuS( "systemx" ), "year prefix was " & TS( year_prefix ));
      Assert( year = 2012, " year was " & year'Img );
      Assert( has_year, " has year " & has_year'Img );
      Assert( base_key = TuS( "incsuppx" ), "base key " & TS( base_key ));
      Assert( NOT is_indexed_key, " is_indexed_key " & is_indexed_key'Img );
      Assert( index = 0, " index " & index'Img );
      Assert( postfix = Null_Unbounded_String, " postfix " & TS( postfix ));
      Parse_By_Year_Indexed_Key( 
         key             => TuS( k3 ),
         year_prefix     => year_prefix,
         has_year        => has_year,
         year            => year,
         base_key        => base_key,
         is_indexed_key  => is_indexed_key,
         index           => index,
         postfix         => postfix );  
      Assert( year_prefix = Null_Unbounded_String, "year prefix was " & TS( year_prefix ));
      Assert( year = Year_Number'First, " year was " & year'Img );
      Assert( NOT has_year, " has year " & has_year'Img );
      Assert( base_key = TuS( "HELLO" ), "base key " & TS( base_key ));
      Assert( NOT is_indexed_key, " is_indexed_key " & is_indexed_key'Img );
      Assert( index = 0, " index " & index'Img );
      Assert( postfix = Null_Unbounded_String, " postfix " & TS( postfix ));
      
   end Test_Year_Keys;
   
   procedure Test_Calmar_2( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- reproduces the basic test case in Creedy NEW ZEALAND TREASURY WORKING PAPER 03/17
      -- using the package
      -- 
     
      weights : WG.Col_Vector;
      
    begin
      weights := WG.Do_Basic_Reweighting(
         obs, 
         initial_weights,
         target_populations );   
      Put_Line( "CALLED FOM MATHS_FUNCTIONS" );
      for k in WG.Col_Range loop
         Put_Line( k'Img & " " & weights( k )'Img & actual_final_weights( k )'Img );
      end loop;
   end Test_Calmar_2; 
   
   procedure Test_Calmar_Iterative( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- reproduces the basic test case in Creedy NEW ZEALAND TREASURY WORKING PAPER 03/17
      -- using the package
      -- 
      use type Weighting_Commons.Distance_Function_Type;
      type CA is array( 1 .. 10 ) of WG.Col_Vector;
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
       for it in Weighting_Commons.Distance_Function_Type loop
          n := 1;          
          if( it = Weighting_Commons.constrained_chi_square ) or it = Weighting_Commons.d_and_s_constrained then
             n := 3;
          end if;
          for i in 1 .. n loop
             if( it = Weighting_Commons.constrained_chi_square )then
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
             elsif( it = Weighting_Commons.d_and_s_constrained )then
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
      for k in WG.Col_Range loop
         Put( k'Img & "," & initial_weights( k )'Img & ",");
         for c in 1..10 loop
            Put( weights( c )( k )'Img & "," );
         end loop;
         New_Line;
      end loop;
   end Test_Calmar_Iterative; 
   
   generic
      vsize : Integer;
   package B is
      
      subtype VT is Vector( 1 .. vsize );
   end B;
   
   procedure Test_Package_Dyn( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   begin
      for i in 1 .. 10 loop
         declare
            package P is new B( i );
            use P;
            v : VT;
         begin
            for j in 1 .. i loop
               v( i ) := 1.0;
            end loop;
         end;
      end loop;
   end Test_Package_Dyn;
   
   --
   -- function is: 
   --  x = 10 - y**2             
   --  y = 2 + x
   --  hence hessian is 
   --  0  -2y
   --  1   0
   procedure Simple_Func(
          inputs               : in  Vector;
          hessian              : out Matrix;
          beta                 : out Vector ) is
   begin
      hessian( 1, 1 ) := 0.0;
      hessian( 1, 2 ) := -2.0 * inputs( 2 );
      hessian( 2, 1 ) := 1.0;
      hessian( 2, 2 ) := 0.0;
      beta( 1 ) :=( inputs( 1 ) - ( 10.0 - inputs( 2 ) ** 2 ));
      beta( 2 ) :=( inputs( 2 ) - ( 2.0 + inputs( 1 )));
   end Simple_Func;

   procedure Test_Eq_Finder( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      procedure Solve is new Solve_Non_Linear_Equation_System( Evaluate => Simple_Func );
      beta   : Vector( 1 .. 2 );
      inputs : Vector( 1 .. 2 );
      error  : Eval_Error_Type;
      hessian : Matrix( 1 .. 2, 1 .. 2 );
      iterations : Natural;
   begin
      inputs := ( 1.0, 3.0 );
      Simple_Func( inputs, hessian, beta );
      Put_Line( "BETA " );Put_Line( To_String( beta ));
      Put_Line( "HESSIAN " );Put_Line( To_String( hessian ));
      inputs := ( 10.0, 10.0 );
      Solve( inputs, 500, 0.00001, 0.00001, iterations, error );
      Put_Line( To_String( inputs ));
      Put_Line( "ERROR WAS " & error'Img & " iterations " & iterations'Img );
   end Test_Eq_Finder;
   
   procedure Test_HH_Database( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Model.WSC.Household.Database;
      use Model.WSC.Household;
      use WSC_Enums;
      use Base_Model_Types;
      
      sernums  : Sernum_List;
      db       : DB_Type;
      hh       : Household;
      wave     : Character := 'c';
   type Populations is array( Waves ) of Amount;
   
      hh_count        : Populations := ( others => 0.0 );
      pers_count      : Populations := ( others => 0.0 );
      adult_count     : Populations := ( others => 0.0 );
      child_count     : Populations := ( others => 0.0 );
      raw_hh_count    : Populations := ( others => 0.0 );
      raw_pers_count  : Populations := ( others => 0.0 );
      raw_adult_count : Populations := ( others => 0.0 );
      raw_child_count : Populations := ( others => 0.0 );
      weight          : Amount;
   begin
      Open( db, "data/wales",  actual, 1 );
      Create( db, "working/users/test_user/data/", estimated, 1 );
      for wave in Waves_With_Data loop
         db.Get_Household_Sernums( wave, sernums );
         for hhno in 1 .. sernums.Length loop
            hh := db.Get_Household( wave, sernums.Element( Positive( hhno )));
            weight := hh.hdata.weights( extended_2 );
            hh_count( wave ) := hh_count( wave ) + weight;
            raw_hh_count( wave ) := raw_hh_count( wave ) + 1.0;
            Put_Line( "hh.num_benefit_units " & hh.num_benefit_units'Img );
            for buno in 1 .. hh.num_benefit_units loop
               Put_Line( "hh.benefit_units( " & buno'Img & " ).num_adults " & hh.benefit_units( buno ).num_adults'Img );
               Put_Line( "hh.benefit_units( " & buno'Img & " ).num_children " & hh.benefit_units( buno ).num_children'Img );
               for adno in 1 .. hh.benefit_units( buno ).num_adults loop
                  pers_count( wave ) :=  pers_count( wave ) + hh.benefit_units( buno ).adults( adno ).respondent_weights( extended_2 );                 
                  adult_count( wave ) :=  adult_count( wave ) + hh.benefit_units( buno ).adults( adno ).respondent_weights( extended_2 );                 
                  raw_pers_count( wave ) :=  raw_pers_count( wave ) + 1.0;                 
                  raw_adult_count( wave ) :=  raw_adult_count( wave ) + 1.0;                 
                  Put_Line( "adult weight " & hh.benefit_units( buno ).adults( adno ).respondent_weights( extended_2 )'Img );
               end loop;
               for chno in 1 .. hh.benefit_units( buno ).num_children loop
                  pers_count( wave ) :=  pers_count( wave ) + hh.benefit_units( buno ).children( chno ).respondent_weights( extended_2 );                 
                  child_count( wave ) :=  child_count( wave ) + hh.benefit_units( buno ).children( chno ).respondent_weights( extended_2 );                 
                  raw_pers_count( wave ) :=  raw_pers_count( wave ) + 1.0;                 
                  raw_child_count( wave ) :=  raw_child_count( wave ) + 1.0;                 
                  Put_Line( "child weight " & hh.benefit_units( buno ).children( chno ).respondent_weights( extended_2 )'Img );
               end loop;
            end loop;
            Put_Line( "on hh " & hh.hid'Img & " wave " & Waves'Image( wave ));
         end loop;
      end loop;
      db.Close;
      Put_Line( "wave,hh_count,pers_count,adult_count,child_count" );
      for wave in Waves_With_Data loop
         Put_Line( Waves'Image( wave ) & "," &
                   hh_count( wave )'Img & "," &
                   pers_count( wave )'Img & "," &
                   adult_count( wave )'Img & "," &
                   child_count( wave )'Img );
      end loop;
      Put_Line( "wave,hh_count,pers_count,adult_count,child_count" );
      for wave in Waves_With_Data loop
         Put_Line( Waves'Image( wave ) & "," &
                   raw_hh_count( wave )'Img & "," &
                   raw_pers_count( wave )'Img & "," &
                   raw_adult_count( wave )'Img & "," &
                   raw_child_count( wave )'Img );
      end loop;
   end Test_HH_Database; 
    
   procedure Test_File_Pos( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      type String10 is array( 1 .. 10 ) of Character;
      package Test_IO is new Ada.Direct_IO( String10 );
      p  : Test_IO.Positive_Count;
      sz : Test_IO.Count;
      f  : Test_IO.File_Type;
      s  : String10  := ( others=>' ' );
   begin
      Test_IO.Create( f, Test_IO.Out_File, "tmp/some_string_test" );
      p := Test_IO.Index( f );
      sz := Test_IO.Size( f );
      Put_Line( "before write: size " & sz'Img & " pos " & p'Img );
      Test_IO.Write( f, s );
      p := Test_IO.Index( f );
      sz := Test_IO.Size( f );
      Put_Line( "after write: size " & sz'Img & " pos " & p'Img );
      Test_IO.Close( f );      
   end Test_File_Pos;

   procedure Test_BHPS_Read_People( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use BHPS; 
   use BHPS.Binary_IO;
   use BHPS.Print;
   use BHPS_Indexes;
   use BHPS_Index_Package;
   use BHPS_Map_Package;
   use Base_Model_Types;
   
      ptrs                 : Record_Pointers;
      total_num_people     : Natural := 0;
      wave                 : Waves;
      weighted_population  : Amount := 0.0;
   
      procedure Load_Adult( c : Index_Cursor ) is
         index : Index_Rec := Element( c );
         num_people : Base_Model_Types.Person_Count := 0;
         ad         : Adult := Load_Adult( index, ptrs, wave );
         weight     : Amount;
      begin
         Put_Line( "on person " & Sernum_Value'Image( index.SERNUM ));
         if wave = 'a' then -- enumeration, not respondent : we need a respondent flag!
            weight := ad.indall.xewght;
         else
            weight := ad.indall.xewght;
         end if;
         Put_Line( "weight is " & Amount'Image( weight ));
         if( ad.pid > 0 )then
            weighted_population := weighted_population + weight;
            total_num_people := total_num_people + 1;
         end if;
      end Load_Adult;

      f                    : File_Type;
      index_map            : BHPS_Index;
   begin
      for w in Waves_With_Data loop
         wave := w;
         total_num_people  := 0;
         weighted_population := 0.0;
         Dump_Index( "/mnt/data/bhps/bin/" & wave & "/individual_index.bin", "index_" & wave & ".dump" );
         Restore_Complete_Index( "/mnt/data/bhps/bin/" & wave & "/individual_index.bin", index_map ); 
         Open( ptrs, wave );
         Put_Line( "on wave " & wave );
         index_map.Iterate( Load_Adult'Access );
         Put_Line( "total person population wave " & wave & " : " & 
            Natural'Image( total_num_people ) & 
            " weighted " & Amount'Image( weighted_population ));
         Close( ptrs, wave );
      end loop;
   end Test_BHPS_Read_People;
   

   procedure Test_Cumulative_Normal( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   
   use Base_Model_Types;
      package MF is new Maths_Functions( Amount );
      p          : Amount := 0.0;
      c          : Amount;
      inv        : Probability;
      prob       : Probability;
      diff       : Amount;
      happens    : Boolean;
      happens_nr : Boolean;
      f          : File_Type;
   begin
      Create( f, Out_File, "inverse_cumulative_normal.csv" );
      loop
         p := p + 0.00001;
         exit when p >= 1.0;
         c := MF.Inverse_Cumulative_Normal( p );
         inv := MF.Cumulative_Normal( c );
         diff := p - inv;
         Assert( abs( diff ) < 1.0E-9, " diff was " & diff'Img );
         Put_Line( f, p'Img & "," & c'Img & "," & inv'Img & "," & diff'Img );
      end loop;
      Close( f );
      p := -100.0;
      loop
         p := p + 0.10;
         exit when p >= 100.0;
         happens := MF.Evaluate_Probit( p );
         happens_nr := MF.Evaluate_Probit( p, 0.5, False );
         prob := MF.Cumulative_Normal( p );
         Put_Line( p'Img & "," & prob'Img & "," & happens'Img & "," & happens_nr'Img );
      end loop;
      -- check extremes for overflows
      inv := MF.Cumulative_Normal( Real'Last/2.0 );
      inv := MF.Cumulative_Normal( Real'First/2.0 );
   end Test_Cumulative_Normal;   
      
   
   procedure Test_Assign( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use WG;
      type K is (a, b, c, d );
      type KAT is array( 1 .. 4 ) of Amount;
      KA : KAT := ( others => 10.0 );
      target_populations : Row_Vector := ( others => 1.0 );
   begin
      target_populations := Row_Vector( KA );
   end Test_Assign;

   procedure Index_Tests( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      package DI_IO is new Ada.Direct_IO( Integer );
      f : DI_IO.File_Type;
      pos : Positive;
      size : Natural;
      p : Integer;
   begin
      DI_IO.Create( f, DI_IO.Inout_File, "tmp/di_io.bin" );
      pos := Positive( DI_IO.Index( f ));
      size := Natural( DI_IO.Size( f ));
      Put_Line( "Index_Tests: after 1 write pos=" & pos'Img & " size=" & size'Img );
      DI_IO.Write( f, 1001 );
      pos := Positive( DI_IO.Index( f ));
      size := Natural( DI_IO.Size( f ));
      Put_Line( "Index_Tests: after 1 write pos=" & pos'Img & " size=" & size'Img );
      DI_IO.Write( f, 2002 );
      pos := Positive( DI_IO.Index( f ));
      size := Natural( DI_IO.Size( f ));
      Put_Line( "Index_Tests: after 2 writes pos=" & pos'Img & " size=" & size'Img );
      
      
      DI_IO.Set_Index( f, 2 );
      DI_IO.Read( f, p );
      Put_Line( "Index_Tests Read from pos 2 " & p'Img );

      DI_IO.Set_Index( f, 1 );
      DI_IO.Read( f, p );
      Put_Line( "Index_Tests Read from pos 1 " & p'Img );

      DI_IO.Write( f, 3003 );
      pos := Positive( DI_IO.Index( f ));
      size := Natural( DI_IO.Size( f ));
      Put_Line( "Index_Tests: after 3 writes and pos set to 1 pos=" & pos'Img & " size=" & size'Img );
      DI_IO.Set_Index( f, 2 );
      DI_IO.Read( f, p );
      Put_Line( "Index_Tests Read from pos 2 after rewrite " & p'Img );

      for i in 1 .. 100 loop
            size := Natural( DI_IO.Size( f ));
            DI_IO.Write( f, 100_000 + i, DI_IO.Positive_Count( size+1 ) );
            Put_Line( "writing item "& i'Img & " at pos " & DI_IO.Index( f )'Img );
      end loop;
      
      DI_IO.Set_Index( f, 1 );
      for i in 1 .. 102 loop
            DI_IO.Read( f, p );
            Put_Line( " item " & i'Img & " = " & p'Img );
      end loop;
      
      DI_IO.Close( f );
   end Index_Tests;
   
   
   procedure Test_Load_Weights( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Model.WSC.Household.Transitions_Basic;
      weights : All_R_Weights_Array;
   begin
      Load_R_Weights( "data/wales/weights_from_wave_r.txt", weights );
      Put_Line( "Weights Loaded" );
   end Test_Load_Weights;
   
   
   --------------------
   -- Register_Tests --
   --------------------
   procedure Register_Tests( t : in out Test_Case ) is
      use AUnit.Test_Cases.Registration;
   begin
      null;
      Register_Routine (T, Test_Load_Weights'Access, "test load weights" );
      Register_Routine (T, Index_Tests'Access, "Index_Tests" );
      Register_Routine (T, Test_Calmar_2'Access, "test calmar 2" );
      Register_Routine (T, Test_Eq_Finder'Access, "eq finder" );
      Register_Routine (T, Test_Calmar_Iterative'Access, "Test_Calmar_Iterative" );
      -- Register_Routine (T, Test_File_Pos'Access, "test file pos" );
      Register_Routine (T, Test_HH_Database'Access, "test hh" );
      Register_Routine (T, Test_Year_Keys'Access, "test year keys" );
      -- Register_Routine (T, Test_Cumulative_Normal'Access, "Test_Cumulative_Normal" );
      -- Register_Routine (T, Test_BHPS_Read_People'Access, "Test_Read_People" );
   end Register_Tests;

   ----------
   -- Name --
   ----------
   function Name ( t : Test_Case ) return Message_String is
   begin
      return Format("WSC Model calculations unit tests.");
   end Name;

end WSC_Tests;
