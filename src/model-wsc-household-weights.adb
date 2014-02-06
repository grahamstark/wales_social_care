with Ada.Numerics.Float_Random;

with Maths_Functions;

with Model.WSC.Global_Settings;
with GNATColl.Traces;

with Weighting_Commons;
with Maths_Functions.Weights_Generator;
with Maths_Functions;
with Model.WSC.Run_Settings;
with Event_Counter;

package body Model.WSC.Household.Weights is

   use Ada.Text_IO;

   package Waves_IO is new Ada.Text_IO.Enumeration_IO( Waves );
   package Sernum_IO is new Ada.Text_IO.Enumeration_IO( Sernum_Value );
   --
   -- NOTE NOW WITH HACK TO COMPRES BOTTOM 5 AGE GROUPS WIRED IN
   -- 
   -- subtype Calmar_Targets_Range is Positive range 1 .. 39;
   subtype Calmar_Targets_Range is Positive range 1 .. 39;
   subtype Populations_Range is Calmar_Targets_Range range 1 .. 30;
   subtype Short_Populations_Range is Calmar_Targets_Range range 1 .. 20;
   subtype Calmar_Targets_Vector is Vector( Calmar_Targets_Range );
   subtype Compressed_Targets_Range is Positive range 1 .. 22;   
   subtype Compressed_Targets_Vector is Vector( Compressed_Targets_Range );
   subtype Full_Weights_Vector is Vector( Calmar_Targets_Range );
  
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.HOUSEHOLD.WEIGHTS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   procedure Load_Weight_Targets( filename : String ) is
   begin
      null;
   end Load_Weight_Targets;
      
   procedure Load(
      w              : in out Weighter;
      filename       : String ) is
      file   : File_Type;
      wave   : Waves;
      hno    : Sernum_Value;
      weight : Amount;
   begin
      Open( file, in_file, filename );
      loop
         Waves_IO.Get( file, wave );
         Sernum_IO.Get( file, hno );
         Amount_IO.Get( file, weight );
         w.weights( wave ).Insert( hno, weight );
         exit when End_Of_File( file );
      end loop;
      Close( file );
    end Load;

   procedure Save(
      w              : Weighter;
      filename       : String ) is
      
   use Sernum_Weight_Package;
         
      file      : File_Type;
      this_wave : Waves;
      
      procedure Write_One( pos : Cursor ) is
         weight : Amount := Element( pos );
         hno    : Sernum_Value := Key( pos );
      begin
         Put_Line( file, Waves'Image( this_wave ) & " " & Sernum_Value'Image( hno ) & " " & Amount'Image( weight ));
      end Write_One;
      
   begin
      Create( file, Out_File, filename );
      for wave in Waves loop
         this_wave := wave;
         w.weights( wave ).Iterate( Write_One'Access );
      end loop;
      null;
      Close( file );
   end Save;
   
   
   function Get_Weight( w : Weighter; hno : Sernum_Value; wave : Waves ) return Amount is
      wt : constant Amount := w.weights( wave ).Element( hno );
   begin
      Log( "Got weight for hh " & hno'Img & " wave " & wave'Img & " = " & Format( wt ));
      return wt;
   end Get_Weight;
   
   function Get_Targets_For_Wave( wave : Simulation_Waves ) return Calmar_Targets_Vector is
      vv : Calmar_Targets_Vector;
   begin
      case wave is
         when r|s|t|u|v|w =>
      -- 2009 
            vv := (
               54_000.0,
               84_820.0,
               50_160.0,
               54_820.0,
               80_000.0,
               150_760.0,
               172_360.0,
               189_290.0,
               199_840.0,
               190_780.0,
               78_990.0,
               63_180.0,
               48_170.0,
               32_400.0,
               24_910.0,
               51_440.0,
               80_410.0,
               47_470.0,
               52_040.0,
               75_906.67,
               142_380.0,
               171_740.0,
               200_970.0,
               210_360.0,
               198_510.0,
               83_740.0,
               69_920.0,
               58_560.0,
               47_070.0,
               51_380.00,
               -- disabled by age
               13_856.0,
               17_505.0,
               18_077.0,
               19_481.0,
               34_406.0,
               -- hhld count
               1_329_940.0,
               -- residential popn -- 
               1_352.0,
               3_837.0,
               7_203.0 
               );
      when x|y|z|aa|ab =>        
         --2015
         vv := (
               53_870.0,
               90_950.0,
               52_650.0,
               50_050.0,
               73_880.0,
               145_710.0,
               200_520.0,
               171_130.0,
               206_850.0,
               183_730.0,
               94_750.0,
               72_210.0,
               54_390.0,
               36_960.0,
               30_870.0,
                       -- females,
               51_370.0,
               86_880.0,
               50_100.0,
               47_040.0,
               70_033.33,
               138_070.0,
               193_490.0,
               177_790.0,
               220_480.0,
               194_150.0,
               99_780.0,
               78_720.0,
               62_880.0,
               48_250.0,
               55_750.00,
               -- disable
               16_560.0,
               19_816.0,
               19_732.0,
               20_645.0,
               38_679.0,
               -- hhls coumt
               1_408_600.0	,
               -- residential popn -- 
               1_579.0,
               4_172.0,
               8_179.0
               );
     when ac|ad|ae|af|ag => 
        -- 2020
         vv := (
               55_090.0,
               91_640.0,
               55_050.0,
               55_500.0,
               70_166.67,
               134_090.0,
               211_450.0,
               179_170.0,
               192_700.0,
               197_630.0,
               85_760.0,
               87_340.0,
               63_310.0,
               43_720.0,
               39_140.0,
               52_530.0,
               87_600.0,
               52_600.0,
               52_860.0,
               66_280.0,
               126_840.0,
               200_650.0,
               180_520.0,
               206_020.0,
               210_920.0,
               91_850.0,
               94_280.0,
               71_640.0,
               53_630.0,
               62_000.00,
               15_127.0,
               23_819.0,
               22_640.0,
               23_422.0,
               44_700.0,
               -- hh count
               1_483_767.0	,
               -- residential popn -- 
               1_642.0,
               4_787.0,
               9_550.0

               );
   when ah|ai|aj|ak|al => 
-- 2025
         vv := (
               54_310.0,
               93_000.0,
               56_040.0,
               55_700.0,
               75_460.0,
               130_250.0,
               202_190.0,
               207_070.0,
               175_260.0,
               204_920.0,
               89_130.0,
               79_660.0,
               77_310.0,
               51_960.0,
               50_690.0,
                       --  females ,
               51_780.0,
               88_890.0,
               53_540.0,
               53_120.0,
               71_933.33,
               123_040.0,
               192_200.0,
               202_230.0,
               183_420.0,
               221_160.0,
               96_390.0,
               87_250.0,
               86_380.0,
               62_070.0,
               73_180.00,
               -- disab
               15_805.0,
               21_926.0,
               27_417.0,
               27_353.0,
               54_329.0,
            1_551_693.0,
                           -- residential popn -- 
               1_610.0,
               5_723.0,
               11_695.0 
               );
    when am .. Simulation_Waves'Last =>
              -- 2030,
         vv := (
            53_010.0,
            91_390.0,
            56_490.0,
            57_050.0,
            76_453.33,
            138_700.0,
            188_780.0,
            217_980.0,
            183_450.0,
            191_900.0,
            99_380.0,
            83_130.0,
            71_080.0,
            64_160.0,
            64_800.0,
           -- end women ,
            50_550.0,
            87_350.0,
            53_980.0,
            54_400.0,
            73_040.0,
            132_010.0,
            178_800.0,
            209_400.0,
            186_370.0,
            207_500.0,
            108_420.0,
            91_880.0,
            80_490.0,
            75_520.0,
            89_370.00,
            -- disabled by age
            17_708.0,
            23_013.0,
            25_432.0,
            33_450.0,
            67_367.0,
         1_609_445.0,
                        -- residential popn -- 
            1_749.0,
            6_002.0,
            14_557.0 
            );
      end case;
      return vv;
   end Get_Targets_For_Wave;

   function Age_Band_Str( age : Age_Range ) return String is
   begin
      case age is
         when 0 .. 2 =>
            return "0 .. 2";
         when 3 .. 7 =>
            return "3 .. 7";
         when 8 .. 10 =>
            return "8 .. 10";
         when 11 .. 13 =>
            return "11 .. 13";
         when 14 .. 17 =>
            return "14 .. 17";
         when 18 .. 24 =>
            return "18 .. 24";
         when 25 .. 34 =>
            return "25 .. 34";
         when 35 .. 44 =>
            return "35 .. 44";
         when 45 .. 54 =>
            return "45 .. 54";
         when 55 .. 64 =>
            return "55 .. 64";
         when 65 .. 69 =>
            return "65 .. 69";
         when 70 .. 74 =>
            return "70 .. 74";
         when 75 .. 79 =>
            return "75 .. 79";
         when 80 .. 84 =>
            return "80 .. 84";
         when 85 .. Age_Range'Last =>
            return "85 +   ";
      end case;
   end Age_Band_Str;
   
   
   function Age_Band_From_Age( age : Age_Range ) return Positive is
      age_pos : Positive;
   begin
      case age is
         when 0 .. 2 =>
            age_pos := 1;
         when 3 .. 7 =>
            age_pos := 2;
         when 8 .. 10 =>
            age_pos := 3;
         when 11 .. 13 =>
            age_pos := 4;
         when 14 .. 17 =>
            age_pos := 5;
         when 18 .. 24 =>
            age_pos := 6;
         when 25 .. 34 =>
            age_pos := 7;
         when 35 .. 44 =>
            age_pos := 8;
         when 45 .. 54 =>
            age_pos := 9;
         when 55 .. 64 =>
            age_pos := 10;
         when 65 .. 69 =>
            age_pos := 11;
         when 70 .. 74 =>
            age_pos := 12;
         when 75 .. 79 =>
            age_pos := 13;
         when 80 .. 84 =>
            age_pos := 14;
         when 85 .. Age_Range'Last =>
            age_pos := 15;
      end case;
      return age_pos;
   end Age_Band_From_Age;
   
   -- zero if no reported problem; otherwse 1 .. 5 for 65-69 .. 85+ age band
   -- see Daffodil http://www.daffodilcymru.org.uk/index.php?pageNo=1062&PHPSESSID=05nhl3dd73jprvqufe12t80lt5&at=a&sc=1&loc=1&np=1
   function Unable_To_Manage_Some_Activity( ad : Person ) return Natural is
      age_pos     : Natural := 0;
      help_needed : Boolean := False;
   begin
      for t in Task_Type loop
         if( ad.fitness( t ).help > by_self )then
            help_needed := True;
            exit;
         end if;
      end loop;
      if( help_needed )then
         case ad.age is
            when 0 .. 64 =>
               age_pos := 0;
            when 65 .. 69 =>
               age_pos := 1;
            when 70 .. 74 =>
               age_pos := 2;
            when 75 .. 79 =>
               age_pos := 3;
            when 80 .. 84 =>
               age_pos := 4;
            when 85 .. Age_Range'Last =>
               age_pos := 5;
         end case;
      end if;
      return age_pos;
   end  Unable_To_Manage_Some_Activity;

   function Get_Targets_From_Person( hh : Household; pers : Person ) return Calmar_Targets_Vector is
      v : Calmar_Targets_Vector;
      age_pos : Positive; 
   begin
      case pers.sex is
         when male   => age_pos := Age_Band_From_Age( pers.age );
         when female => age_pos := Age_Band_From_Age( pers.age ) + 15;
      end case;
      v( age_pos ) := 1.0;
      return v;   
   end Get_Targets_From_Person;

   --
   -- compress the 39 element array so there are no forecasting holes 
   --             males                     |         females                            |   disabled   |p|care home
   -- from 1 2 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39
   --      ---------                         -----------------                            --------------                         
   -- to       1     2 3 4  5  6  7  8  9 10      11           12 13 14 15 16 17 18 19 20       21       22 23 24 25 
   
   function Compress_Full_Vector( full : Full_Weights_Vector ) return Compressed_Targets_Vector is
      comp : Compressed_Targets_Vector := ( others => 0.0 );
   begin
      for i in 1 .. Full_Weights_Vector'Length loop
         case i is 
            when 1 .. 6 => comp( 1 ) := comp( 1 ) + full( i );
            when 7 .. 15 => comp( i - 5 ) := full( i );
            when 16 .. 21 => comp( 11 ) := comp( 11 ) + full( i );
            when 22 .. 30 => comp( i - 10 ) := full( i );
            when 31 .. 35 => comp( 21 ) := comp( 21 ) + full( i );
            -- when 36 .. Full_Weights_Vector'Last => comp( i - 14 ) := full( i ); 
            when 36 .. Full_Weights_Vector'Last => comp( 22 ) := comp( 22 ) + full( i ); 
         end case;
      end loop;
      return comp;
   end Compress_Full_Vector;
   
   function Make_Full_Vector( hh : Household ) return Full_Weights_Vector is
      obs : Full_Weights_Vector := ( others => 0.0 );
      p   : Positive;
      s   : Amount;
   begin
      obs( 36 ) := 1.0;
      
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 .. hh.benefit_units( buno ).num_adults loop
            declare
               ad      : Person renames hh.benefit_units( buno ).adults( adno );
               dis_pos : Natural := Unable_To_Manage_Some_Activity( ad );
            begin
               
                if( ad.years_in_residential_care > 0 ) and ( ad.age >= 65 )then
                   Log( "adding residential observation for ad age " & ad.age'Img );
                   if( ad.age < 75 )then
                      obs( 37 ) := 1.0;
                   elsif( ad.age < 85 )then
                      obs( 38 ) := 1.0;
                   elsif( ad.age >= 85 )then
                      obs( 39 ) := 1.0;
                   end if;
                end if;
               
               p := Age_Band_From_Age( ad.age );
               if(  ad.sex = female )then
                  p := p + 15;
               end if;
               obs( p ) := obs( p ) + 1.0;
               if( dis_pos > 0 )then
                  dis_pos := dis_pos + Populations_Range'Last;
                  obs( dis_pos ) := obs( dis_pos ) + 1.0;
               end if;
            end;
         end loop;
         for chno in 1 .. hh.benefit_units( buno ).num_children loop
            p := Age_Band_From_Age( hh.benefit_units( buno ).children( chno ).age );
            if(  hh.benefit_units( buno ).children( chno ).sex = female )then
               p := p + 15;
            end if;
            obs( p ) := obs( p ) + 1.0;   
         end loop;
      end loop;
      -- s := Sum( obs( 1 .. Populations_Range'Last ));
      return obs;
   end Make_Full_Vector;
   
   function Make_Vector( hh : Household ) return Compressed_Targets_Vector is
      obs : Full_Weights_Vector := Make_Full_Vector( hh );
   begin
      return Compress_Full_Vector( obs );
   end Make_Vector;
   
   function First_All_Zero_Row( m : Matrix ) return Integer is
      s : Real;
   begin
      for r in m'Range( 1 ) loop
         s := 0.0;
         for c in m'Range( 2 ) loop
            s := s + m( r, c );
         end loop;
         if( s = 0.0 )then
            return r;
         end if;
      end loop;
      return -1;
   end First_All_Zero_Row;
   
   function First_All_Zero_Col( m : Matrix ) return Integer is
      s : Real;
   begin
      for c in m'Range( 2 ) loop
         s := 0.0;
         for r in m'Range( 1 ) loop
            s := s + m( r, c );
         end loop;
         if( s = 0.0 )then
            return c;
         end if;
      end loop;
      return -1;
   end First_All_Zero_Col;

   procedure Create(
      w              : in out Weighter;
      db             : in out Model.WSC.Household.Database.DB_Type;
      wsc_run        : Run; 
      monitor        : in out Model.WSC.Run_Settings.Model_Monitor'Class;
      iterations     : out Positive;
      error          : out Eval_Error_Type ) is
   use Matrix_Functions;
   use Model.WSC.Household.Database;
   use Model.WSC.Household;
   use Base_Model_Types;
   use WSC_Enums;
   use Model.WSC.Run_Settings;
   use Weighting_Commons;
      
      sernums     : Sernum_List;
      hh          : Household;
      wave        : Waves := r;
      start_wave  : Waves;
      end_wave    : Waves;
      year        : Year_Number := wsc_run.Start_Year;
   begin
      iterations := 1;
      error := normal;
      Log( "Weights: Create ; files opened OK" );
      start_wave := Wave_From_Year( wsc_run.Start_Year );
      end_wave := Wave_From_Year( wsc_run.End_Year );
      waves:
      for wave in start_wave .. end_wave loop
         db.Get_Household_Sernums( wave, sernums );
         w.weights( wave ).Clear;
         declare
            num_households     : constant Positive := Positive( sernums.Length );
            package Reweigher is new Maths_Funcs.Weights_Generator(    
               Num_Constraints   => Compressed_Targets_Range'Last,
               Num_Observations  => num_households );
            use Reweigher;
            obs                 : Reweigher.Dataset := ( others => ( others => 0.0 ));
            initial_weights     : Col_Vector;
            weights             : Col_Vector;
            target_populations  : Row_Vector := Compress_Full_Vector( Get_Targets_For_Wave( wave ));
            population_total    : constant Amount := Sum( target_populations( 1 .. Short_Populations_Range'Last ));
            population_weighted : Row_Vector := ( others => 0.0 );
            sample_population   : Amount := 0.0;
            weight              : Amount;
            ct                  : Row_Vector;
         begin
            Log( "Reweight_Data ; on wave" & wave'Img & " target population " & Format( population_total ));
            monitor.Set_Stage( pre_calculations );
            monitor.Set_Counter( 2, year );
            monitor.Set_Counter( 5, 1 );
            exit when monitor.Is_Aborting;
            households:
            for hhno in 1 .. num_households loop
               hh := db.Get_Household( 
                  wave => wave, 
                  hid  => sernums.Element( hhno ));
               -- weight := hh.hdata.weights( extended_2 );
               -- if( weight = 0.0 )then
                  -- weight := hh.hdata.weights( basic );
               -- end if;
               weight := 1.0;
               ct := Make_Vector( hh );
               monitor.Assert( Sum( ct( Short_Populations_Range )) > 0.0, " populations all zero for hh " & hh.hid'Img );
               exit when monitor.Is_Aborting;
               if( wsc_run.do_reweighting )then
                  for i in Compressed_Targets_Range loop
                     obs( hhno, i ) := ct( i );
                  end loop;
                  initial_weights( hhno ) := weight;
                  sample_population := sample_population + ( weight * Amount( hh.Num_People ));
               else
                  w.weights( wave ).Insert( sernums.Element( hhno ), weight );
               end if;
            end loop households;
            if( wsc_run.do_reweighting )then
               initial_weights := initial_weights * (population_total/sample_population);
               Log( "made obs matrix as " & To_String( obs ));
               Log( "target populations " & To_String( target_populations ));
               Log( "Initial Weights " & To_String( initial_weights ));
               declare
                  curr_iterations     : Positive;
                  curr_error          : Eval_Error_Type;
                  fc                  : constant Integer := First_All_Zero_Col( obs );
                  fr                  : constant Integer := First_All_Zero_Row( obs );
               begin   
                  Log( "FC = " & fc'Img & " fr " & fr'Img );
                  monitor.Assert( fc < 0, " all zero column in data " & fc'Img );
                  exit when monitor.Is_Aborting;
                  monitor.Assert( fr < 0, " all zero row in data " & fr'Img );
                  exit when monitor.Is_Aborting;
                  Do_Reweighting(
                     Data               => obs, 
                     Which_Function     => wsc_run.Weighting_Function,
                     Initial_Weights    => initial_weights,            
                     Target_Populations => target_populations,
                     TolX               => 0.01,
                     TolF               => 0.01,
                     Max_Iterations     => 40,
                     RU                 => wsc_run.Weighting_Upper_Bound,
                     RL                 => wsc_run.Weighting_Lower_Bound,
                     New_Weights        => weights,
                     Iterations         => curr_iterations,
                     Error              => curr_error );
                  monitor.Assert( curr_error = normal, 
                     " error reported in do reweighting; " & 
                     Positive'Image( curr_iterations ) & 
                     " error " & Eval_Error_Type'Image( curr_error ));
                  exit when monitor.Is_Aborting;
                  
                  if( curr_iterations > iterations )then
                     iterations := curr_iterations;
                  end if;
                  Log( " iterations " & iterations'Img & " error " & error'Img );   
                  for k in Row_Range loop
                     Log( k'Img & " " & Format( initial_weights( k )) & " " & Format( weights( k )));
                  end loop;
                  household_weights:
                  for hhno in 1 .. num_households loop
                     w.weights( wave ).Insert( sernums.Element( hhno ), weights( hhno ));
                  end loop household_weights;
               end;
            end if;
         end; -- declare
         year := year + 1;
      end loop waves;
      Log( "Reweight_Data complete; closing dbs" );
   end Create;
   
end  Model.WSC.Household.Weights;
