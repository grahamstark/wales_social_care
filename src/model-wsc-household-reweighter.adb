with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Assertions;
with Ada.Numerics.Float_Random;
with Maths_Functions;

with Model.WSC.Household.Database;
with Model.WSC.Household.Regressions;
with Model.WSC.Uprate;
with Model.WSC.Global_Settings;
with GNATColl.Traces;

with Weighting_Commons;
with Maths_Functions.Weights_Generator;
with Maths_Functions;

package body Model.WSC.Household.Transitions_Basic is

   use Ada.Calendar;
   use Model.WSC.Uprate;
   use Ada.Assertions;
   
   Random_Gen : Ada.Numerics.Float_Random.Generator;

   function Get_Random return Rate is
   begin
      return Rate( Ada.Numerics.Float_Random.Random( Random_Gen ));
   end Get_Random;
   
   subtype Tmp_Waves is Waves range q .. ao;
   
   type Tmp_A is array( Tmp_Waves ) of Probability;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.HOUSEHOLD.TRANSITIONS_BASIC" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;



   
   Tmp_A_A : constant Tmp_A := 
   ( 
      q => 1.0000000000,
      r => 0.9731673547,
      s => 0.9443786119,
      t => 0.9181066105,
      u => 0.8935148463,
      v => 0.8698426119,
      w => 0.8476696563,
      x => 0.8270537179,
      y => 0.8056720345,
      z => 0.787065162,
      aa => 0.7701037256,
      ab => 0.7526878326,
      ac => 0.7373249543,
      ad => 0.7217748861,
      ae => 0.7086336441,
      af => 0.6970567577,
      ag => 0.6859803452,
      ah => 0.6760836553,
      ai => 0.6681036757,
      aj => 0.6589735131,
      ak => 0.6514038776,
      al => 0.6446946292,
      am => 0.6391657609,
      an => 0.6346012848,
      ao => 0.6306127195 
   );

   function Get_Death_Probability( wave : Simulation_Waves; pers : Person ) return Probability is
      deaths_per_1000 : Rate;
   begin
      case pers.sex is
         when male =>
            case pers.age is
               when  0 =>  deaths_per_1000 := 5.2;
               when  1 .. 4  => deaths_per_1000 := 0.2;
               when  5 .. 14 => deaths_per_1000 := 	0.1;
               when 15 .. 24 => deaths_per_1000 := 0.6;
               when 25 .. 34 => deaths_per_1000 := 1.1;
               when 35 .. 44 => deaths_per_1000 := 1.9;
               when 45 .. 54 => deaths_per_1000 := 3.7;
               when 55 .. 64 => deaths_per_1000 := 9.0;
               when 65 .. 74 => deaths_per_1000 := 22.8;
               when 75 .. 84 => deaths_per_1000 := 61.9;
               when 85 .. Age_Range'Last => 	deaths_per_1000 := 156.4;
            end case;
         when female =>
            case pers.age is
               when 0 =>	deaths_per_1000 := 4.3;
               when  1 .. 4 	 => deaths_per_1000 := 0.1;
               when  5 .. 14 	 => deaths_per_1000 := 0.1;
               when 15 .. 24	 => deaths_per_1000 := 0.2;
               when 25 .. 34	 => deaths_per_1000 := 0.4;
               when 35 .. 44	 => deaths_per_1000 := 1.0;
               when 45 .. 54	 => deaths_per_1000 := 2.3;
               when 55 .. 64	 => deaths_per_1000 := 6.1;
               when 65 .. 74	 => deaths_per_1000 := 14.8;
               when 75 .. 84	 => deaths_per_1000 := 44.1;
               when 85 .. Age_Range'Last => deaths_per_1000 := 138.8;
            end case;
      end case;
      
      return Tmp_A_A( wave ) * deaths_per_1000 / 1000.0;      
   end Get_Death_Probability;

   
   function  Get_One_Target_Vector_For_Wave( wave : Simulation_Waves ) return Calmar_Targets_Vector is
      vv : Calmar_Targets_Vector;
   begin
      case wave is
         when r|s|t|u|v|w =>
      -- 2009 (r) / 2010 (s)
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
   end Get_One_Target_Vector_For_Wave;
   
   function Growth_Between( base_wave, wave1 : Waves; v1, v2 : Amount ) return Amount is
      g : amount := (v2/v1)**(0.2) - 1.0; -- (a/b)**1/n - 1
      p : Amount := Amount( Waves'Pos( wave1 ) - Waves'Pos( base_wave ));
   begin
      return v1 * ( 1.0 + g )**p;
   end Growth_Between;
      
   
   function Get_Targets_For_Wave( wave : Simulation_Waves ) return Calmar_Targets_Vector is
      v1, v2, outv :  Calmar_Targets_Vector;
      start_wave : Waves;
   begin
      case wave is
         when r|s|t|u|v|w =>
            v1 := Get_One_Target_Vector_For_Wave( r );
            v2 := Get_One_Target_Vector_For_Wave( x );
            start_wave := s;
         when x|y|z|aa|ab =>       
            v1 := Get_One_Target_Vector_For_Wave( x );
            v2 := Get_One_Target_Vector_For_Wave( ac );
            start_wave := x;
         when ac|ad|ae|af|ag => 
            v1 := Get_One_Target_Vector_For_Wave( ac );
            v2 := Get_One_Target_Vector_For_Wave( ah );
            start_wave := ac;
         when ah|ai|aj|ak|al => 
            v1 := Get_One_Target_Vector_For_Wave( ah );
            v2 := Get_One_Target_Vector_For_Wave( am );
            start_wave := ah;
         when am .. Simulation_Waves'Last =>
            v1 := Get_One_Target_Vector_For_Wave( am );
            v2 := Get_One_Target_Vector_For_Wave( am );
            start_wave := am;
      end case;
      for i in v1'Range loop
         outv( i ) := Growth_Between( start_wave, wave, v1( i ), v2( i ));
      end loop;
      return outv;
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
   

   procedure Impute_Care_Amounts( 
      hh             : in out Household; 
      hh_last_period : Household;
      wsc_run : Run ) is
   use Model.WSC.Household.Regressions;
   begin
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 ..  hh.benefit_units( buno ).num_adults loop
            declare
               bu                  : Benefit_Unit renames hh.benefit_units( buno );
               ad                  : Person renames bu.adults( adno );
               ad_last_period      : Person renames hh_last_period.benefit_units( buno ).adults( adno );
               is_couple           : Boolean := bu.position_of_spouse > 0 and ( bu.position_of_head = ad.pno or bu.position_of_spouse = ad.pno );
               pr                  : Amount;
               rr                  : Regression_Results;
               use_lagged_dep_vars : constant Boolean := False;
            begin
               if( hh.hdata.region = wales )then
                  if( ad.age >= 65 )then
                     --
                     --
                     pr :=  Informal_Care_From_Householder_Probit( ad, ad_last_period, hh.hdata.region, use_lagged_dep_vars );
                     Log( "prob informal care is " & Format( pr ));
                     ad.receives_informal_care_from_household_member := 
                        Evaluate_Probit( pr, wsc_run.Use_Random_Threshold );
                     if( ad.receives_informal_care_from_household_member )then
                        rr :=  Hours_Of_Care_Regression( ad, hh.hdata.region );   
                        if( wsc_run.Use_Random_Threshold )then
                           rr.vp := rr.vp + Random_Normal_Generator.Draw( 
                              mean => 0.0, 
                              standard_deviation => rr.sd );
                        end if;
                        Log( "got predicted hours as " & Format( rr.vp ));
                        if( rr.vp < 0.0 )then
                           ad.hours_of_care_recieved := 0;
                        elsif( rr.vp > Amount( Hours_Count'Last ))then
                           ad.hours_of_care_recieved := Hours_Count'Last; 
                        else
                           ad.hours_of_care_recieved := Hours_Count( rr.vp );
                        end if;
                     end if;
                     Recalculate_Regressors( ad, hh.Num_Adults, hh.Num_Children, hh.hdata );
                     pr :=  Informal_Care_From_Non_Householder_Probit( ad, ad_last_period, hh.hdata.wave );
                     ad.receives_informal_care_from_non_householder  := 
                        Evaluate_Probit( pr, wsc_run.Use_Random_Threshold );
                     Recalculate_Regressors( ad, hh.Num_Adults, hh.Num_Children, hh.hdata );
                  end if;
               end if;
            end;
         end loop;
      end loop;
   end Impute_Care_Amounts;

   function Get_Uprate_Amount( 
      interview_date : Time; 
      wave     : Waves;
      which    : Uprate_Targets; 
      wsc_run : Run ) return Rate is
      y        : Year_Number;
      m        : Month_Number;
      d        : Day_Number;
      sec      : Day_Duration;
      assump   : Uprate_Assumption := wsc_run.uprate_assumptions( which );
      ch       : Rate;
      rpich    : Rate := 1.0;
      years_between : Natural := Natural( Waves'Pos( wave ) - Waves'Pos( r ));
      yfw : Year_Number := Year_From_Wave( wave );
   begin
      
      Split( interview_date, y, m, d, sec );
      if( assump.use_obr )then
         ch := Get_Ratio_Between( assump.element, y, m, yfw, m );
         rpich := Get_Ratio_Between( rpi, y, m, yfw, m );
         Log( "useobr = " & assump.use_obr'Img 
              & " which = " & which'Img 
              & " assump.element " & assump.element'Img
              & " y = " & y'Img 
              & " yfw = " & yfw'Img 
              & " m = " & m'Img 
              & " ch = " & ch'Img 
              & " rpich = " & rpich'Img );
      else
         return ( 1.0 + ( assump.percent_change / 100.0 )) ** years_between;
      end if;
      
      if( assump.element = gdp_at_market_prices ) and ( not wsc_run.real_terms ) then -- gdp is in real terms
         return ch * rpich;
      elsif( assump.element /= gdp_at_market_prices ) and ( wsc_run.real_terms ) then
         return ch / rpich;
      end if;
      return ch;
   end  Get_Uprate_Amount;
   
   procedure Uprate( 
      incomes  : in out Incomes_Array; 
      date     : Time; 
      wave     : Waves;
      wsc_run : Run ) is
      dother   : Amount;
      dbens    : Amount;
      dearn    : Amount;
   begin
      dbens := Get_Uprate_Amount( date, wave,  upr_state_benefits, wsc_run); -- Get_Ratio_Between( gdp_at_market_prices_nominal, y-1, m, y, m );
      dearn := Get_Uprate_Amount( date, wave, upr_earnings, wsc_run); -- Get_Ratio_Between( obr_earnings, y-1, m, y, m );
      dother := Get_Uprate_Amount( date, wave, upr_other_income, wsc_run); -- Get_Ratio_Between( rpix, y-1, m, y, m ); -- no ROSSI index?
      for i in Earnings loop
         incomes( i ) := incomes( i ) * dearn;
      end loop;
      for i in State_Benefits loop
         incomes( i ) := incomes( i ) * dbens;
      end loop;
      for i in Other_Income loop
        incomes( i ) := incomes( i ) * dother; 
      end loop;
   end Uprate;

   procedure Add_One_Year( date : in out Time ) is
      y        : Year_Number;
      m        : Month_Number;
      d        : Day_Number;
      sec      : Day_Duration;
   begin
      Split( date, y, m, d, sec );      
      date := Time_Of( y+1, m, 1 ); 
   end Add_One_Year;
   
   procedure Age_And_Uprate( 
      hh : in out Household; 
      wsc_run : Run;
      wave     : Waves;
      uprate_only : Boolean ) is
      date    : Time renames hh.hdata.interview_date;
      dhouse  : Amount;
      dwealth : Amount;
      hh_last_period : Household := hh;
   begin
      Log( "uprating to " & Ada.Calendar.Formatting.Image( date ));
      --
      -- assume 1st day of month always
      -- keep this date change so we can uprate correctly
      -- Add_One_Year( date );
      
      dhouse := Get_Uprate_Amount( date, wave, upr_housing_costs, wsc_run); -- Get_Ratio_Between( gdp_at_market_prices_nominal, y-1, m, y, m );
      dwealth := Get_Uprate_Amount( date, wave, upr_wealth, wsc_run);
      -- Get_Ratio_Between( gdp_at_market_prices_nominal, y-1, m, y, m );
      -- pro tem assumptions
      -- 1) rents & mortgage costs rise with nominal gdp
      
      hh.hdata.wave := Waves'Succ( hh.hdata.wave );
      hh.wave := hh.hdata.wave;
      
      hh.hdata.gross_rent := hh.hdata.gross_rent * dhouse;
      hh.hdata.net_rent := hh.hdata.net_rent * dhouse;
      hh.hdata.mortgage_outstanding := hh.hdata.mortgage_outstanding * dhouse;
      hh.hdata.gross_housing_costs := hh.hdata.gross_housing_costs * dhouse;
      hh.hdata.net_housing_costs := hh.hdata.net_housing_costs * dhouse;
      hh.hdata.total_income := hh.hdata.total_income * dhouse;
      hh.hdata.mortgage_payment := hh.hdata.mortgage_payment * dhouse;
      
      hh.hdata.house_value := hh.hdata.house_value * dhouse;
      hh.hdata.other_property_value := hh.hdata.other_property_value * dhouse;
      if( not uprate_only )then
         if( hh.hdata.years_outstanding_on_mortgage > 1 )then
            hh.hdata.years_outstanding_on_mortgage := hh.hdata.years_outstanding_on_mortgage - 1;
         else
            hh.hdata.years_outstanding_on_mortgage := 0;
            hh.hdata.mortgage_outstanding := 0.0; -- must have mortgage 
         end if;
      end if;
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 ..  hh.benefit_units( buno ).num_adults loop
            declare
               bu : Benefit_Unit renames hh.benefit_units( buno );
               ad : Person renames bu.adults( adno );
               bu_last_period : Benefit_Unit renames hh_last_period.benefit_units( buno );
               ad_last_period : Person renames bu_last_period.adults( adno );
               is_couple : Boolean := bu.position_of_spouse > 0 and ( bu.position_of_head = ad.pno or bu.position_of_spouse = ad.pno );
            begin
               if( not uprate_only )then
                  ad.age := ad.age + 1;   
                  ad.wave := hh.wave;
                  if( ad.years_in_residential_care > 0 )then
                     ad.years_in_residential_care := ad.years_in_residential_care + 1;
                  end if;
               end if;
               -- Uprate( ad.annual_income, date, wave, wsc_run);
               Uprate( ad.current_income, date, wave, wsc_run);
               ad.personal_wealth := ad.personal_wealth * dwealth;
               Recalculate_Regressors( 
                  ad, 
                  hh.Num_Adults, 
                  hh.Num_Children,
                  hh.hdata );
            end; 
         end loop;
         for chno in 1 .. hh.benefit_units( buno ).num_children loop
            declare
               ch : Person renames hh.benefit_units( buno ).children( chno );
               ch_last_period : Person renames hh_last_period.benefit_units( buno ).children( chno );
               is_couple : Boolean := False;
            begin
               if( not uprate_only )then
                  ch.age := ch.age + 1;   
                  ch.wave := hh.wave;
               end if;
               -- Uprate( ch.annual_income, date, wave, wsc_run);
               Uprate( ch.current_income, date, wave, wsc_run);
               Recalculate_Regressors( 
                  ch, 
                  hh.Num_Adults, 
                  hh.Num_Children,
                  hh.hdata );
            end; 
         end loop;
      end loop;
   end Age_And_Uprate;

   procedure Create_Initial_Care_Home_Population(       
      db           : in out Model.WSC.Household.Database.DB_Type;
      wsc_run : Run;   
      wave         : Waves ) is
      use Model.WSC.Household.Database;
      use Model.WSC.Household;
      use Base_Model_Types;
      use WSC_Enums;
      
      procedure Make_New_Res_HHlds( pl : in out Person_Ordering_List; target_count : Amount ) is
         c              : Amount := 0.0;
         n              : Natural := Natural( pl.Length );
         po             : Person_Ordering_Record;
         hh             : Household;
         hh_last_period : Household;
         buno           : Benefit_Unit_Number; 
         adno           : Adult_Count;
         chno           : Child_Count;
         found          : Boolean;
         j              : Natural := 0;
      begin
         Sort_By_V( pl );
         Log( "Create_Initial_Care_Home_Population; n = " & n'Img & " target count : " & Format( target_count ));
         for i in reverse 1 .. n loop
            exit when( c >= target_count );
            po := pl.Element( i );
            Log( "Create_Initial_Care_Home_Population; wave " & wave'Img & i'Img & " : " & To_String( po ));
            hh := db.Get_Household( wave, po.hid );
            hh_last_period := hh;
            -- Age_And_Uprate( hh, wsc_run, r, False );
            Impute_Care_Amounts( hh, hh_last_period, wsc_run );
            hh.hid := hh.hid + 200_000;
            hh.hdata.hid := hh.hid; 
            hh.benefit_units( 1 ).adults( 1 ).years_in_residential_care := 1;
            Log( "Create_Initial_Care_Home_Population got household " & 
                 hh.hid'Img & 
                 " new sernum " & hh.hid'Img );
            hh.Find_Person_By_Pno( po.pno, buno, adno, chno, found );
            Log( "Create_Initial_Care_Home_Population changed years_in_residential_care for buno " & buno'Img & " adno " & adno'Img );
            Log( "hhld is " & To_String( hh ));
            declare
               ad : Person renames hh.benefit_units( buno ).adults( adno );
            begin
               ad.years_in_residential_care := 1;     
               ad.pid := ad.pid + 200_000;
            end;            
            db.Write_Household( hh );
            c := c + po.weight * WEIGHT_MULTIPLIER;
            j := j + 1;
            Log( "population is now " & Format( c ) & " target " & Format( Target_Count ) & " i = " & j'Img );
         end loop;
      end Make_New_Res_HHlds;
      
      sernums        : Sernum_List;
      hh             : Household;
      hh_last_period : Household;
      num_households : Natural;
      age_u_75, age_75_84, age_85_plus : Person_Ordering_List;
    begin
      db.Get_Household_Sernums( wave, sernums );
      num_households := Natural( sernums.Length );
      households:
      for hhno in 1 .. num_households loop
         hh := db.Get_Household( 
            wave => wave, 
            hid  => sernums.Element( hhno ));
         if( hh.Num_People = 1 ) and ( hh.benefit_units( 1 ).adults( 1 ).age >= 65 )then
            hh_last_period := hh;
            -- Age_And_Uprate( hh, wsc_run, r, False );
            Impute_Care_Amounts( hh, hh_last_period, wsc_run );
            declare
               ad      : Person renames hh.benefit_units( 1 ).adults( 1 );
               po      : Person_Ordering_Record := ( 
                  hid    => hh.hid, 
                  pid    => ad.pid, 
                  pno    => ad.pno,
                  v      => ad.regressors( adlscore ), 
                  weight => hh.hdata.weights( extended_2 )); -- extended2 == wales sample
            begin
               if( ad.years_in_residential_care <= 0 ) then  -- always true, of course
                  Log( "Create_Initial_Care_Home_Population wave " & wave'Img & " hh " & hh.hid'img & " adding a person aged " & ad.age'Img & "ad.years_in_residential_care " & ad.years_in_residential_care'Img );
                  Log( "po is " & To_String( po ));
                  if( ad.age < 75 )then
                     age_u_75.Append( po );
                  elsif( ad.age < 85 )then
                     age_75_84.Append( po );
                  elsif( ad.age >= 85 )then
                     age_85_plus.Append( po );
                  end if;
               end if;
            end;
         end if;
      end loop households;
      Make_New_Res_HHlds( age_u_75, 1_352.0 );
      Make_New_Res_HHlds( age_75_84, 3_837.0 );
      Make_New_Res_HHlds( age_85_plus, 7_203.0 );
   end Create_Initial_Care_Home_Population;

   procedure Infer_Wealth( 
      hh : in out Household;
      wsc_run : Run ) is
   use Model.WSC.Household.Regressions;
   begin
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 ..  hh.benefit_units( buno ).num_adults loop
            declare
               bu                     : Benefit_Unit renames hh.benefit_units( buno );
               ad                     : Person renames bu.adults( adno );
               has_significant_wealth : Boolean;
               pr                     : Amount;
               rr                  : Regression_Results;
             begin
               pr := Has_Wealth_Probit( ad );
               has_significant_wealth := Evaluate_Probit( pr, wsc_run.Use_Random_Threshold );
               Log( "has significant wealth; pr = " & pr'Img & " has_significant_wealth " & has_significant_wealth'Img );
               if has_significant_wealth then
                  rr :=  Log_Wealth_Regression( ad, hh.hdata.region );
                  if( wsc_run.Use_Random_Threshold )then
                     rr.vp := rr.vp + Random_Normal_Generator.Draw( 
                        mean => 0.0, 
                        standard_deviation => rr.sd );
                  end if;
                  ad.personal_wealth := Exp( rr.vp );
                  Log( "ad.personal_wealth " & ad.personal_wealth'Img );
               end if;
             end;
         end loop;
      end loop;
   end Infer_Wealth;

   procedure Load_R_Weights( filename : String; weights : in out All_R_Weights_Array ) is
      use Ada.Text_IO;
      hh_count  : R_Weights_Range;
      inf       : File_Type;
      this_wave : Waves;
      f_href    : R_Weights_Range;
      package Wave_IO is new Ada.Text_IO.Enumeration_IO( Waves );
   begin
      Open( inf, In_File, filename );
      Put_Line( "wave file opened OK" );
      for wave in Simulation_Waves loop
         Wave_IO.Get( inf, this_wave );
         Put_Line( "read wave id "& this_wave'Img );
         Assert( wave = this_wave, " waves mismatch should be " & wave'Img & " was " & this_wave'Img );
         Int_IO.Get( inf, hh_count );
         Put_Line( "read count as " & hh_count'Img );
         Assert( hh_count = R_Weights_Range'Last, " should be " & R_Weights_Range'Last'Img & " was " & hh_count'Img );
         for hhref in R_Weights_Range loop
            Int_IO.Get( inf, f_href );
            Amount_IO.Get( inf, weights( wave)( hhref ));
            Put_Line( "weight[" & hhref'Img & "] = " & Format( weights( wave )( hhref )));
         end loop;
      end loop;
      Close( inf );
   end  Load_R_Weights;
   
   procedure Create_Weights(
      wsc_run             : Run;
      db                  : in out Model.WSC.Household.Database.DB_Type;
      wave                : Waves;
      target_populations  : Vector;
      iterations          : out Positive;
      error               : out Eval_Error_Type;
      weights             : out Weights_Map ) is
   use Matrix_Functions;
   use Model.WSC.Household.Database;
   use Model.WSC.Household;
   use Base_Model_Types;
   use WSC_Enums;
   use Weighting_Commons;
      sernums : Sernum_List;
      hh      : Household;
   begin
      iterations := 1;
      error := normal;
      db.Get_Household_Sernums( wave, sernums );
      Log( "Reweight_Data ; files opened OK" );
      db.Get_Household_Sernums( wave, sernums );
      declare
         num_households     : constant Positive := Positive( sernums.Length );
         package Reweigher is new Maths_Funcs.Weights_Generator(    
            Num_Constraints   => Calmar_Targets_Range'Last,
            Num_Observations  => num_households );
         use Reweigher;
         obs                 : Reweigher.Dataset := ( others => ( others => 0.0 ));
         initial_weights     : Col_Vector;
         final_weights       : Col_Vector;
         p                   : Row_Range;
         population_total    : constant Amount := Sum( target_populations( 1 .. Populations_Range'Last ));
         population_weighted : Row_Vector := ( others => 0.0 );
         sample_population   : Amount := 0.0;
         weight              : Amount;
      begin
         Log( "Reweight_Data ; on wave" & wave'Img & " target population " & Format( population_total ));
         households:
         for hhno in 1 .. num_households loop
            hh := db.Get_Household( 
               wave => wave, 
               hid  => sernums.Element( hhno ));
            weight := hh.hdata.weights( extended_2 );
            if( weight = 0.0 )then
               weight := hh.hdata.weights( basic );
            end if;
            -- weight := 1.0;
            initial_weights( hhno ) := weight;
            sample_population := sample_population + ( weight * Amount( hh.Num_People ));
            obs( hhno, 36 ) := 1.0;
            for buno in 1 .. hh.num_benefit_units loop
               for adno in 1 .. hh.benefit_units( buno ).num_adults loop
                  declare
                     ad      : Person renames hh.benefit_units( buno ).adults( adno );
                     dis_pos : Natural := Unable_To_Manage_Some_Activity( ad );
                  begin
                     if( ad.years_in_residential_care > 0 ) and ( ad.age >= 65 )then
                        Log( "adding residential observation for ad age " & ad.age'Img );
                        if( ad.age < 75 )then
                           obs( hhno, 37 ) := 1.0;
                        elsif( ad.age < 85 )then
                           obs( hhno, 38 ) := 1.0;
                        elsif( ad.age >= 85 )then
                           obs( hhno, 39 ) := 1.0;
                        end if;
                     end if;
                     --
                     -- note that this is the combine dhhld and non-hhld population
                     --
                     p := Age_Band_From_Age( ad.age );
                     if(  ad.sex = female )then
                        p := p + 15;
                     end if;
                     obs( hhno, p ) := obs( hhno, p ) + 1.0;
                     if( dis_pos > 0 )then
                        dis_pos := dis_pos + Populations_Range'Last;
                        obs( hhno, dis_pos ) := obs( hhno, dis_pos ) + 1.0;
                     end if;
                  end;
               end loop;
               for chno in 1 .. hh.benefit_units( buno ).num_children loop
                  p := Age_Band_From_Age( hh.benefit_units( buno ).children( chno ).age );
                  if(  hh.benefit_units( buno ).children( chno ).sex = female )then
                     p := p + 15;
                  end if;
                  obs( hhno, p ) := obs( hhno, p ) + 1.0;   
               end loop;
            end loop;
         end loop households;
         initial_weights := initial_weights * (population_total/sample_population);
         Log( "made obs matrix as " & To_String( obs ));
         Log( "Initial Weights " & To_String( initial_weights ));
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
            New_Weights        => final_weights,
            Iterations         => iterations,
            Error              => error );
         Log( " iterations " & iterations'Img & " error " & error'Img );
         if( error = normal )then
            for hhno in 1 .. num_households loop
               weights.Insert( 
                  Key      =>  sernums.Element( hhno ),
                  New_Item => final_weights( hhno ));
            end loop;               
         end if;
      end;
   end Create_Weights;

   procedure Create_Weights_File(
      weights_file_name : String;
      from_wave         : Waves;
      wsc_run : Run ) is     
   use Weighting_Commons;
   use Ada.Text_IO;
   use Model.WSC.Household.Database;
      out_f       : File_Type;
      db          : DB_Type;
   begin
      Create( out_f, Out_File, weights_file_name & "_" & from_wave'Img & ".txt" );
      Open( db, Model.WSC.Global_Settings.Physical_Root & "data/wales",  actual, 1 ); 
      waves:
      for wave in Simulation_Waves loop
         declare
            target_populations  : Vector := Get_Targets_For_Wave( wave );
            iterations          : Positive;
            error               : Eval_Error_Type;
            weights             : Weights_Map;       
         begin
            weights.Clear;
            Create_Weights(
               wsc_run             => wsc_run,
               db                  => db,
               wave                => wave,
               target_populations  => target_populations,
               iterations          => iterations,
               error               => error,
               weights             => weights );
            if( error = normal )then
               Put_Line( out_f, wave'Img );
               Put_Line( out_f, weights.Length'Img );
               for cur in weights.Iterate loop 
                  declare
                     use Weights_Map_Package;
                     w  : Amount := Element( cur );
                     id : Sernum_Value := Key( cur );
                  begin
                     Put_Line( out_f, id'Img & " " & w'Img );
                  end;
               end loop;
            else
               Put_Line( "ERROR " & error'Img & " iters " & iterations'Img );
            end if;
         end; -- declare
      end loop waves;
      Log( "Create_Simulation_Data; closing dbs" );
      db.Close;
      Close( out_f );
   end Create_Weights_File;
      
   procedure Create_Simulation_Data(
      in_db_name     : String;
      out_db_name    : String;
      wsc_run : Run; 
      monitor        : Model.Run_Settings.Model_Monitor'Class ) is
         
      use Matrix_Functions;
      use Model.WSC.Household.Database;
      use Model.WSC.Household;
      use Base_Model_Types;
      use WSC_Enums;
      use Weighting_Commons;
      
      sernums  : Sernum_List;
      in_db     : DB_Type;
      out_db   : DB_Type;
      hh       : Household;
      wave     : Waves := r;
    begin
      Open( in_db, Model.WSC.Global_Settings.Physical_Root & "data/wales",  actual, 1 ); 
      for wave in Estimated_Data_Waves loop
         Create_Initial_Care_Home_Population( in_db, wsc_run, wave );
      end loop;
      in_db.Get_Household_Sernums( wave, sernums );
      Log( "Reweight_Data ; files opened OK" );
      waves:
      for wave in Estimated_Data_Waves loop
         in_db.Get_Household_Sernums( wave, sernums );
         declare
            num_households     : constant Positive := Positive( sernums.Length );
            package Reweigher is new Maths_Funcs.Weights_Generator(    
               Num_Constraints   => Calmar_Targets_Range'Last,
               Num_Observations  => num_households );
            use Reweigher;
            obs                 : Reweigher.Dataset := ( others => ( others => 0.0 ));
            initial_weights     : Col_Vector;
            weights             : Col_Vector;
            target_populations  : Row_Vector := Row_Vector( Get_Targets_For_Wave( wave ));
            p                   : Row_Range;
            population_total    : constant Amount := Sum( target_populations( 1 .. Populations_Range'Last ));
            population_weighted : Row_Vector := ( others => 0.0 );
            sample_population   : Amount := 0.0;
            weight              : Amount;
            iterations          : Positive;
            error               : Eval_Error_Type;
         begin
            Log( "Reweight_Data ; on wave" & wave'Img & " target population " & Format( population_total ));
            households:
            for hhno in 1 .. num_households loop
               hh := in_db.Get_Household( 
                  wave => r, 
                  hid  => sernums.Element( hhno ));
               Age_And_Uprate( 
                  hh => hh, 
                  wsc_run => wsc_run, 
                  wave => wave, 
                  uprate_only => True );
               if( wave = r )then
                  Infer_Wealth( hh, wsc_run );
                  Impute_Care_Amounts( hh, hh, wsc_run );
               end if;
               weight := hh.hdata.weights( extended_2 );
               if( weight = 0.0 )then
                  weight := hh.hdata.weights( basic );
               end if;
               weight := 1.0;
               -- Log( "hid " & hh.hid'Img & "wave " & wave'Img & " hh.Num_People " &  hh.Num_People'Img & " hh.hdata.weight " &  Format( weight ));
               -- HELP!! FIXME Assert( weight > 0.0, " we should only see positive weights " );
               initial_weights( hhno ) := weight;
               sample_population := sample_population + ( weight * Amount( hh.Num_People ));
               obs( hhno, 36 ) := 1.0;
               
               for buno in 1 .. hh.num_benefit_units loop
                  for adno in 1 .. hh.benefit_units( buno ).num_adults loop
                     declare
                        ad      : Person renames hh.benefit_units( buno ).adults( adno );
                        dis_pos : Natural := Unable_To_Manage_Some_Activity( ad );
                     begin
                        
                         if( ad.years_in_residential_care > 0 ) and ( ad.age >= 65 )then
                            Log( "adding residential observation for ad age " & ad.age'Img );
                            if( ad.age < 75 )then
                               obs( hhno, 37 ) := 1.0;
                            elsif( ad.age < 85 )then
                               obs( hhno, 38 ) := 1.0;
                            elsif( ad.age >= 85 )then
                               obs( hhno, 39 ) := 1.0;
                            end if;
                         end if;
                        
                        p := Age_Band_From_Age( ad.age );
                        if(  ad.sex = female )then
                           p := p + 15;
                        end if;
                        obs( hhno, p ) := obs( hhno, p ) + 1.0;
                        if( dis_pos > 0 )then
                           dis_pos := dis_pos + Populations_Range'Last;
                           obs( hhno, dis_pos ) := obs( hhno, dis_pos ) + 1.0;
                        end if;
                     end;
                  end loop;
                  for chno in 1 .. hh.benefit_units( buno ).num_children loop
                     p := Age_Band_From_Age( hh.benefit_units( buno ).children( chno ).age );
                     if(  hh.benefit_units( buno ).children( chno ).sex = female )then
                        p := p + 15;
                     end if;
                     obs( hhno, p ) := obs( hhno, p ) + 1.0;   
                  end loop;
               end loop;
            end loop households;
            initial_weights := initial_weights * (population_total/sample_population);
            Log( "made obs matrix as " & To_String( obs ));
            Log( "Initial Weights " & To_String( initial_weights ));
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
               Iterations         => iterations,
               Error              => error );
            Log( " iterations " & iterations'Img & " error " & error'Img );   
            for k in Col_Range loop
               Log( k'Img & " " & Format( initial_weights( k )) & " " & Format( weights( k )));
            end loop;
            household_weights:
            for hhno in 1 .. num_households loop
               hh := in_db.Get_Household( 
                  wave => wave, 
                  hid  => sernums.Element( hhno ));
                  
               hh.hdata.weights( extended_2 ) := weights( hhno );
               for buno in 1 .. hh.num_benefit_units loop
                  for adno in 1 .. hh.benefit_units( buno ).num_adults loop
                     declare
                        ad      : Person renames hh.benefit_units( buno ).adults( adno );
                     begin
                        ad.respondent_weights( extended_2 ) := weights( hhno );
                     end;
                  end loop;
                  for chno in 1 .. hh.benefit_units( buno ).num_children loop
                     declare
                        ch      : Person renames hh.benefit_units( buno ).children( chno );
                     begin
                        ch.respondent_weights( extended_2 ) := weights( hhno );
                     end;
                  end loop;
               end loop;
               out_db.Write_Household( hh );
            end loop household_weights;
         end; -- declare
      end loop waves;
      Log( "Create_Simulation_Data; closing dbs" );
      in_db.Close;
      out_db.Close;
   end Create_Simulation_Data;
   
end  Model.WSC.Household.Transitions_Basic;
