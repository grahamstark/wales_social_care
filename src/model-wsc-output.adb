with Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO;

with Costs_Tabulator.Text_IO;
with IO_Commons;
with Inequality_Generator;
with Tabulator.Text_IO;
with Text_Utils;
with GNATColl.Traces;

package body Model.WSC.Output is

   use Ada.Text_IO;
   use IO_Commons;
   use Text_Utils;
   use Tabulator_Commons;
   
   -- function Get_Available_Years( o : Outputs_Array ) return Years_Array is
      -- use Outputs_By_Year;
      -- l : Natural := Natural( o.Length );
      -- a : Years_Array( 1 .. l );
      -- p : Positive := 1;
      -- 
      -- procedure Add( c : Cursor ) is
      -- begin
         -- a( p ) := Key( c );
         -- p := p + 1;
      -- end Add;
      -- 
   -- begin
      -- o.Iterate( Add'Access );
      -- return a;
   -- end Get_Available_Years;
-- 
   -- procedure Free( o : in out Outputs_Array_Access ) is
   -- begin
      -- Free_Outputs_Array( o );
   -- end Free;
-- 
   -- function Compare_Outputs_Rec( l,r : Outputs_Rec ) return Boolean is
   -- begin
      -- return False;
   -- end Compare_Outputs_Rec;
   -- 
   -- 
   -- function Get_Null_Output return Outputs_Array_Access is
      -- o : Outputs_Array_Access := null;
   -- begin
      -- return o;
   -- end Get_Null_Output;


   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.OUTPUT" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

  
   function Is_Initialised( outputs : Outputs_Rec ) return Boolean is
   begin
      return outputs.initialised;
   end Is_Initialised;
   
   function Get_Null_Data return Outputs_Rec is
      d :  Outputs_Rec;
   begin
      d.initialised := False;
      return d;
   end Get_Null_Data;
   
   function Pretty_Print( t : Breakdown_Target ) return String is
   begin
      return Breakdown_Target'Image( t );
   end Pretty_Print;
   
   procedure Clear( lg : in out Lorenz_And_Gini ) is
   begin
      lg.gini  := 0.0;
      lg.inequality_measures := ( others => 0.0 );
      lg.lorenz.Clear;
   end Clear;
    
   
   function Create_Lorenz_And_Gini( 
      all_points               : Inequality_List; 
      target_type              : Breakdown_Target;
      which_tenure             : Tenure_Type := Tenure_Type'First;
      which_decile             : Decile_Number := Decile_Number'First;
      which_age_of_head        : Adult_Age_Band;
      num_bins                 : Natural := LORENZ_BIN_SIZE ) return Lorenz_And_Gini is
      
      points_for_output : be_inequality.Quantile_List;
      
      procedure Add_To_Disaggregated( c : Inequality_Package.Cursor ) is
         iq : Inequality_Record := Inequality_Package.Element( c );
         include_this_point : Boolean := False;
         quant : be_inequality.Quantile;
      begin
         if( iq.income > 0.0 ) then
            case target_type is
               when no_breakdown          => include_this_point := True;
               when by_tenure             => include_this_point := iq.tenure = which_tenure;
               when by_decile             => include_this_point := iq.decile = which_decile;
               when by_age_of_head        => include_this_point := iq.age_of_head = which_age_of_head;
            end case;
            if( include_this_point ) then
               quant.income := iq.income;
               quant.population := iq.population;
               be_inequality.Quantile_Package.Append( points_for_output, quant );
            end if;
         end if;
      end Add_To_Disaggregated;
      
      
      use Ada.Containers;
      lg : Lorenz_And_Gini;
      inequality_measures : be_inequality.Inequality_Array;
      lorenz        : be_inequality.Quantile_List;
   begin
      Clear( lg );
      Inequality_Package.Iterate( all_points, Add_To_Disaggregated'Access );
      if( be_inequality.Quantile_Package.Length( points_for_output ) > 0 ) then
         be_inequality.Sort_By_Income( points_for_output );
         inequality_measures := be_inequality.Generate( points_for_output );
         lg.gini := inequality_measures( be_inequality.gini ) * 100.0;
         lg.inequality_measures := inequality_measures;
         lg.lorenz := be_inequality.Binify( points_for_output, num_bins );
      end if;
      return lg;
   end Create_Lorenz_And_Gini;

   procedure Print_Gini( 
      f           : File_Type; 
      target_type : Breakdown_Target;
      breakdown   : String;
      sys_no      : Positive; 
      lg          : Lorenz_And_Gini ) is
      use be_inequality;
      use Quantile_Package;
      quant : Quantile;
      top_quant : Quantile;
      num_bins    : constant Natural := Natural(Length( lg.lorenz ));
   begin
      Put( f, """lorenz""," );
      Put( f, """" & Pretty_Print( target_type ) & """," );
      Put( f, """" & breakdown & """," );
      Put( f, Rate'Image( lg.gini ) & "," );
      Put( f, "2009," );
      Put( f, Positive'Image( sys_no ) & "," );
      Put_Line( f, Natural'Image( num_bins + 1 ) ); -- 1 extra for zero starting point
      top_quant := Element( lg.lorenz, num_bins );
      Put_Line( f, "0.0,0.0" ); -- starting point      
      for i in 1 .. num_bins loop
         quant := Element( lg.lorenz, i );
         Put_Line( 
            f, 
            Rate'Image( quant.population/top_quant.population ) & "," &
            Rate'Image( quant.income/top_quant.income ));
      end loop;
   end Print_Gini;
   
   --
   -- FIXME 
   --
   procedure Print_All_Ginis( 
      f               : File_Type; 
      sys_no          : Positive; 
      all_net_incomes : Inequality_List ) is
      lg : Lorenz_And_Gini;
   begin
      Print_Gini( 
         f, 
         no_breakdown, 
         "no_breakdown", 
         sys_no, 
         lg );
      for tenure in Tenure_Type loop -- skip all the junk ones
         Print_Gini( 
            f, 
            by_tenure, 
            Censor_String( Tenure_Type'Image( tenure )), 
            sys_no, 
            lg );
      end loop;

   end Print_All_Ginis;
   
   package Gain_Lose_By_Tenure_Printer is new 
      Gain_Lose_By_Tenure.Text_IO;
      
   package Household_Costs_By_Tenure_Printer is new 
      Household_Costs_By_Tenure.Text_IO;
      
   procedure Print_Outputs( 
      filename : String; 
      outputs  : in out Outputs_Rec ) is
      f : File_Type;
   begin
      Put_Line( "Output written to |" & filename & "|" );
      Create( f, Out_File, filename );
      New_Line( f );
      Put_Line( f , "SUMMARY, SYS1" );
      for sn in Summary_Items_Type loop
         Put_Line( f, sn'Img & " : " & Format( outputs.summary_items_1( sn )));
      end loop;
      New_Line( f );
      New_Line( f );
      Put_Line( f , "SUMMARY, SYS2" );
      for sn in Summary_Items_Type loop
         Put_Line( f, sn'Img & " : " & Format( outputs.summary_items_2( sn )));
      end loop;
      New_Line( f );
      New_Line( f );
      -- 
      -- Household_Costs_By_Tenure_Printer.Print(
         -- f,
         -- "budget_table_sys_1",
         -- outputs.costs_by_tenure( 1 ),
         -- 2,
         -- "tenure",
         -- "taxes",
         -- 2009,
         -- 1
         -- );
      -- Household_Costs_By_Tenure_Printer.Print(
         -- f,
         -- "budget_table_sys_2",
         -- outputs.costs_by_tenure( 2 ),
         -- 7,
         -- "tenure",
         -- "taxes",
         -- 2009,
         -- 2
         -- );
      -- --
      -- -- missing columns in poverty tables
      -- -- 
      -- -- Gain/Lose tables
      -- -- 
         -- 
      -- Gain_Lose_By_Tenure_Printer.Print( 
         -- f, 
         -- "gains_by_tenure",
         -- outputs.gains_by_tenure, 
         -- NO_CHANGE_COL, 
         -- "change_income", 
         -- "tenure", 
         -- 2009, 
         -- 21 );
      -- -- FIXME TODO
      -- -- Print_All_Ginis( f, 1, outputs.all_net_incomes_1 );
      -- -- Print_All_Ginis( f, 2, outputs.all_net_incomes_2 );
      Close( f );
   end Print_Outputs;
   
   procedure Initialise_Outputs( outputs : in out Outputs_Rec ) is
   begin
     null;
   end Initialise_Outputs;

   procedure Create_Summary_Statistics( outputs  : in out Outputs_Rec ) is
   use Tabulator_Commons;
      -- express in millions pa & reverse the sign for total cost
      gain : Amount := outputs.summary_statistics( average_gain ) * 12.0/1000.0;
   begin
      if( outputs.summary_statistics( hh_count )) > 0.0 then
         null;
         -- outputs.summary_statistics( average_gain ) := 
            -- outputs.summary_statistics( average_gain ) / outputs.summary_statistics( hh_count ); 
         -- outputs.summary_statistics( percent_gaining ) := 
            -- 100.0 * outputs.summary_statistics( percent_gaining ) / outputs.summary_statistics( hh_count );
         -- outputs.summary_statistics( percent_losing ) := 
            -- 100.0 * outputs.summary_statistics( percent_losing ) / outputs.summary_statistics( hh_count );
         -- outputs.summary_statistics( total_cost ) := gain;
         -- outputs.summary_statistics( change_in_poverty_rate ) := 
            -- outputs.poverty_by_decile( 2 ).totals( people_in_poverty ) - 
            -- outputs.poverty_by_decile( 1 ).totals( people_in_poverty );
         -- outputs.summary_statistics( change_in_gini ) := 
            -- outputs.ineq( 2 ).inequality_measures( be_inequality.Gini ) - 
            -- outputs.ineq( 1 ).inequality_measures( be_inequality.Gini );
      end if;
   end Create_Summary_Statistics;
   
   procedure Add_To_Outputs( 
      hh_ref   : Positive;
      hh       : Model.WSC.Household.Household; 
      year     : Simulation_Years;
      weight   : Amount;
      result_1 : Household_Result; 
      result_2 : Household_Result;
      outputs  : in out Outputs_Rec ) is
         
      breakdown            : Cell_Breakdown;
      column               : Scale_Range;
      exmpl                : Example;
      age_band             : Adult_Age_Band := Get_Age_Band( hh.benefit_units( 1 ).adults( 1 ).age );
      change_in_net_income : Amount;
      sum1                 : Summary_Items_Array;
      sum2                 : Summary_Items_Array;
      -- output               : Outputs_Rec;
      costs_1              : Costs_Array;
      costs_2              : Costs_Array;
      age_of_oldest        : constant Age_Range := hh.Age_Of_Oldest_Member;
      equivalent_income_decile : Decile_Number := Decile_Number'First;
      weighted_people      : constant Amount := weight * Amount( hh.num_people );
   begin
      -- if( not outputs.Contains( year ))then
      --    outputs.Insert( year, output );
      -- end if;
      -- output := outputs.Element( year );
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 .. hh.benefit_units( buno ).num_adults loop
            sum1 := Make_Summary_Items( 
               hh.benefit_units( buno ).adults( adno ), 
               result_1.benefit_units( buno ).people( adno ));
            sum2 := Make_Summary_Items( 
               hh.benefit_units( buno ).adults( adno ), 
               result_2.benefit_units( buno ).people( adno ));
            Log( "summary 1 " & buno'img & " adno " & adno'img & " = " & Summary_Items_Package.To_String( sum1 ));
            Log( "summary 2 " & buno'img & " adno " & adno'img & " = " & Summary_Items_Package.To_String( sum2 ));
            for sn in Summary_Items_Type loop
               outputs.summary_items_1( sn ) := outputs.summary_items_1( sn ) + ( sum1( sn ) * weight );
               outputs.summary_items_2( sn ) := outputs.summary_items_2( sn ) + ( sum2( sn ) * weight );
            end loop;
         end loop;
         outputs.summary_items_1( population ) := outputs.summary_items_1( population ) + ( weight * Amount( hh.benefit_units( buno ).num_children ));
         outputs.summary_items_2( population ) := outputs.summary_items_2( population ) + ( weight * Amount( hh.benefit_units( buno ).num_children ));
      end loop;
      
      Log( "accumulated 1" & Summary_Items_Package.To_String( outputs.summary_items_1 ));
      Log( "accumulated 2 " & Summary_Items_Package.To_String( outputs.summary_items_2 ));
      
      breakdown.tenure := hh.hdata.tenure;
      exmpl.hid := hh.hid;
      exmpl.wave := hh.wave;
      
      --
      -- Costs table
      --
      if( age_of_oldest >= 60 )then
   
         Household_Costs_By_Tenure.Add_Observation( 
                outputs.costs_by_tenure( 1 ),
                hh.hdata.tenure, 
                weight, 
                result_1.res.costs_summary );
         Household_Costs_By_Decile.Add_Observation( 
                outputs.costs_by_decile( 1 ),
                equivalent_income_decile, 
                weight, 
                result_1.res.costs_summary );
         Household_Costs_By_Age_Band.Add_Observation( 
                outputs.costs_by_age_band( 1 ),
                age_band, 
                weight,
                result_1.res.costs_summary );
         Household_Costs_By_Tenure.Add_Observation( 
                outputs.costs_by_tenure( 2 ),
                hh.hdata.tenure, 
                weight, 
                result_2.res.costs_summary );
         Household_Costs_By_Decile.Add_Observation( 
                outputs.costs_by_decile( 2 ),
                equivalent_income_decile, 
                weight, 
                result_2.res.costs_summary );
         Household_Costs_By_Age_Band.Add_Observation( 
                outputs.costs_by_age_band( 2 ),
                age_band, 
                weight,
                result_2.res.costs_summary );
         change_in_net_income := result_2.res.net_income - result_1.res.net_income;
         outputs.summary_statistics( average_gain ) :=
             outputs.summary_statistics( average_gain ) + 
               ( change_in_net_income * weight );     
         outputs.summary_statistics( hh_count ) :=
             outputs.summary_statistics( hh_count ) + 
               weight;     
         outputs.summary_statistics( pers_count ) :=
             outputs.summary_statistics( pers_count ) + 
             weighted_people;
             
         if( change_in_net_income > 0.01 )then
             outputs.summary_statistics( percent_gaining ) :=
                     outputs.summary_statistics( percent_gaining ) + weight;     
         elsif( change_in_net_income < -0.01 )then
             outputs.summary_statistics( percent_losing ) :=
                     outputs.summary_statistics( percent_losing ) + weight;     
         end if;
         column := Gain_Lose_By_Tenure.Get_Col_Range(
               result_1.res.net_income,
               result_2.res.net_income,
               Gain_Lose_Scale,
               pct_change );
   
         Gain_Lose_By_Tenure.Add_Observation( 
               outputs.gains_by_tenure, 
               hh.hdata.tenure, 
               column, 
               weight, 
               result_1.res.summary,
               result_2.res.summary,
               breakdown,
               exmpl );
         Gain_Lose_By_Age_Band.Add_Observation( 
               outputs.gains_by_age_band, 
               age_band, 
               column, 
               weight, 
               result_1.res.summary,
               result_2.res.summary,
               breakdown,
               exmpl );
         Gain_Lose_By_Decile.Add_Observation( 
               outputs.gains_by_decile, 
               equivalent_income_decile, 
               column, 
               weight, 
               result_1.res.summary,
               result_2.res.summary,
               breakdown,
               exmpl );
      end if; -- age of oldest >= 60
      
      --
      --
      --
      -- result( 1 ).poverty_gap := Calculate_Poverty_Gap_Per_Person( hh, outputs.poverty_line_per_person( 1 ), result( 1 ) ); -- note we use the base poverty line all the time       
      -- result( 2 ).poverty_gap := Calculate_Poverty_Gap_Per_Person( hh, outputs.poverty_line_per_person( 1 ), result( 2 ) ); -- even though we calculate the reform one
      -- outputs.Replace( year, output );
      
   end Add_To_Outputs;
  
   -- function Get_Output_For_Year( outa : Outputs_Array_Access; year : Simulation_Years ) return Outputs_Rec is
   -- begin
      -- if( outa.Contains( year ))then
         -- return outa.Element( year );
      -- else
         -- declare
            -- no : Outputs_Rec;
         -- begin
            -- no.is_null := True;
            -- return no;
         -- end;
      -- end if;
   -- end Get_Output_For_Year;
-- 
   -- 
end Model.WSC.Output;
