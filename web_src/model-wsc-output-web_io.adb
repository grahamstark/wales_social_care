with Ada.Calendar;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;

with Model.WSC.Global_Settings;
with Model.WSC.Main_Menu;
with Model.WSC.Summary_Items_DAO;
with Model.WSC.Output.DAO;
with Model.WSC.Stats_Table_Commons;

with Costs_Tabulator.Web_IO;

with T_Utils;
with T_Utils.Standard_Chart_Generator;
with Table_Stats_IO;
with Tabulator.Web_IO;
with Templates_Parser;
with WSC_DB_Data;
with Web_Utils;
with GNATColl.Traces;
with Time_Series_Chart_Generator;
with Standard_Colours;
with Colours;
with Costs_Tabulator.Statistics_IO;

package body Model.WSC.Output.Web_IO is
   
   use Translations;
  
   package euws          renames Model.WSC.Global_Settings; 
   package summary_dao   renames Model.WSC.Summary_Items_DAO;
   package o_dao         renames Model.WSC.Output.DAO;
   package stats_commons renames Model.WSC.Stats_Table_Commons;
   
   package Gain_Lose_By_Tenure_Statistics_IO is new 
      Gain_Lose_By_Tenure.Statistics_IO;
   package Gain_Lose_By_Decile_Statistics_IO is new 
      Gain_Lose_By_Decile.Statistics_IO;
   package Gain_Lose_By_Age_band_Statistics_IO is new 
      Gain_Lose_By_Age_band.Statistics_IO;

   package Costs_By_Tenure_Statistics_IO is new 
       Household_Costs_By_Tenure.Statistics_IO;
   package Costs_By_Decile_Statistics_IO is new 
       Household_Costs_By_Decile.Statistics_IO;
   package Costs_By_Age_band_Statistics_IO is new 
       Household_Costs_By_Age_Band.Statistics_IO;
       
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.OUTPUT.WEB_IO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   function Get_Divisor( item : Summary_Items_Type ) return Rate is 
      divisor : Rate;
   begin
      case item is
      when population .. age80_plus_population |
         combined_benefit_recipients | 
         dla_care_recipients |
         dla_mob_recipients |
         pension_recipients |
         aa_recipients |
         pc_recipients => divisor := 1.0;
      when 
         net_income |
         disposable_income |
         social_care_clients |
         residential_clients |
         non_residential_clients |
         residential_fully_publicly_funded_clients |
         residential_partially_publicly_funded_clients |
         residential_privately_funded_clients |
         
         non_residential_fully_publicly_funded_clients |
         non_residential_partially_publicly_funded_clients |
         non_residential_privately_funded_clients |
         
         health_poor_or_very_poor |
         health_poor |
         health_very_poor => divisor := 1.0;
      when 
         residential_private_funding |
         residential_public_funding |
         non_residential_private_funding |
         non_residential_public_funding |
         dla_mob_cost |
         dla_care_cost |
         pension_cost |
         pc_cost |
         aa_cost |
         public_expenditure_net_of_user_charges_and_other_income |
         private_spend_on_social_care |
         combined_benefit_spend => divisor := 1_000_000.0;
      end case;      
      return divisor;
   end Get_Divisor;

   function Get_Charts_Unit_String( item : Summary_Items_Type ) return String is 
   begin
      case item is
      when population .. age80_plus_population |
         combined_benefit_recipients | 
         dla_care_recipients |
         dla_mob_recipients |
         pension_recipients |
         aa_recipients |
         pc_recipients => return "Counts";
      when 
         net_income |
         disposable_income => return "GBP per week";
      when
         social_care_clients |
         residential_clients |
         non_residential_clients |
         residential_fully_publicly_funded_clients |
         residential_partially_publicly_funded_clients |
         residential_privately_funded_clients |
         
         non_residential_fully_publicly_funded_clients |
         non_residential_partially_publicly_funded_clients |
         non_residential_privately_funded_clients |
         
         health_poor_or_very_poor |
         health_poor |
         health_very_poor => return "Counts";
      when 
         residential_private_funding |
         residential_public_funding |
         non_residential_private_funding |
         non_residential_public_funding |
         dla_mob_cost |
         dla_care_cost |
         pension_cost |
         pc_cost |
         aa_cost |
         public_expenditure_net_of_user_charges_and_other_income |
         private_spend_on_social_care |
         combined_benefit_spend => return "GBP per anum";
      end case;      
   end Get_Charts_Unit_String;
   
   function Draw_Time_Series_Fan_Chart( 
      wsc_run      : Run;
      summary_item : Summary_Items_Type; 
      system       : Pre_Or_Post;
      size         : Chart_Size;
      is_svg       : Boolean ) return Unbounded_String is
      start_wave  : constant Waves := Wave_From_Year( wsc_run.Start_Year );
      end_wave    : constant Waves := Wave_From_Year( wsc_run.End_Year );
      
      subtype Run_Waves is Waves range start_wave .. end_wave;
      subtype Run_Waves_Array is Abs_Waves_Array( Run_Waves );
      
      num_systems : constant Positive := ( if system = pre or system = pct_change or system = abs_change then 1 else 2 );
      units_string : constant String := Get_Charts_Unit_String( summary_Item );
      
      package Waves_TS_Charts is new Time_Series_Chart_Generator(
         Index_Type        => Run_Waves,
         Real_Type         => Amount,
         Time_Series_Array => Run_Waves_Array,
         num_systems       => num_systems
      );
      use Waves_TS_Charts;
      item_string : constant String := Pretty_Print( summary_item ) & "(" & units_string & ")";
      subtitle    : constant String := Year_Number'Image( wsc_run.Start_Year ) & " : " & Year_Number'Image( wsc_run.End_Year );
      data        : Run_Waves_Array;
      chart       : Time_Series_Chart;
   begin
      chart := Waves_TS_Charts.Construct( 
         plotter_url  => Model.WSC.Global_Settings.Charts_URL ,
         title        => "Time Series Chart of " & item_string,
         subtitle     => subtitle,
         start_year   => wsc_run.Start_Year,
         x_axis_label => "Year",
         y_axis_label => item_string,
         ctype        => time_series_fan,
         system       => system );
         
      for stat in mean .. decile_10 loop
            declare
               stat_str : constant String := Prettify_Image( stat'Img );
            begin
               case system is
               when pre =>
                  data := summary_dao.Retrieve( wsc_run, stat, summary_item, 1 )( start_wave .. end_wave );
                  chart.Add_Data_Series( data, stat_str, Standard_Colours.gray_blue, 1 );
               when post =>
                  data := summary_dao.Retrieve( wsc_run, stat, summary_item, 2 )( start_wave .. end_wave );
                  chart.Add_Data_Series( data, stat_str, Standard_Colours.gray_blue, 2 );
               when abs_change | pct_change =>
                  data := summary_dao.Retrieve( wsc_run, stat, summary_item, 3 )( start_wave .. end_wave );
                  chart.Add_Data_Series( data, stat_str, Standard_Colours.gray_blue, 1 );
               when both =>
                  data := summary_dao.Retrieve( wsc_run, stat, summary_item, 1 )( start_wave .. end_wave );
                  chart.Add_Data_Series( data, stat_str, Standard_Colours.light_turquoise, 1 );
                  data := summary_dao.Retrieve( wsc_run, stat, summary_item, 2 )( start_wave .. end_wave );
                  chart.Add_Data_Series( data, stat_str, Standard_Colours.pale_orange, 2 );
               end case;
            end;
      end loop;
      return chart.To_URL( size, is_svg );
   end Draw_Time_Series_Fan_Chart;
   
   
   function Get_Single_Table( 
      wsc_run        : Run;
      summary_item   : Summary_Items_Type;
      do_comparisons : Boolean ) return Unbounded_String is
      start_wave  : constant Waves := Wave_From_Year( wsc_run.Start_Year );
      end_wave    : constant Waves := Wave_From_Year( wsc_run.End_Year );
      
      units_string   : constant String := Get_Charts_Unit_String( summary_Item );
      item_string    : constant String := Pretty_Print( summary_item ) & "(" & units_string & ")";
      subtitle       : constant String := Year_Number'Image( wsc_run.Start_Year ) & " : " & Year_Number'Image( wsc_run.End_Year );
      divisor        : constant Rate := Get_Divisor( summary_item );
      title          : constant String := Pretty_Print( summary_item );
      as_real        : constant Boolean := False; -- can't remember what this was for..
      table          : Unbounded_String;
      year_cells     : Templates_Parser.Vector_Tag;
      pre_cells      : Templates_Parser.Vector_Tag;
      post_cells     : Templates_Parser.Vector_Tag;
      translations   : Translate_Set;
   begin
      Insert( translations, Assoc( "FOOTER", subtitle ));
      Insert( translations, Assoc( "CAPTION", item_string ));
      for y in wsc_run.start_year .. wsc_run.end_year loop
         declare
            wave        : Waves := Wave_From_Year( y );
            wave_str    : constant String := Waves'Image( wave );
            pre_stats   : WSC_DB_Data.Table_Stats;
            post_stats  : WSC_DB_Data.Table_Stats;
            cell        : Unbounded_String;
            popup_title : constant String := title & " : " & Year_Number'Image( y );
            id          : constant String := wave_str & "-" & Summary_Items_Type'Image( summary_item );
         begin
            year_cells := year_cells & Format( y );
            pre_stats := Table_Stats_IO.Retrieve_By_PK( 
               wsc_run.run_id, 
               wsc_run.username, 
               TuS( "summary_items" ), 
               Summary_Items_Type'Pos( summary_item ), 
               -999, 
               TuS( wave_str ), 
               1 );
            pre_cells := pre_cells & stats_commons.Stats_To_Table( pre_stats, popup_title, id, divisor, "", as_real );
            if( do_comparisons )then
               post_stats := Table_Stats_IO.Retrieve_By_PK( 
                  wsc_run.run_id, 
                  wsc_run.username, 
                  TuS( "summary_items" ), 
                  Summary_Items_Type'Pos( summary_item ), 
                  -999, 
                  TuS( wave_str ), 
                  2 );
               post_cells := post_cells & stats_commons.Stats_To_Table( post_stats, popup_title, id, divisor, "", as_real );
            end if;
         end;
      end loop;
      Insert( translations, Assoc( "LABEL", year_cells ));
      Insert( translations, Assoc( "PRE", pre_cells ));
      Insert( translations, Assoc( "DO-COMPARISONS", do_comparisons ));
      if( do_comparisons )then
         Insert( translations, Assoc( "POST", post_cells ));
      end if;
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "single_output_table", translations );      
   end Get_Single_Table;
   
   function Make_Stats_Link( num_iterations : Natural; year : Year_Number; sysno : Positive; do_differences : Boolean ) return String is
      s : Unbounded_String;
      root_str : constant String := Model.WSC.Global_Settings.WSC_Web_Root;
    begin
        if( num_iterations = 1 ) or do_differences then
           return Year_Number'Image( year );
        end if;
        s := s & "<a href='" & root_str & "/output_page/";
        s := s & Year_Number'Image( year ) & "/";
        s := s & "/stats_page/" & Format( sysno );
        s := s & "'>" & Format( year ) & "</a>";
        return TS( s );
   end Make_Stats_Link;

   function Stats_To_Tag( stats : WSC_DB_Data.Table_Stats ) return Templates_Parser.Vector_Tag is
      t : Templates_Parser.Vector_Tag;
   begin
      t := t & Web_Format( stats.rmean_1, en );
      t := t & Web_Format( stats.rmin_1, en );
      t := t & Web_Format( stats.rmax_1, en );
      t := t & Web_Format( stats.rmed_1, en );
      t := t & Web_Format( stats.sddev_1, en );
      t := t & Web_Format( stats.dec1_1, en );
      t := t & Web_Format( stats.dec10_1, en );
      return t;         
   end Stats_To_Tag;


   function Get_Stats_Summary_Page(
      wsc_run : Run;
      sysno   : Positive;
      wave    : Waves ) return Unbounded_String is
         
      translations : Translate_Set;
      tags         : array( Summary_Items_Type ) of Templates_Parser.Vector_Tag;
      stats        : WSC_DB_Data.Table_Stats;
      large_charts : Templates_Parser.Vector_Tag;
      medium_charts : Templates_Parser.Vector_Tag;
      thumbnail_charts : Templates_Parser.Vector_Tag;
      
      svg_large_charts : Templates_Parser.Vector_Tag;
      svg_medium_charts : Templates_Parser.Vector_Tag;
      svg_thumbnail_charts : Templates_Parser.Vector_Tag;
   begin
      for st  in Summary_Items_Type loop
         stats := Table_Stats_IO.Retrieve_By_PK( 
            wsc_run.run_id, 
            wsc_run.username, 
            TuS( "summary_items" ), 
            Summary_Items_Type'Pos( st ), 
            -999, 
            TuS( wave'Img ), 
            sysno );
         tags( st ) := Stats_To_Tag( stats );
         
         large_charts := large_charts & Draw_Time_Series_Fan_Chart( wsc_run, st,  pre, large, False );
         medium_charts := medium_charts & Draw_Time_Series_Fan_Chart( wsc_run, st,  pre, medium, False );
         thumbnail_charts := thumbnail_charts & Draw_Time_Series_Fan_Chart( wsc_run, st,  pre, thumb, False );

         -- svg_large_charts := svg_large_charts & Draw_Time_Series_Fan_Chart( wsc_run, st,  pre, large, True );
         -- svg_medium_charts := svg_medium_charts & Draw_Time_Series_Fan_Chart( wsc_run, st,  pre, medium, True );
         -- svg_thumbnail_charts := svg_thumbnail_charts & Draw_Time_Series_Fan_Chart( wsc_run, st, units_string, pre, thumb, True );
         
      end loop;
      
      
      Insert( translations, Assoc( "NUM-STATS", "7" ));
      
      Insert( translations, Assoc( "LARGE-CHARTS", large_charts )); 
      Insert( translations, Assoc( "MEDIUM-CHARTS", medium_charts )); 
      Insert( translations, Assoc( "THUMBNAIL-CHARTS", thumbnail_charts )); 

      Insert( translations, Assoc( "SVG-LARGE-CHARTS", svg_large_charts )); 
      Insert( translations, Assoc( "SVG-MEDIUM-CHARTS", svg_medium_charts )); 
      Insert( translations, Assoc( "SVG-THUMBNAIL-CHARTS", svg_thumbnail_charts )); 
      
      
      Insert( translations, Assoc( "SOCIAL-CARE-CLIENTS", tags( social_care_clients ))); 
      
      Insert( translations, Assoc( "RESIDENTIAL", tags( residential_clients )));
      Insert( translations, Assoc( "NON-RESIDENTIAL", tags( non_residential_clients )));
      
      Insert( translations, Assoc( "RESIDENTIAL-FULLY-PUBLICLY-FUNDED",     tags( residential_fully_publicly_funded_clients )));
      Insert( translations, Assoc( "RESIDENTIAL-PARTIALLY-PUBLICLY-FUNDED", tags( residential_partially_publicly_funded_clients )));
      Insert( translations, Assoc( "RESIDENTIAL-PRIVATELY-FUNDED",          tags( residential_privately_funded_clients )));
                                   
      Insert( translations, Assoc( "NON-RESIDENTIAL-FULLY-PUBLICLY-FUNDED",     tags( non_residential_fully_publicly_funded_clients )));
      Insert( translations, Assoc( "NON-RESIDENTIAL-PARTIALLY-PUBLICLY-FUNDED", tags( non_residential_partially_publicly_funded_clients )));
      Insert( translations, Assoc( "NON-RESIDENTIAL-PRIVATELY-FUNDED",          tags( non_residential_privately_funded_clients )));
      
      Insert( translations, Assoc( "DLA-CASELOAD", tags( dla_care_recipients )));
      Insert( translations, Assoc( "AA-CASELOAD", tags( aa_recipients )));

      Insert( translations, Assoc( "RESIDENTIAL-PRIVATE-FUNDING", tags( residential_private_funding )));
      Insert( translations, Assoc( "RESIDENTIAL-PUBLIC-FUNDING", tags( residential_public_funding )));
                                    
      Insert( translations, Assoc( "NON-RESIDENTIAL-PRIVATE-FUNDING", tags( non_residential_private_funding )));
      Insert( translations, Assoc( "NON-RESIDENTIAL-PUBLIC-FUNDING", tags( non_residential_public_funding )));
 
      Insert( translations, Assoc( "DLA-CARE-EXPENDITURE", tags( dla_care_cost ))); 
      Insert( translations, Assoc( "DLA-MOBILITY-EXPENDITURE", tags( dla_mob_cost )));
      Insert( translations, Assoc( "AA-EXPENDITURE", tags( aa_cost )));

      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "stats_table", translations );      
   end Get_Stats_Summary_Page;

   function Make_Chart_Popup_URL(
      item           : Summary_Items_Type;
      p_or_p         : Pre_Or_Post ) return Unbounded_String is
   use Ada.Strings.Fixed;
      url : Unbounded_String 
            := TuS( 
               Ada.Characters.Handling.To_Lower(
                  Model.WSC.Global_Settings.WSC_Web_Root & 
                  "chart_popup/single_chart_popup/" & Summary_Items_Type'Image( item ) & "/" &  Pre_Or_Post'Image( p_or_p )));
   begin
      return url;      
   end  Make_Chart_Popup_URL;

   
   function Print_This_Year( wsc_run : Run; this_year : Year_Number ) return Boolean is
      mid_year : Year_Number := wsc_run.start_year + ((wsc_run.end_year - wsc_run.start_year)/2);
   begin
      if( wsc_run.end_year - wsc_run.start_year <= 5 )then
         return True;
      end if;
      return this_year = wsc_run.start_year or this_year = wsc_run.end_year or this_year = mid_year;
   end Print_This_Year;

   function Make_One_Summary_Row( 
      wsc_run    : Run;
      item       : Summary_Items_Type;
      as_real    : Boolean ) return Unbounded_String is
      year_cells   : Templates_Parser.Vector_Tag;
      translations : Translate_Set;
      thumbnail    : constant Unbounded_String := Draw_Time_Series_Fan_Chart( wsc_run, item, pre, thumb, False );
      title        : constant String := Pretty_Print( item );
      divisor      : constant Rate := Get_Divisor( item );
   begin
      Insert( translations, Assoc( "TITLE", title ));
      for y in wsc_run.start_year .. wsc_run.end_year loop
         if Print_This_Year( wsc_run, y ) then
            declare
               wave        : Waves := Wave_From_Year( y );
               wave_str    : constant String := Waves'Image( wave );
               stats       : WSC_DB_Data.Table_Stats;
               cell        : Unbounded_String;
               popup_title : constant String := title & " : " & Year_Number'Image( y );
               id          : constant String := wave_str & "-" & Summary_Items_Type'Image( item );
            begin
               stats := Table_Stats_IO.Retrieve_By_PK( 
                  wsc_run.run_id, 
                  wsc_run.username, 
                  TuS( "summary_items" ), 
                  Summary_Items_Type'Pos( item ), 
                  -999, 
                  TuS( wave_str ), 
                  1 );
               year_cells := year_cells & stats_commons.Stats_To_Table( stats, popup_title, id, divisor, "", as_real );
            end;
         end if;
      end loop;
      Insert( translations, Assoc( "YEAR-CELLS", year_cells ));
      Insert( translations, Assoc( "THUMBNAIL", thumbnail ));
      Insert( translations, Assoc( "POPUP-URL", Make_Chart_Popup_URL( item, pre )));
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "summary_table_row", translations );      
   end Make_One_Summary_Row;

   
   function Make_One_Comparison_Summary_Row( 
      wsc_run    : Run;
      item       : Summary_Items_Type;
      as_real    : Boolean ) return Unbounded_String is
      pre_cells    : Templates_Parser.Vector_Tag;
      post_cells   : Templates_Parser.Vector_Tag;
      classes      : Templates_Parser.Vector_Tag;
      translations : Translate_Set;
      thumbnail    : constant Unbounded_String := Draw_Time_Series_Fan_Chart( wsc_run, item, both, thumb, False );
      title        : constant String := Pretty_Print( item );
      divisor      : constant Rate := Get_Divisor( item );
   begin
      Insert( translations, Assoc( "TITLE", title ));
      for y in wsc_run.start_year .. wsc_run.end_year loop
         if Print_This_Year( wsc_run, y ) then
            declare
               wave        : Waves := Wave_From_Year( y );
               wave_str    : constant String := Waves'Image( wave );
               pre_stats   : WSC_DB_Data.Table_Stats;
               post_stats  : WSC_DB_Data.Table_Stats;
               cell        : Unbounded_String;
               popup_title : constant String := title & " : " & Year_Number'Image( y );
               id          : constant String := wave_str & "-" & Summary_Items_Type'Image( item );
            begin
               pre_stats := Table_Stats_IO.Retrieve_By_PK( 
                  wsc_run.run_id, 
                  wsc_run.username, 
                  TuS( "summary_items" ), 
                  Summary_Items_Type'Pos( item ), 
                  -999, 
                  TuS( wave_str ), 
                  1 );
               pre_cells := pre_cells & stats_commons.Stats_To_Table( pre_stats, popup_title, id, divisor, "pre_display", as_real );
               post_stats := Table_Stats_IO.Retrieve_By_PK( 
                  wsc_run.run_id, 
                  wsc_run.username, 
                  TuS( "summary_items" ), 
                  Summary_Items_Type'Pos( item ), 
                  -999, 
                  TuS( wave_str ), 
                  2 );
               post_cells := post_cells & stats_commons.Stats_To_Table( post_stats, popup_title, id, divisor, "", as_real );
               declare
                  display_class : constant String := ( if pre_stats.rmean_1 /= post_stats.rmean_1 then "changed_value" else "" );
               begin
                  classes := classes & display_class;
               end;
            end;
         end if;
      end loop;
      Insert( translations, Assoc( "DISPLAY-CLASS", classes ));
      Insert( translations, Assoc( "PRE-CELLS", pre_cells ));
      Insert( translations, Assoc( "POST-CELLS", post_cells ));
      Insert( translations, Assoc( "THUMBNAIL", thumbnail ));
      Insert( translations, Assoc( "POPUP-URL", Make_Chart_Popup_URL( item, both )));
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "summary_table_row_pre_post", translations );      
   end Make_One_Comparison_Summary_Row;

   function Get_Chart_Popup(
      wsc_run        : Run;
      item           : Summary_Items_Type;
      p_or_p         : Pre_Or_Post;
      is_svg         : Boolean ) return Unbounded_String is
      translations : Translate_Set;
      chart   : Unbounded_String; 
      title   : constant String := Pretty_Print( item );
   begin
      chart := Draw_Time_Series_Fan_Chart( wsc_run, item, p_or_p, large, is_svg );
      Insert( translations, Assoc( "TITLE", wsc_run.Make_Title ));
      Insert( translations, Assoc( "CHART", chart ));
      
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "chart_popup", translations );      
   end Get_Chart_Popup;


   function Convert_Summary_Results_To_CSV( wsc_run : Run ) return Unbounded_String is
      s : Unbounded_String;
      SEP : constant Character := ',';
      n : constant Natural := ( if wsc_run.num_iterations = 1 then 1 else 7 );
   begin
      s := s & wsc_run.Make_Title( as_html=> False ) & LINE_BREAK;
      s := s & SEP & SEP & SEP;
      for y in wsc_run.start_year .. wsc_run.end_year loop
         s := s & Year_Number'Image( y );
         if( y < wsc_run.end_year )then
            s := s & SEP;
         else
            s := s & LINE_BREAK;
         end if;      
      end loop;
      items:
      for item in Summary_Items_Type loop
         declare
            pre_lines   : array( 1 .. n ) of Unbounded_String;
            post_lines   : array( 1 .. n ) of Unbounded_String;
            item_str : constant String := Pretty_Print( item ) & SEP;
         begin
            
            pre_lines( 1 ) := pre_lines( 1 ) & item_str & "BEFORE" & SEP & "MEAN" & SEP;
            post_lines( 1 ) := post_lines( 1 ) & item_str & "AFTER" & SEP & "MEAN" & SEP;
            if( n = 7 )then
               pre_lines( 2 ) := pre_lines( 2 ) & item_str & "BEFORE" & SEP &"MIN" & SEP;
               pre_lines( 3 ) := pre_lines( 3 ) & item_str & "BEFORE" & SEP &"MAX" & SEP;
               pre_lines( 4 ) := pre_lines( 4 ) & item_str & "BEFORE" & SEP &"DECILE_1" & SEP;
               pre_lines( 5 ) := pre_lines( 5 ) & item_str & "BEFORE" & SEP &"DECILE_10" & SEP;
               pre_lines( 6 ) := pre_lines( 6 ) & item_str & "BEFORE" & SEP &"MEDIAN" & SEP;
               pre_lines( 7 ) := pre_lines( 7 ) & item_str & "BEFORE" & SEP &"STD_DEV" & SEP;
               post_lines( 2 ) := post_lines( 2 ) & item_str & "AFTER" & SEP &"MIN" & SEP;
               post_lines( 3 ) := post_lines( 3 ) & item_str & "AFTER" & SEP &"MAX" & SEP;
               post_lines( 4 ) := post_lines( 4 ) & item_str & "AFTER" & SEP &"DECILE_1" & SEP;
               post_lines( 5 ) := post_lines( 5 ) & item_str & "AFTER" & SEP &"DECILE_10" & SEP;
               post_lines( 6 ) := post_lines( 6 ) & item_str & "AFTER" & SEP &"MEDIAN" & SEP;
               post_lines( 7 ) := post_lines( 7 ) & item_str & "AFTER" & SEP &"STD_DEV" & SEP;
            end if;
            years:
            for y in wsc_run.start_year .. wsc_run.end_year loop
               declare
                  wave        : Waves := Wave_From_Year( y );
                  wave_str    : constant Unbounded_String := TuS( Waves'Image( wave ));
                  pre_stats   : WSC_DB_Data.Table_Stats;
                  post_stats  : WSC_DB_Data.Table_Stats;
               begin
                  pre_stats := Table_Stats_IO.Retrieve_By_PK( 
                     wsc_run.run_id, 
                     wsc_run.username, 
                     TuS( "summary_items" ), 
                     Summary_Items_Type'Pos( item ), 
                     -999, 
                     wave_str, 
                     1 );
                  pre_lines( 1 ) := pre_lines( 1 ) & Amount'Image( pre_stats.rmean_1 );
                  if( n = 7 )then
                     pre_lines( 2 ) := pre_lines( 2 ) & Amount'Image( pre_stats.rmin_1 );
                     pre_lines( 3 ) := pre_lines( 3 ) & Amount'Image( pre_stats.rmax_1 );
                     pre_lines( 4 ) := pre_lines( 4 ) & Amount'Image( pre_stats.dec1_1 );
                     pre_lines( 5 ) := pre_lines( 5 ) & Amount'Image( pre_stats.dec10_1 );
                     pre_lines( 6 ) := pre_lines( 6 ) & Amount'Image( pre_stats.rmed_1 ); 
                     pre_lines( 7 ) := pre_lines( 7 ) & Amount'Image( pre_stats.sddev_1 ); 
                  end if;                     
                  post_stats := Table_Stats_IO.Retrieve_By_PK( 
                     wsc_run.run_id, 
                     wsc_run.username, 
                     TuS( "summary_items" ), 
                     Summary_Items_Type'Pos( item ), 
                     -999, 
                     wave_str, 
                     2 );
                  post_lines( 1 ) := post_lines( 1 ) & Amount'Image( post_stats.rmean_1 );
                  if( n = 7 )then
                     post_lines( 2 ) := post_lines( 2 ) & Amount'Image( post_stats.rmin_1 );
                     post_lines( 3 ) := post_lines( 3 ) & Amount'Image( post_stats.rmax_1 );
                     post_lines( 4 ) := post_lines( 4 ) & Amount'Image( post_stats.dec1_1 );
                     post_lines( 5 ) := post_lines( 5 ) & Amount'Image( post_stats.dec10_1 );
                     post_lines( 6 ) := post_lines( 6 ) & Amount'Image( post_stats.rmed_1 ); 
                     post_lines( 7 ) := post_lines( 7 ) & Amount'Image( post_stats.sddev_1 ); 
                  end if;
               end;
               for i in 1 .. n loop
                  if( y < wsc_run.end_year )then
                     post_lines( i ) := post_lines( i ) & SEP;                     
                     pre_lines( i ) := pre_lines( i ) & SEP;                     
                  end if;
               end loop;
            end loop years;
            for i in 1 .. n loop
               s := s & pre_lines( i ) & LINE_BREAK;                     
            end loop;
            for i in 1 .. n loop
               s := s & post_lines( i ) & LINE_BREAK;                     
            end loop;
         end; -- declare lines
      end loop items;
      return s;
   end Convert_Summary_Results_To_CSV;
   

   
   function Get_All_Years_Summary( wsc_run : Run ) return Unbounded_String is
      translations : Translate_Set;
      year_cells            : Templates_Parser.Vector_Tag;
      subsets : array( Summary_Items_Group ) of Templates_Parser.Vector_Tag;
      group   : Summary_Items_Group;
      as_real : Boolean := False;
   begin
      for y in wsc_run.start_year .. wsc_run.end_year loop
         if Print_This_Year( wsc_run, y ) then
            year_cells := year_cells &  Year_Number'Image( y );
         end if;
      end loop;
      for item in Summary_Items_Type loop
         group := Group_From_Item( item );
         if( group in population .. health )then -- same pre- and post
            subsets( group ) := subsets( group ) & Make_One_Summary_Row( wsc_run, item, as_real ); 
         else
            subsets( group ) := subsets( group ) & Make_One_Comparison_Summary_Row( wsc_run, item, as_real ); 
         end if;
      end loop;
      Insert( translations, Assoc( "ROOT", Model.WSC.Global_Settings.WSC_Web_Root ));
      Insert( translations, Assoc( "TITLE", wsc_run.Make_Title ));
      Insert( translations, Assoc( "YEAR-CELLS", year_cells )); 
      for g in Summary_Items_Group loop
         Insert( translations, Assoc(  Summary_Items_Group'Image( g ), subsets( g )));
      end loop;
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "summary_table_full", translations );      
   end Get_All_Years_Summary;

   function Get_Summary_Table( 
      outputs        : Outputs_Rec;
      lang           : Languages        := Languages'First ) return Unbounded_String is
   use Templates_Parser;
       translations : Translate_Set;                                                   
   begin
      Insert( translations, Assoc( "ROOT", Model.WSC.Global_Settings.WSC_Web_Root ));
      Insert( translations, 
         Assoc( "GAIN-LOSE-SUMMARY", 
                Model.WSC.Output.Gain_Lose_Web_IO.Get_Summary( outputs, lang )));
      Insert( translations, 
         Assoc( "BUDGET-SUMMARY", 
                Model.WSC.Output.Budget_Web_IO.Get_Summary( outputs, lang )));
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "summary_output", translations );      
   end Get_Summary_Table;
   
   function Get_Summary_Control_Section(
      sysno          : Positive      := 1;
      do_differences : Boolean            := False;
      lang           : Languages          := Languages'First) return Unbounded_String is
         
      package Breakdown_Target_T is new T_Utils( T => Disaggregated_Breakdown_Target, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package Breakdown_Target_T_Web_IO is new Breakdown_Target_T.Web_IO;
      
      package System_Number_T is new T_Utils( T => System_Number, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package System_Number_T_Web_IO is new System_Number_T.Web_IO;

      function Pretty_Print( breakdown : Disaggregated_Breakdown_Target ) return String is
      begin
         return Prettify_Image( Censor_String( Disaggregated_Breakdown_Target'Image( breakdown )));
      end Pretty_Print;
      
      function Pretty_Print( sysno : System_Number ) return String is
      begin
          case sysno is
             when 1 => return Lookup( "Base System", lang );
             when 2 => return Lookup( "Reformed System", lang );
          end case;
       end Pretty_Print;

      s : Unbounded_String;
      checkbox : Unbounded_String;
      dd : Boolean := do_differences;
      
   begin
      s := s & "<table class='control_section'>" & LINE_BREAK;
      
      s := s & "   <tr><td><label for='sysno'>"  & Lookup( "System to display?", lang ) & "</label></td><td>" & LINE_BREAK;
      s := s & System_Number_T_Web_IO.Make_Select( 
         "sysno", sysno, Pretty_Print'Access);
      s := s & "   </td></tr>" & LINE_BREAK;
      
      s := s & "   <tr><td><label for='do_differences'>"  & Lookup( "Print differences from base?", lang ) & "</label></td><td>" & LINE_BREAK;
      WSC_HTML.Make_One_Input( "do_differences", checkbox, dd, False );
      s := s & checkbox;
      s := s & "   </td></tr>" & LINE_BREAK;
      
      s := s & "  <tr><td>" & LINE_BREAK;
      s := s & "     <input id='Run_Submit_Button' type='submit' value='" & Lookup( "Redraw", lang ) & "' name='Redraw'  />" & LINE_BREAK;
      s := s & "  </td></tr>" & LINE_BREAK;
      s := s & "</table>" & LINE_BREAK;
      return s;
   end Get_Summary_Control_Section;
   
   function Get_Budget_Control_Section(
      sysno          : System_Number      := 1;
      breakdown      : Disaggregated_Breakdown_Target   := Disaggregated_Breakdown_Target'First;
      print_counts   : Boolean            := False;
      do_differences : Boolean            := False;
      lang           : Languages          := Languages'First) return Unbounded_String is
         
      package Breakdown_Target_T is new T_Utils( T => Disaggregated_Breakdown_Target, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package Breakdown_Target_T_Web_IO is new Breakdown_Target_T.Web_IO;
      
      package System_Number_T is new T_Utils( T => System_Number, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package System_Number_T_Web_IO is new System_Number_T.Web_IO;

      function Pretty_Print( breakdown : Disaggregated_Breakdown_Target ) return String is
      begin
         return Lookup( Censor_String( Disaggregated_Breakdown_Target'Image( breakdown )), lang );
      end Pretty_Print;
      
      function Pretty_Print( sysno : System_Number ) return String is
      begin
          case sysno is
             when 1 => return Lookup( "Base System", lang );
             when 2 => return Lookup( "Reformed System", lang );
          end case;
       end Pretty_Print;

      s : Unbounded_String;
      checkbox : Unbounded_String;
      pc : Boolean := print_counts;
      dd : Boolean := do_differences;
      
   begin
      s := s & "<table class='control_section'>" & LINE_BREAK;
      
      s := s & "   <tr><td><label for='breakdown'>"  & Lookup( "Break Down By?", lang ) & "</label></td><td>" & LINE_BREAK;
      s := s & Breakdown_Target_T_Web_IO.Make_Select( 
         "breakdown", breakdown, Pretty_Print'Access );
      s := s & "   </td></tr>" & LINE_BREAK;

      s := s & "   <tr><td><label for='sysno'>"  & Lookup( "System to display?", lang ) & "</label></td><td>" & LINE_BREAK;
      s := s & System_Number_T_Web_IO.Make_Select( 
         "sysno", sysno, Pretty_Print'Access);
      s := s & "   </td></tr>" & LINE_BREAK;
      
      s := s & "   <tr><td><label for='print_counts'>"  & Lookup( "Print caseloads?", lang ) & "</label></td><td>" & LINE_BREAK;
      WSC_HTML.Make_One_Input( "print_counts", checkbox, pc, False );
      s := s & checkbox;
      s := s & "   </td></tr>" & LINE_BREAK;

      s := s & "   <tr><td><label for='do_differences'>"  & Lookup( "Print differences from base?", lang ) & "</label></td><td>" & LINE_BREAK;
      WSC_HTML.Make_One_Input( "do_differences", checkbox, dd, False );
      s := s & checkbox;
      s := s & "   </td></tr>" & LINE_BREAK;
      
      s := s & "  <tr><td>" & LINE_BREAK;
      s := s & "     <input id='Run_Submit_Button' type='submit' value='" & Lookup( "Redraw", lang ) & "' name='Redraw'  />" & LINE_BREAK;
      s := s & "  </td></tr>" & LINE_BREAK;
      s := s & "</table>" & LINE_BREAK;
      return s;
   end Get_Budget_Control_Section;
   
   
   function Get_Budget_Table(      
      wsc_run        : Run;
      wave           : Waves;
      sysno          : System_Number      := 1;
      breakdown      : Disaggregated_Breakdown_Target   := Disaggregated_Breakdown_Target'First;
      print_counts   : Boolean            := False;
      do_differences : Boolean            := False ) return Unbounded_String is
      name       : constant String := 
            "Budget Table for " & Prettify_Image( Disaggregated_Breakdown_Target'Image( breakdown )) & 
            " Year " & Year_Number'Image( Year_From_Wave( wave ));
      table      : Unbounded_String;
      l_sysno    : constant Positive := ( if do_differences then 3 else sysno );
      units_string : constant String := ( if print_counts then "Counts of Households" else "&#163;m per year" );
   begin
      case breakdown is
         when by_tenure =>
            table := Costs_By_Tenure_Statistics_IO.To_String( 
               wsc_run, 
               wave, 
               o_dao.COSTS_BY_TENURE_KEY, 
               l_sysno,
               print_counts,
               name, 
               name & "( " & units_string & ")" );
         when by_age_of_head =>
            table := Costs_By_Age_Band_Statistics_IO.To_String( 
               wsc_run, 
               wave, 
               o_dao.COSTS_BY_AGE_BAND_KEY, 
               l_sysno,
               print_counts,
               name, 
               name & "( " & units_string & ")" );
      end case;            
      return table;      
   end Get_Budget_Table;

   
   function Get_Gain_Lose_Table(
      wsc_run : Run;
      wave    : Waves;
      which   : Breakdown_Target ) return Unbounded_String is
      name       : constant String := 
         "Gain/Lose Table for " & Prettify_Image( Breakdown_Target'Image( which )) & 
            " Year " & Year_Number'Image( Year_From_Wave( wave )) & "(Counts of Households)";
      table      : Unbounded_String;
   begin
      case which is
         when no_breakdown =>
            table := Null_Unbounded_String;
         when by_tenure =>
            table := Gain_Lose_By_Tenure_Statistics_IO.To_String( wsc_run, wave, o_dao.GAIN_LOSE_BY_TENURE_KEY, name, name );
         when by_decile =>
            table := Gain_Lose_By_Decile_Statistics_IO.To_String( wsc_run, wave, o_dao.GAIN_LOSE_BY_DECILE_KEY, name, name );
         when by_age_of_head =>
            table := Gain_Lose_By_Age_band_Statistics_IO.To_String( wsc_run, wave, o_dao.GAIN_LOSE_BY_AGE_BAND_KEY, name, name );
      end case;            
      return table;      
   end Get_Gain_Lose_Table;
      
   function Get_Gain_Lose_Control_Section(
      breakdown        : Disaggregated_Breakdown_Target   := Disaggregated_Breakdown_Target'First;
      comp_cell        : Compare_Cell       := current_cell;
      cell_op          : Cell_Compare_Type  := counter;
      value_To_Use     : Summary_Items_Type := disposable_income;
      lang             : Languages := Languages'First;
      advanced_version : Boolean := False ) return Unbounded_String is
        
      package Summary_Items_Type_T is new T_Utils( T => Summary_Items_Type, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package Summary_Items_Type_T_Web_IO is new Summary_Items_Type_T.Web_IO;
      function Pretty_Print is new Translations.Lookup_V( T=> Summary_Items_Type );
      
      package Cell_Compare_Type_T is new T_Utils( T => Cell_Compare_Type, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package Cell_Compare_Type_T_Web_IO is new Cell_Compare_Type_T.Web_IO;
      function Pretty_Print is new Translations.Lookup_V( T=> Cell_Compare_Type );
      
      package Breakdown_Target_T is new T_Utils( T => Disaggregated_Breakdown_Target, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package Breakdown_Target_T_Web_IO is new Breakdown_Target_T.Web_IO;
      
      function Pretty_Print( breakdown : Disaggregated_Breakdown_Target ) return String is
      begin
         return Lookup( Censor_String( Disaggregated_Breakdown_Target'Image( breakdown )), lang);
      end Pretty_Print;

      package Compare_Cell_T is new T_Utils( T => Compare_Cell, Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type );
      package Compare_Cell_T_Web_IO is new Compare_Cell_T.Web_IO;
      function Pretty_Print is new Translations.Lookup_V( T=> Compare_Cell );   
         
       -- 
      function Pretty_Print( v : Compare_Cell ) return String is
      begin
         return Pretty_Print( v, lang );
      end  Pretty_Print;
      
      function Pretty_Print( v : Cell_Compare_Type ) return String is
      begin
         return Pretty_Print( v, lang );
      end  Pretty_Print;
      
      function Pretty_Print( v : Summary_Items_Type ) return String is
      begin
         return Pretty_Print( v, lang );
      end  Pretty_Print;
        
      s : Unbounded_String;
   begin
      s := s & "<table class='control_section'>" & LINE_BREAK;
      
      s := s & "   <tr><td>"  & Lookup( "Break Down By?", lang ) & "</td><td>" & LINE_BREAK;
      
      s := s & Breakdown_Target_T_Web_IO.Make_Select( 
         "breakdown", breakdown, Pretty_Print'Access);
     
      s := s & "   </td></tr>" & LINE_BREAK;
      
      s := s & "   <tr><td>" & Lookup( "Express table as?", lang ) & "</td><td>" & LINE_BREAK;
      s := s & Compare_Cell_T_Web_IO.Make_Select( 
         "comp_cell", comp_cell, Pretty_Print'Access);
      s := s & "   </td></tr>" & LINE_BREAK;

      if( advanced_version )then
         s := s & "   <tr><td>"& Lookup( "Item shown in cells", lang ) & "</td><td>"  & LINE_BREAK;
         s := s & Summary_Items_Type_T_Web_IO.Make_Select( 
            "value_to_use", value_to_use, Pretty_Print'Access );
         s := s & "   </td></tr>" & LINE_BREAK;

         s := s & "  <tr><td>" & Lookup( "Compare ", lang ) & "</td><td>"  &  LINE_BREAK;
         s := s & Cell_Compare_Type_T_Web_IO.Make_Select( 
            "cell_op", cell_op, Pretty_Print'Access );
         
         s := s & "  </td></tr>" & LINE_BREAK;
      end if;
      s := s & "  <tr><td>" & LINE_BREAK;
      s := s & "     <input id='Run_Submit_Button' type='submit' value='" & Lookup( "Redraw", lang ) & "' name='Redraw'  />" & LINE_BREAK;
      s := s & "  </td></tr>" & LINE_BREAK;
      s := s & "</table>" & LINE_BREAK;
      return s;
   end Get_Gain_Lose_Control_Section;
     
   function Make_Year_Menu(
      start_year          : Year_Number;
      end_year            : Year_Number;
      which_year          : Year_Number;
      include_no_year     : Boolean := False ) return Unbounded_String is
      s : Unbounded_String;
      root_str : constant String := Model.WSC.Global_Settings.WSC_Web_Root;
   begin
      s := s & "<div id='modelNavSub'>" & LINE_BREAK;
      s := s & "  <ul>" & LINE_BREAK; 
      if( include_no_year )then
            if( which_year = Year_Number'First )then
               s := s & "    <li class='on'>All Years Summary</li>" & LINE_BREAK;
            else
               declare
                  year_str  : String := Format( Year_Number'First );
               begin
                  s := s & "    <li><a href='" & root_str & "/output_page/"& year_str & "'>All Years Summary</a></li>" & LINE_BREAK;
               end;
            end if;
      end if;
      for y in start_year .. end_year loop
         declare
            year_str  : String := Format( y );
         begin
            if( y = which_year )then
               s := s & "    <li class='on'>" & year_str & "</li>" & LINE_BREAK;
            else
               s := s & "    <li><a href='" & root_str & "/output_page/"& year_str & "'>" & year_str & "</a></li>" & LINE_BREAK;
            end if;
         end;
      end loop;
      s := s & "</ul>" & LINE_BREAK;
      s := s & "</div>"& LINE_BREAK;
      return s;
   end Make_Year_Menu;

   
   function Get_Output_Menu( 
      start_year : Year_Number;
      end_year   : Year_Number;
      which_year : Year_Number;
      which_page : Output_Page_Type;
      lang       : Languages ) return Unbounded_String is
      s : Unbounded_String;
      no_year : Boolean := True; -- which_page = summary_page;
      root_str : constant String := Model.WSC.Global_Settings.WSC_Web_Root;
   begin
      s := s & Make_Year_Menu( start_year, end_year, which_year, no_year );  
      if( which_year < start_year )then
         return s;
      end if;
      s := s & "<div id='modelNavSub'>" & LINE_BREAK;
      
      s := s & "  <ul>" & LINE_BREAK; 
      for b in gain_lose_page .. examples_page loop
         -- exit when b >= inequality_page;
         declare
            k : String := Censor_String( Output_Page_Type'Image( b ));
            url : String := Format( which_year ) & "/ " & k;
            text : String := Lookup( k, lang );
         begin
            if( b /= which_page )then
               s := s & "      <li><a href='" & root_str & "/output_page/" & url & "/'>" & text & "</a></li>" & LINE_BREAK;
            else
               s := s & "      <li class='on'>" & text & "</li>" & LINE_BREAK;
            end if;
         end;
      end loop;
      s := s & "   </ul>"& LINE_BREAK;
      s := s & "</div>"& LINE_BREAK;
      return s;  
   end Get_Output_Menu;
    
   function Get_Output_Page(  title           : Unbounded_String;
                              translations    : Translate_Set;
                              which_page      : Output_Page_Type;
                              start_year      : Year_Number;
                              end_year        : Year_Number;
                              which_year      : Year_Number;   
                              breadcrumb      : Unbounded_String;
                              control_section : Unbounded_String;
                              gallery         : Unbounded_String;
                              content         : Unbounded_String;
                              lang            : Languages ) return Unbounded_String is
   use Templates_Parser;
   use Model.WSC.Main_Menu;
       full_translations    : Translate_Set := translations; 
       menu : Unbounded_String;
   begin
      menu := Get_Main_Menu( output_page, False, lang ) & LINE_BREAK & 
         Get_Output_Menu( start_year, end_year, which_year, which_page, lang );
         
      Insert( full_translations, Assoc( "TITLE", Title ));
      
      Insert( full_translations, Assoc( "TEMPLATE_ROOT", euws.template_components_path ));
      Insert( full_translations, Assoc( "LANG", Lang_Str( lang )));
      Insert( full_translations, Assoc( "ROOT", euws.WSC_Web_Root ));
      Insert( full_translations, Assoc( "SEP", euws.Dir_Separator ));
      Insert( full_translations, Assoc( "MODEL-MENU", menu ));
      Insert( full_translations, Assoc( "BREADCRUMB", breadcrumb ));
      
      Insert( full_translations, Assoc( "GALLERY-SECTION", gallery ));
      Insert( full_translations, Assoc( "CONTENT-SECTION", content ));
      Insert( full_translations, Assoc( "IS-OUTPUT-PAGE", True ));
      Insert( full_translations, Assoc( "CONTROL-SECTION", control_section ));
      Insert( full_translations, Assoc( "WHICH-YEAR", Censor_String( which_year'Img )));
      Insert( full_translations, Assoc( "WHICH_PAGE", Censor_String( Output_Page_Type'Image( which_page )))); 
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "output", translations );      
   end Get_Output_Page;

end  Model.WSC.Output.Web_IO;
