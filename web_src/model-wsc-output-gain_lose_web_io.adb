with Ada.Containers.Hashed_Sets;

with Templates_Parser;
with GNATColl.Traces;


with Costs_Tabulator.Web_IO;
with Model.WSC.Global_Settings;
with General_Chart_Constants;
with Tabulator.Web_IO;

with Web_Utils;
with Model.WSC.Main_Menu;
with WSC_Enums;


package body Model.WSC.Output.Gain_Lose_Web_IO is
   
   use Translations;
   use WSC_Enums;
   
   package euws renames Model.WSC.Global_Settings; 
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.OUTPUT.GAIN_LOSE_WEB_IO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
   function Fmt( a : Amount; cells_as_ints : Boolean; lang : Languages ) return String is
   begin
      if( cells_as_ints )then
         return Web_Format( Integer( a ), lang );
      else
         return Web_Format( a, lang );
      end if;
   end Fmt;

   --
   -- FIXME MAKE THESE GENERIC !!!!! --- 
   --
   
   function Format_One_Tenure_Cell( 
      complete_table : Gain_Lose_By_Tenure.Table_Type;
      row            : Tenure_Type; 
      col            : Scale_Range; 
      a              : Amount; 
      ctype          : Type_Of_Cell;
      cells_as_ints   : Boolean;
      lang         : Languages )  return String is
      ex_count : constant Natural := complete_table.Get_Num_Examples( row, col );
   begin
      if( ctype /= standard_cell ) or ( ex_count = 0 )then
         return Fmt( a, cells_as_ints, lang );
      else
         declare
            root_str : constant String := Model.WSC.Global_Settings.WSC_Web_Root;
            exmpl    : Example := complete_table.Get_Example( row, col, 1 );
            s        : Unbounded_String;
            hhstr    : constant String := Sernum_Value'Image( exmpl.hid );
            wavestr  : constant String := Waves'Image( exmpl.wave );
            cellstr  : constant String :=  "<a href='" & root_str & "/example/" & hhstr( 2 .. hhstr'Length ) & 
                      "/" & wavestr &
               "' onclick='ExampleWindow( this.href ); return false;'>" & Fmt( a, cells_as_ints, lang ) & "</a>";
         begin
            return cellstr;
         end;
      end if;
   end Format_One_Tenure_Cell;

   function Format_One_Decile_Cell( 
      complete_table : Gain_Lose_By_Decile.Table_Type;
      row            : Decile_Number; 
      col            : Scale_Range; 
      a              : Amount; 
      ctype          : Type_Of_Cell;
      cells_as_ints   : Boolean;
      lang         : Languages )  return String is
      ex_count : constant Natural := complete_table.Get_Num_Examples( row, col );
      root_str : constant String := Model.WSC.Global_Settings.WSC_Web_Root;
   begin
      if( ctype /= standard_cell ) or ( ex_count = 0 )then
         return Fmt( a, cells_as_ints, lang );
      else
         declare
            exmpl   : Example := complete_table.Get_Example( row, col, 1 );
            s       : Unbounded_String;
            hhstr   : constant String := Sernum_Value'Image( exmpl.hid );
            wavestr : constant String := Waves'Image( exmpl.wave );
            cellstr : constant String :=  "<a href='" & root_str & "/example/" & hhstr( 2 .. hhstr'Length ) & 
                      "/" & wavestr &
               "' onclick='ExampleWindow( this.href ); return false;'>" & Fmt( a, cells_as_ints, lang ) & "</a>";
         begin
            return cellstr;
         end;
      end if;
   end Format_One_Decile_Cell;
   
   function Format_One_Age_Band_Cell( 
      complete_table : Gain_Lose_By_Age_Band.Table_Type;
      row            : Age_Band_Type; 
      col            : Scale_Range; 
      a              : Amount; 
      ctype          : Type_Of_Cell;
      cells_as_ints   : Boolean;
      lang         : Languages )  return String is
      ex_count : constant Natural := complete_table.Get_Num_Examples( row, col );
      root_str : constant String := Model.WSC.Global_Settings.WSC_Web_Root;
   begin
      if( ctype /= standard_cell ) or ( ex_count = 0 )then
         return Fmt( a, cells_as_ints, lang );
      else
         declare
            exmpl   : Example := complete_table.Get_Example( row, col, 1 );
            s       : Unbounded_String;
            hhstr   : constant String := Sernum_Value'Image( exmpl.hid );
            wavestr : constant String := Waves'Image( exmpl.wave );
            cellstr : constant String :=  "<a href='" & root_str & "/example/" & hhstr( 2 .. hhstr'Length ) & 
                      "/" & wavestr &
               "' onclick='ExampleWindow( this.href ); return false;'>" & Fmt( a, cells_as_ints, lang ) & "</a>";
         begin
            return cellstr;
         end;
      end if;
   end Format_One_Age_Band_Cell;

   
   package Gain_Lose_By_Tenure_Web_IO is new 
      Gain_Lose_By_Tenure.Web_IO( 
         Languages       => Languages,
         Lookup          => Lookup,
         Format_One_Cell => Format_One_Tenure_Cell );
   package Gain_Lose_By_Decile_Web_IO is new 
      Gain_Lose_By_Decile.Web_IO(
         Languages       => Languages,
         Lookup          => Lookup,
         Format_One_Cell => Format_One_Decile_Cell
      );
   package Gain_Lose_By_Age_band_Web_IO is new 
      Gain_Lose_By_Age_band.Web_IO(
         Languages       => Languages,
         Lookup          => Lookup,
         Format_One_Cell => Format_One_Age_Band_Cell
      );

   function Get_Summary(
      outputs       : Outputs_Rec; 
      lang          : Languages  := Languages'First ) return Unbounded_String is
   use Gain_Lose_By_Decile_Web_IO;
   use Templates_Parser;
      chart : Unbounded_String;
      translations : Translate_Set;
   begin
      chart := Make_Chart_By_Row( 
                        tab           => outputs.gains_by_decile, 
                        title         => Lookup( "Average Gains by Income Decile", lang ),
                        subtitle      => Lookup( "&#163;s per month", lang ),
                        chart_type    => General_Chart_Constants.bar,
                        chart_size    => General_Chart_Constants.medium,
                        cell_op       => average_change,
                        x_axis_label  => Lookup( "Income Decile", lang ),
                        value_to_use  => disposable_income,
                        lang          => lang );
                        
      if outputs.summary_statistics( average_gain ) > 0.01 then
         Insert( translations, Assoc( "GAIN-LOSE-ARROW", "up-good" ));
      elsif outputs.summary_statistics( average_gain ) < -0.01 then
         Insert( translations, Assoc( "GAIN-LOSE-ARROW", "down-bad" ));  
      else
         Insert( translations, Assoc( "GAIN-LOSE-ARROW", "nc" ));
      end if;
      
      Insert( translations, Assoc( "ROOT", euws.WSC_Web_Root )); 
      Insert( translations, Assoc( "MORE", Lookup( "more", lang ))); 
      Insert( translations, Assoc( "BAD-GOOD-CLASS", GL_Class( outputs.summary_statistics( average_gain ), True )));
      Insert( translations, Assoc( "TITLE", Lookup( "Gains and Losses", lang )));
      Insert( translations, Assoc( "CHART", chart ));
      Insert( translations, Assoc( "AVERAGE-GAIN-STR", Lookup( "Average Gain", lang )));
      Insert( translations, Assoc( "AVERAGE-GAIN", 
         GL_Format( outputs.summary_statistics( average_gain ), True, lang )));

      Insert( translations, Assoc( "PERCENT-GAINING-STR", Lookup( "Percent Gaining", lang )));
      Insert( translations, Assoc( "UNITS-STR", Lookup( "&#163;s per month", lang )));
      Insert( translations, Assoc( "PERCENT-GAINING", 
         Web_Format( outputs.summary_statistics( percent_gaining ), lang )));
      
      Insert( translations, Assoc( "PERCENT-LOSING-STR", Lookup( "Percent Losing", lang )));
      Insert( translations, Assoc( "PERCENT-LOSING", 
         Web_Format( outputs.summary_statistics( percent_losing ), lang )));

      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "gain_lose_summary", translations );      
   end Get_Summary;
      
   function Get_Gain_Lose_Chart_Grid(
      outputs       : Outputs_Rec; 
      breakdown     : Breakdown_Target   := no_breakdown;
      lang          : Languages          := Languages'First ) return Unbounded_String is
   use Templates_Parser;
         translations : Translate_Set;
         title      : Unbounded_String;
         subtitle   : Unbounded_String;
         gal_str    : Unbounded_String;
         thumb_list : Vector_Tag;
   begin
      case breakdown is
         when by_tenure =>
             declare
                use Gain_Lose_By_Tenure_Web_IO;
             begin
                for tt in Tenure_Type loop
                  for use_totals in False .. True loop
                     title := TuS( "" ) & Lookup( Censor_String( Tenure_Type'Image( tt )), lang );
                     subtitle := TuS(" ");
                     if( use_totals )then
                        subtitle := subtitle & " " & Lookup( "totals", lang );
                     end if;
                     gal_str:= TuS( "         <li>" ) & LINE_BREAK;
                     gal_str:= gal_str& "              <a class='thumb' href='";
                     gal_str:= gal_str& 
                     Make_Chart_By_Col( 
                        tab           => outputs.gains_by_tenure, 
                        title         => TS( title ),
                        subtitle      => TS( subtitle ),
                        row_target    => tt,
                        chart_type    => General_Chart_Constants.bar,
                        chart_size    => General_Chart_Constants.large,
                        value_to_use  => disposable_income,
                        use_totals    => use_totals,
                        lang          => lang );
                     gal_str:= gal_str& "' title='" & title & "' >" & LINE_BREAK;  
                     gal_str:= gal_str& "<img src='" &  
                     Make_Chart_By_Col( 
                        tab           => outputs.gains_by_tenure, 
                        title         => TS( title ),
                        subtitle      => TS( subtitle ),
                        value_to_use  => disposable_income,
                        row_target    => tt,
                        chart_type    => General_Chart_Constants.bar,
                        chart_size    => General_Chart_Constants.thumb,
                        use_totals    => use_totals,
                        lang          => lang );
                     gal_str:= gal_str& "' alt='" & title & "' />" & LINE_BREAK;  
                     gal_str:= gal_str& "            </a>" & LINE_BREAK;
                     gal_str:= gal_str& "         </li>" & LINE_BREAK;
                     thumb_list := thumb_list & gal_str;
                  end loop;
               end loop;
            end;
            when by_decile =>
              declare
                 use Gain_Lose_By_Decile_Web_IO;
              begin
                 for tt in Decile_Number loop
                   for use_totals in False .. True loop
                      title := TuS( "" ) & 
                           Lookup( "Decile", lang ) & " " &
                           Lookup( Censor_String( Decile_Number'Image( tt )), lang );
                      subtitle := TuS(" ");
                      if( use_totals )then
                         subtitle := subtitle & " " & Lookup( "totals", lang );
                      end if;
                      gal_str:= TuS( "         <li>" ) & LINE_BREAK;
                      gal_str:= gal_str& "              <a class='thumb' href='";
                      gal_str:= gal_str& 
                      Make_Chart_By_Col( 
                         tab           => outputs.gains_by_decile, 
                         title         => TS( title ),
                         subtitle      => TS( subtitle ),
                         value_to_use  => disposable_income,
                         row_target    => tt,
                         chart_type    => General_Chart_Constants.bar,
                         chart_size    => General_Chart_Constants.large,
                         use_totals    => use_totals,
                         lang          => lang );
                      gal_str:= gal_str& "' title='" & title & "' >" & LINE_BREAK;  
                      gal_str:= gal_str& "<img src='" &  
                      Make_Chart_By_Col( 
                         tab           => outputs.gains_by_decile, 
                         title         => TS( title ),
                         subtitle      => TS( subtitle ),
                         value_to_use  => disposable_income,
                         row_target    => tt,
                         chart_type    => General_Chart_Constants.bar,
                         chart_size    => General_Chart_Constants.thumb,
                         use_totals    => use_totals,
                         lang          => lang );
                      gal_str:= gal_str& "' alt='" & title & "' />" & LINE_BREAK;  
                      gal_str:= gal_str& "            </a>" & LINE_BREAK;
                      gal_str:= gal_str& "         </li>" & LINE_BREAK;
                      thumb_list := thumb_list & gal_str;
                   end loop;
                end loop;
             end;
         when by_age_of_head =>
             declare
                use Gain_Lose_By_Age_band_Web_IO;
             begin
                for tt in Adult_Age_Band loop
                  for use_totals in False .. True loop
                     title := TuS( "" ) & 
                         Lookup( "Age", lang ) & " " &
                         Lookup( Censor_String( Adult_Age_Band'Image( tt )), lang );
                     subtitle := TuS(" ");
                     if( use_totals )then
                        subtitle := subtitle & " " & Lookup( "totals", lang );
                     end if;
                     gal_str:= TuS( "         <li>" ) & LINE_BREAK;
                     gal_str:= gal_str& "              <a class='thumb' href='";
                     gal_str:= gal_str& 
                     Make_Chart_By_Col( 
                        tab           => outputs.gains_by_age_band, 
                        title         => TS( title ),
                        subtitle      => TS( subtitle ),
                        value_to_use  => disposable_income,
                        row_target    => tt,
                        chart_type    => General_Chart_Constants.bar,
                        chart_size    => General_Chart_Constants.large,
                        use_totals    => use_totals,
                        lang          => lang );
                     gal_str:= gal_str& "' title='" & title & "' >" & LINE_BREAK;  
                     gal_str:= gal_str& "<img src='" &  
                     Make_Chart_By_Col( 
                        tab           => outputs.gains_by_age_band, 
                        title         => TS( title ),
                        subtitle      => TS( subtitle ),
                        value_to_use  => disposable_income,
                        row_target    => tt,
                        chart_type    => General_Chart_Constants.bar,
                        chart_size    => General_Chart_Constants.thumb,
                        use_totals    => use_totals,
                        lang          => lang );
                     gal_str:= gal_str& "' alt='" & title & "' />" & LINE_BREAK;  
                     gal_str:= gal_str& "            </a>" & LINE_BREAK;
                     gal_str:= gal_str& "         </li>" & LINE_BREAK;
                     thumb_list := thumb_list & gal_str;
                  end loop;
               end loop;
            end;
         when no_breakdown => null;
      end case;
      Insert( translations, Assoc( "ROOT", euws.WSC_Web_Root ));
      Insert( translations, Assoc( "INTRO_TEXT", Lookup( "gain_lose_explanation", lang )));
      Insert( translations, Assoc( "THUMBNAILS", thumb_list ));
      return TuS( Web_Utils.Parse_Template( 
            Model.WSC.Global_Settings.template_components_path & 
            Model.WSC.Global_Settings.dir_separator & "output_gallery", 
            translations ));      
   end Get_Gain_Lose_Chart_Grid;
   
   function Get_Gain_Lose_Table( 
      outputs      : Outputs_Rec; 
      breakdown    : Disaggregated_Breakdown_Target   := Disaggregated_Breakdown_Target'First;
      comp_Cell    : Compare_Cell       := current_cell;
      cell_op      : Cell_Compare_Type  := counter;
      value_To_Use : Summary_Items_Type := disposable_income;
      lang         : Languages := Languages'First ) return Unbounded_String is
         
    begin
      case breakdown is
         when by_tenure =>
            declare
               tab : Gain_Lose_By_Tenure.Table_Expression;
            begin
               tab := Gain_Lose_By_Tenure.Express_Table( 
                  outputs.gains_by_tenure, 
                  comp_cell, 
                  cell_op, 
                  value_to_use );
               return Gain_Lose_By_Tenure_Web_IO.To_String(
                  outputs.gains_by_tenure,
                  tab,
                  Lookup( "Gainers and Losers by tenure type", lang ),
                  Lookup( "Gainers and Losers by tenure type", lang ),
                  lang,
                  True );
                  
            end;
          -- when by_decile =>
             -- declare
                -- tab : Gain_Lose_By_Decile.Table_Expression;
             -- begin
                -- tab := Gain_Lose_By_Decile.Express_Table( 
                   -- outputs.gains_by_decile, 
                   -- comp_cell, 
                   -- cell_op, 
                   -- value_to_use );
                -- return Gain_Lose_By_Decile_Web_IO.To_String(
                   -- outputs.gains_by_decile,
                   -- tab,
                   -- Lookup( "Gainers and Losers by decile", lang ),
                   -- Lookup( "Gainers and Losers by decile", lang ),
                   -- lang,
                   -- True );
             -- end;
         when by_age_of_head =>
            declare
               tab : Gain_Lose_By_Age_band.Table_Expression;
            begin
               tab := Gain_Lose_By_Age_band.Express_Table( 
                  outputs.gains_by_age_band, 
                  comp_cell, 
                  cell_op, 
                  value_to_use );
               return Gain_Lose_By_Age_band_Web_IO.To_String(
                  outputs.gains_by_age_band,
                  tab,
                  Lookup( "Gainers and Losers by age of head", lang ),
                  Lookup( "Gainers and Losers by age of head", lang ),
                  lang,
                  True );
            end;
       end case;
    end Get_Gain_Lose_Table;
 
end  Model.WSC.Output.Gain_Lose_Web_IO;
