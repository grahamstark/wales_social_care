with Ada.Containers.Hashed_Sets;

with AWS.URL;

with Costs_Tabulator.Web_IO;
with Model.WSC.Global_Settings;
with General_Chart_Constants;
with Templates_Parser;
with Web_Utils;
with GNATColl.Traces;

package body Model.WSC.Output.Budget_Web_IO is
   
   use Translations;
   
   package Costs_By_Tenure_Web_IO is new 
      Household_Costs_By_Tenure.Web_IO;
   package Costs_By_Decile_Web_IO is new 
      Household_Costs_By_Decile.Web_IO;
   package Costs_By_Age_band_Web_IO is new 
      Household_Costs_By_Age_band.Web_IO;

   package euws renames Model.WSC.Global_Settings; 

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.OUTPUT.BUDGET_WEB_IO " );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   function Get_Summary(
      outputs       : Outputs_Rec; 
      lang          : Languages          := Languages'First ) return Unbounded_String is
   use Templates_Parser;
   use Household_Costs_By_Tenure;
   
      translations : Translate_Set;
      sum_lines    : Vector_Tag;
      row_str      : Unbounded_String;
      diff         : Amount;
      up_is_good   : Boolean;
      -- pre_totals   : Costs_Array := outputs. ; -- costs_by_tenure( 1 ).Extract_Costs_Row( Tenure_Type'First, True );
      -- outputs.summary_items_2  : Costs_Array := outputs.costs_by_tenure( 2 ).Extract_Costs_Row( Tenure_Type'First, True );
      itc          : constant Integer := Integer( outputs.summary_statistics( total_cost )/1000.0 );
   begin
     Insert( translations, Assoc( "ROOT", Model.WSC.Global_Settings.WSC_Web_Root ));
     Insert( translations, Assoc( "TITLE", Lookup( "Budget Breakdown", lang ))); 
     Insert( translations, Assoc( "MORE", Lookup( "more", lang ))); 
     for col in Summary_Items_Type loop
        diff := outputs.summary_items_2( col ) -  outputs.summary_items_1( col );
        up_is_good := False; -- ( col = personal_income_tax );
        row_str := TuS( "<tr>" ) & 
        
         "<th>" & Lookup( Censor_String( Summary_Items_Type'Image( col )), lang )  & "</th>" &
         "<td>" & Web_Format( outputs.summary_items_1( col ), lang ) & "</td>" &
         "<td>" & Web_Format( outputs.summary_items_2( col ), lang ) & "</td>" &
         "<td>" & GL_Format( diff, up_is_good, lang ) & "</td></tr>";
        sum_lines := sum_lines & row_str; 
     end loop;
     Insert( translations, Assoc( "BAD-GOOD-CLASS", GL_Class( diff, False )));
     Insert( translations, Assoc( "PRE", Lookup( "Before", lang )));
     Insert( translations, Assoc( "POST",  Lookup( "After", lang )));
     Insert( translations, Assoc( "DIFFERENCE", Lookup( "Change", lang )));
     Insert( translations, Assoc( "BUD-ROWS", sum_lines ));
     Insert( translations, Assoc( "TOTAL-COST-STR", Lookup( "Total cost of your measures", lang )));
     Insert( translations, Assoc( "UNITS", Lookup( "millions of &#163;s per year", lang )));
     Insert( translations, Assoc( "TOTAL-COST", GL_Format( itc, False, lang )));
     if( itc >= 1 )then
        Insert( translations, Assoc( "TOTAL-COST-ARROW", "up-bad" ));
     elsif(  itc <= -1 )then
        Insert( translations, Assoc( "TOTAL-COST-ARROW", "down-good" ));
     else
        Insert( translations, Assoc( "TOTAL-COST-ARROW", "nc" ));
     end if;
     return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "budget_summary", translations );      
   end Get_Summary;
   
   function Get_Gallery_Table_Header( lang : Languages ) return Unbounded_String is
      t : Unbounded_String;
   begin
      t := t & "            <table class='summary_table'>" & LINE_BREAK;
      t := t & "              <thead>" & LINE_BREAK;
      t := t & "                <tr>" & LINE_BREAK;
      t := t & "                  <td width='33%' align='center' valign='middle'>" & LINE_BREAK;
      t := t & "                  </td>" & LINE_BREAK;
      t := t & "                  <td width='33%' align='center'>" & LINE_BREAK;
      t := t & "                     " & Lookup( "Cost", lang ) & "<br />" & LINE_BREAK;
      t := t & "                     (&pound;)" & LINE_BREAK;
      t := t & "                  </td>" & LINE_BREAK;
      t := t & "                  <td width='33%' align='center'>" & LINE_BREAK;
      t := t & "                     " & Lookup( "Caseload", lang ) & "<br />" & LINE_BREAK;
      t := t & "                     (000s)" & LINE_BREAK;
      t := t & "                  </td>" & LINE_BREAK;
      t := t & "                </tr>" & LINE_BREAK;
      t := t & "              </thead>" & LINE_BREAK;
      
      return t;
   end Get_Gallery_Table_Header;
   
   function Create_Chart_Popup_Link(
      breakdown     : Breakdown_Target ;
      col           : Costs_Type;
      size          : Chart_Size;
      title         : String;
      print_counts  : Boolean;
      thumbnail_url : Unbounded_String ) return Unbounded_String is
      u : Unbounded_String;
   begin
      u := u & "<a class='picture_thumbnail' href='single_chart_popup?";
      u := u & "col=" & Censor_String( Costs_Type'Image( col ));
      u := u & "&amp;size=" & Censor_String( Chart_Size'Image( size ));
      u := u & "&amp;print_counts=" & Censor_String( Boolean'Image( print_counts ));
      u := u & "&amp;breakdown=" & Censor_String( Breakdown_Target'Image( breakdown ));
      u := u & "' ";
      u := u & "onclick=""ExampleWindow( this.href, '" & title & "', '" & title & "', '' ); return false;"" ";
      u := u & "onfocus='this.blur()'>";
      u := u & "<img src='" & thumbnail_url & "' alt='" & title & "' /></a>"; 
      return u;
   end Create_Chart_Popup_Link;
      
   function Create_Chart( 
      outputs      : Outputs_Rec; 
      breakdown    : Breakdown_Target;
      col          : Costs_Type;
      size         : Chart_Size;
      print_counts : Boolean;
      lang         : Languages ) return Unbounded_String is
      title      : Unbounded_String;
    begin
       title := TuS( "" ) & Lookup( Censor_String( Costs_Type'Image( col )), lang ) & " ";
       if( print_counts )then
          title := title & " (" & Lookup( "Caseloads", lang ) & ")";
       end if;
       case breakdown is
          when by_tenure =>
            return Costs_By_Tenure_Web_IO.Make_Chart( 
               pre_tab       => outputs.costs_by_tenure( 1 ), 
               post_tab      => outputs.costs_by_tenure( 2 ), 
               title         => TS( title ),             
               subtitle      => "",
               col           => col,
               print_counts  => print_counts,
               x_axis_label  => "Tenure Type",
               chart_type    => General_Chart_Constants.bar,
               size          => size,
               lang          => lang );
         when by_decile =>
            return Costs_By_Decile_Web_IO.Make_Chart( 
               pre_tab       => outputs.costs_by_decile( 1 ), 
               post_tab       => outputs.costs_by_decile( 2 ), 
               title         => TS( title ),
               subtitle      => "",
               col           => col,
               print_counts  => print_counts,
               x_axis_label  => "Income Decile",
               chart_type    => General_Chart_Constants.bar,
               size          => size,
               lang=>lang );
         when by_age_of_head =>
            return Costs_By_Age_band_Web_IO.Make_Chart( 
               pre_tab       => outputs.costs_by_age_band( 1 ), 
               post_tab      => outputs.costs_by_age_band( 2 ), 
               title         => TS( title ),
               subtitle      => "",
               col           => col,
               print_counts  => print_counts,
               x_axis_label  => "Age Band",
               chart_type    => General_Chart_Constants.bar,
               size          => size,
               lang=>lang );
         when no_breakdown => return TuS( "" );
      end case;
   end Create_Chart;
   
   function Get_Large_Budget_Chart_Page(
      outputs        : Outputs_Rec; 
      breakdown      : Disaggregated_Breakdown_Target   := Disaggregated_Breakdown_Target'First;
      col            : Costs_Type;
      print_counts   : Boolean            := False;
      lang           : Languages          := Languages'First ) return Unbounded_String is
   use Templates_Parser;
         chart : Unbounded_String := Create_Chart( 
            outputs, breakdown, col, large, print_counts, lang );
         translations : Translate_Set;
   begin
      Insert( translations, Assoc( "CHART", chart ));
      Insert( translations, Assoc( "TEMPLATE_ROOT", euws.template_components_path ));
      Insert( translations, Assoc( "LANG", Lang_Str( lang )));
      Insert( translations, Assoc( "ROOT", euws.WSC_Web_Root ));
      Insert( translations, Assoc( "SEP", euws.Dir_Separator ));
      return TuS( Web_Utils.Parse_Template( 
            Model.WSC.Global_Settings.template_components_path & 
            Model.WSC.Global_Settings.dir_separator & "chart_popup", 
            translations ));      
   end Get_Large_Budget_Chart_Page;
   
   function Get_Budget_Chart_Popup_Link(
      outputs       : Outputs_Rec; 
      breakdown     : Breakdown_Target;
      lang          : Languages := Languages'First ) return Unbounded_String is
   use Templates_Parser;
         translations : Translate_Set;
         title      : Unbounded_String;
         subtitle   : Unbounded_String;
         thumbnail  : Unbounded_String;
         gal_str    : Unbounded_String;
         print_counts : constant Boolean := FALSE;
   begin
      thumbnail := Create_Chart( 
            outputs, 
            breakdown, 
            social_care_residential,
            thumb,
            print_counts,
            lang );
      Insert( translations, Assoc( "TAX", thumbnail ));
      thumbnail := Create_Chart( 
            outputs, 
            breakdown, 
            social_care_non_residential,
            thumb,
            print_counts,
            lang );
      Insert( translations, Assoc( "SOCIAL", thumbnail ));
      thumbnail := Create_Chart( 
            outputs, 
            breakdown, 
            state_pension,
            thumb,
            print_counts,
            lang );
      Insert( translations, Assoc( "CHILD", thumbnail ));
      Insert( translations, Assoc( "TITLE", Lookup( Censor_String( Breakdown_Target'Image( breakdown )), lang )));
      Insert( translations, Assoc( "INITIAL", Censor_String( Breakdown_Target'Image( breakdown ))));
      return TuS( Web_Utils.Parse_Template( 
            Model.WSC.Global_Settings.template_components_path & 
            Model.WSC.Global_Settings.dir_separator & "budget_gallery_link", 
            translations ));      
         
   end Get_Budget_Chart_Popup_Link;
   
   function Get_Budget_Chart_Grid(
      outputs       : Outputs_Rec; 
      lang          : Languages := Languages'First ) return Unbounded_String is
   use Templates_Parser;
      translations : Translate_Set;
      table      : Unbounded_String;
      title      : Unbounded_String;
      thumbnail  : Unbounded_String;
      table_list : Vector_Tag;
      link_list  : Vector_Tag;
      title_list : Vector_Tag;
   begin
      breakdowns:
      for breakdown in by_decile .. Breakdown_Target'Last loop
         link_list := link_list & Censor_String( Breakdown_Target'Image( breakdown ));
         title_list := title_list & Lookup( Censor_String( Breakdown_Target'Image( breakdown )), lang );
         table := Get_Gallery_Table_Header( lang );
         table := table & "              <tbody>" & LINE_BREAK;
         cols:
         for col in Costs_Type loop
            title := TuS( "" ) & Lookup( Censor_String( Costs_Type'Image( col )), lang ) & " ";
            table := table & "                 <tr class='tableRowEven'><td valign='middle'>" & title & "</td>" & LINE_BREAK;
            counts:
            for print_counts in False .. True loop
               thumbnail := Create_Chart( 
                  outputs, 
                  breakdown, 
                  col,
                  thumb,
                  print_counts,
                  lang );
               table := table & "                    <td>";
               table := table & Create_Chart_Popup_Link(
                  breakdown, 
                  col,
                  large,
                  TS( title ),
                  print_counts,
                  thumbnail );
               table := table & "</td>" & LINE_BREAK;
            end loop counts;
            table := table & "                 </tr>" & LINE_BREAK;
         end loop cols;
         table := table & "              </tbody>" & LINE_BREAK;
         table := table & "          </table>" & LINE_BREAK;
         table_list := table_list & table;
      end loop breakdowns;
      Insert( translations, Assoc( "PAGE-TITLE", Lookup( "Chart Gallery", lang )));
      Insert( translations, Assoc( "INTRO_TEXT", Lookup( "household_budget_explanation", lang )));
      Insert( translations, Assoc( "TABLES", table_list ));
      Insert( translations, Assoc( "LINKS", link_list ));
      Insert( translations, Assoc( "TITLES", title_list ));
      Insert( translations, Assoc( "TEMPLATE_ROOT", euws.template_components_path ));
      Insert( translations, Assoc( "LANG", Lang_Str( lang )));
      Insert( translations, Assoc( "ROOT", euws.WSC_Web_Root ));
      Insert( translations, Assoc( "SEP", euws.Dir_Separator ));
      Insert( translations, Assoc( "BEFORE", Lookup( "Before Reform", lang  )));
      Insert( translations, Assoc( "AFTER", Lookup( "After Reform", lang  )));
      
      Insert( translations, Assoc( "IS-EXAMPLE-PAGE", True ));
      return TuS( Web_Utils.Parse_Template( 
            Model.WSC.Global_Settings.template_components_path & 
            Model.WSC.Global_Settings.dir_separator & "output_gallery_popup", 
            translations ));      
   end Get_Budget_Chart_Grid;

   
   function Get_Budget_Table( 
      outputs        : Outputs_Rec; 
      sysno          : System_Number      := 1;
      breakdown      : Disaggregated_Breakdown_Target   := Disaggregated_Breakdown_Target'First;
      print_counts   : Boolean            := False;
      do_differences : Boolean            := False;
      lang           : Languages          := Languages'First ) return Unbounded_String is
    begin
      case breakdown is
         when by_tenure =>
            declare
               tab : Household_Costs_By_Tenure.Table_Type;
            begin
               if( do_differences )then
                  tab := Household_Costs_By_Tenure.Difference( outputs.costs_by_tenure( 1 ), outputs.costs_by_tenure( 2 ));
               else
                  tab := outputs.costs_by_tenure( sysno );
               end if;
               return Costs_By_Tenure_Web_IO.To_String( 
                  tab,
                  Lookup( "By Tenure Type", lang ),
                  Lookup( "National budget broken down by tenure type (&pound; 000s)",lang ),
                  lang,
                  print_counts,
                  True );
            end;
         -- when by_decile =>
            -- declare
               -- tab : Household_Costs_By_Decile.Table_Type;
            -- begin
               -- if( do_differences )then
                  -- tab := Household_Costs_By_Decile.Difference( outputs.costs_by_decile( 1 ), outputs.costs_by_decile( 2 ));
               -- else
                  -- tab := outputs.costs_by_decile( sysno );
               -- end if;
               -- return Costs_By_Decile_Web_IO.To_String( 
                  -- tab,
                  -- Lookup( "By Decile", lang ),
                  -- Lookup( "National budget broken down by decile (&pound; 000s)",lang ),
                  -- lang,
                  -- print_counts,
                  -- True );
            -- end;
         when by_age_of_head =>
            declare
               tab : Household_Costs_By_Age_band.Table_Type;
            begin
               if( do_differences )then
                  tab := Household_Costs_By_Age_band.Difference( outputs.costs_by_age_band( 1 ), outputs.costs_by_age_band( 2 ));
               else
                  tab := outputs.costs_by_age_band( sysno );
               end if;
               return Costs_By_Age_band_Web_IO.To_String( 
                  tab,
                  Lookup( "By age of the head of the household", lang ),
                  Lookup( "National Budget broken down by  age of the head of the household(&pound; 000s)",lang ),
                  lang,
                  print_counts,
                  True );
            end;
        end case;
    end Get_Budget_Table;   
    

end  Model.WSC.Output.Budget_Web_IO;
