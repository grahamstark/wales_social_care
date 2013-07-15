--
-- copyright(c) 2009 Graham Stark/ Virtual Worlds (graham.stark@virtual-worlds.biz)
--
-- ////////////////////////////////
--
-- This is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 3, or (at your option)
-- any later version.
--
-- It is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this software; see the file docs/gpl_v3.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street,
-- Boston, MA 02110-1301, USA.
--
-- /////////////////////////////
with IO_Commons;
with Text_Utils;
with Base_Model_Types;
with T_Utils.Standard_Chart_Generator;
with General_Chart_Constants;
with Model.WSC.Global_Settings;

package body Tabulator.Web_IO is

   use IO_Commons;
   use Text_Utils;
   use Base_Model_Types;
   use General_Chart_Constants;

   function Make_Chart_By_Col(
      tab          : Table_Type;
      title        : String;
      subtitle     : String;
      value_to_use : Cell_Values_Range;
      use_totals   : Boolean;
      row_target   : Row_Range;
      chart_type   : General_Chart_Constants.Chart_Type;
      chart_size   : General_Chart_Constants.Chart_Size;
      lang         : Languages ) return Unbounded_String is
      package Gl_T is new T_Utils(
         Rate_Type => Rate,
         Amount_Type => Amount,
         Counter_Type => Counter_Type,
         T => Col_Range );

      package Gl_Standard_Charts is new Gl_T.Standard_Chart_Generator;

      function Pretty_Print( v : Col_Range ) return String is
      begin
         return Prettify_Image( Censor_String( Col_Range'Image( v )) & "_gain_lose" );
      end  Pretty_Print;

   use General_Chart_Constants;
   use Text_Utils;
   use Gl_T;
      url    : Unbounded_String;
      data   : Gl_T.Amount_Array;
      expr   : Table_Expression;
   begin
      expr := Express_Table(
         tab,
         current_cell,
         counter,
         value_to_use );
      for c in Col_Range loop
         if( use_totals )then
            data( c ) := Amount( expr.col_totals( c ));
         else
            data( c ) := Amount( expr.cells( row_target, c ));
         end if;
      end loop;
      url := Gl_Standard_Charts.Make_Univariate_Chart(
         plotter_url  => Model.WSC.Global_Settings.Charts_URL,
         title        => title,
         subtitle     => subtitle,
         data1        => data,
         x_axis_label => Lookup( "Changes in net income", lang ),
         y_axis_label => Lookup( "Number of households", lang ),
         printer      => Pretty_Print'Access,
         ctype        => bar,
         size         => chart_size  );
      return url;
   end Make_Chart_By_Col;

   function Make_Chart_By_Row(
      tab          : Table_Type;
      title        : String;
      subtitle     : String;
      value_to_use : Cell_Values_Range;
      cell_op      : Cell_Compare_Type;
      x_axis_label : String;
      chart_type   : General_Chart_Constants.Chart_Type;
      chart_size   : General_Chart_Constants.Chart_Size;
      lang         : Languages ) return Unbounded_String is

      package Gl_T is new T_Utils( Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type, T => Row_Range );
      package Gl_Standard_Charts is new Gl_T.Standard_Chart_Generator;

      function Pretty_Print( v : Row_Range ) return String is
      begin
         return Prettify_Image( Censor_String( Row_Range'Image( v )));
      end  Pretty_Print;

    use General_Chart_Constants;
    use Text_Utils;
    use Gl_T;
      url    : Unbounded_String;
      data   : Gl_T.Amount_Array;
      expr   : Table_Expression;
   begin
      expr := Express_Table(
         tab,
         current_cell,
         cell_op,
         value_to_use );

      for r in Row_Range loop
         data( r ) := Amount( expr.row_totals( r ));
      end loop;
      url := Gl_Standard_Charts.Make_Univariate_Chart(
         plotter_url  => Model.WSC.Global_Settings.Charts_URL,
         title        => title,
         subtitle     => subtitle,
         data1        => data,
         printer      => Pretty_Print'Access,
         y_axis_label => Lookup( "Average Gain/Loss", lang ),
         x_axis_label => Lookup( x_axis_label, lang ),
         ctype        => bar,
         size         => chart_size  );
         
      return url;

   end  Make_Chart_By_Row;


   function To_String(
         complete_table : Table_Type;
         tab            : Table_Expression;
         name           : String;
         description    : String;
         lang           : Languages;
         cells_as_ints  : Boolean := True ) return Unbounded_String is
      
      t : Unbounded_String;
   begin
      t := t & "<span class='smallText'>" & description & "</span>" & LINE_BREAK;
      t := t & "<table class='datatable' cellpadding='8' >" & LINE_BREAK;
      t := t & "   <caption>" & Lookup( name, lang ) & "</caption>" & LINE_BREAK;
      t := t & "   <thead>" & LINE_BREAK;
      t := t & "      <tr class='headerRow'><th></th><th colspan='3' align='center'>" & Lookup( "losers", lang ) & "</th><th>" & Lookup( "no_change", lang ) & "</th> <th colspan='3' align='center'>" & Lookup( "gainers", lang ) & "</th><th></th></tr> " & LINE_BREAK;
      t := t & "      <tr class='headerRow'>" & LINE_BREAK;
      t := t & "          <th width='20%'></th><th width='10%' >&lt; -50</th><th width='10%' >-49.99 -10</th><th width='10%' >-9.99 - 1</th><th width='10%' >-</th><th width='10%' >1-9.99</th><th width='10%' >10 49 9</th><th width='10%' > &ge; 50</th><th width='10%' >" & Lookup( "totals",  lang ) & "</th> " & LINE_BREAK;
      t := t & "      </tr>" & LINE_BREAK;
      t := t & "   </thead>" &  LINE_BREAK;
      t := t & "   <tbody>" & LINE_BREAK;
      for row in Row_Range loop
         t := t & "   <tr>" & Line_BREAK;
         t := t & "      <th width='20%'>" & Prettify_Image( Censor_String( Row_Range'Image( row ))) & "</th>" & LINE_BREAK;
         for col in Col_Range loop
            
            t := t & "<td>";
            t := t &   Format_One_Cell( complete_table, row, col, tab.cells( row, col ), standard_cell, cells_as_ints, lang );
            t := t & "</td>";
         end loop;
         t := t & "<td class='totals'>" & 
            Format_One_Cell( 
               complete_table, 
               row, 
               Col_Range'First, 
               tab.row_totals( row ), 
               row_total, 
               cells_as_ints, 
               lang ) & "</td>";
         t := t & "   </tr>" & LINE_BREAK;
      end loop;
      t := t & "   <tr class='tableRowTotals'>" & LINE_BREAK;
      t := t & "      <th width='20%'>" & Lookup( "totals", lang ) & "</th>";
      for col in Col_Range loop
         t := t & "<td>" & Format_One_Cell( complete_table, Row_Range'First, col, tab.col_totals( col ), col_total, cells_as_ints, lang ) & "</td>";
      end loop;
      t := t & "<td>" & Format_One_Cell( complete_table, Row_Range'First, Col_Range'First, tab.total, overall_total, cells_as_ints, lang ) & "</td>";
      t := t & "      </tr>" & LINE_BREAK;

      t := t & "   </tbody>" & LINE_BREAK;
      t := t & "</table>" & LINE_BREAK;
      return t;
   end To_String;

-- FIXME ADD NET INCOMES

end Tabulator.Web_IO;
