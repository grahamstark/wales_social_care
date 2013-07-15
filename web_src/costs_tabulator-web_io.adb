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
with T_Utils.Standard_Chart_Generator;
with Base_Model_Types;
with Model.WSC.Global_Settings;

package body Costs_Tabulator.Web_IO is
   
   use base_Model_Types;
   use IO_Commons;
   use Text_Utils;
   use Translations;
   
   function Make_Chart(
      pre_tab      : Table_Type;
      post_tab     : Table_Type;
      title        : String;
      subtitle     : String;
      print_counts : Boolean;
      col          : Values_Range;
      x_axis_label : String;
      chart_type   : General_Chart_Constants.Chart_Type;
      size         : General_Chart_Constants.Chart_Size;
      lang         : Languages ) return Unbounded_String is

      package Costs_T is new T_Utils( Rate_Type => Rate, Amount_Type => Amount, Counter_Type => Counter_Type, T => Breakdown_Range );
      package Costs_Standard_Charts is new Costs_T.Standard_Chart_Generator;
      
      function Pretty_Print( v : Breakdown_Range ) return String is
      begin
         return Prettify_Image( Censor_String( Breakdown_Range'Image( v )));
      end  Pretty_Print;

         
    use General_Chart_Constants;
    use Text_Utils;
    use Costs_T;  
      url        : Unbounded_String;
      utitle     : Unbounded_String;
      pre_data   : Costs_T.Amount_Array;
      post_data  : Costs_T.Amount_Array;
    begin
       utitle := utitle & Lookup( title, lang );
       if( subtitle /= "" ) then
          utitle := utitle &  "|" & Lookup( subtitle, lang );
       end if;
       for b in Breakdown_Range loop
          if( print_counts )then
             pre_data( b ) := Amount( pre_tab.cells( b )( col ).count );
             post_data( b ) := Amount( post_tab.cells( b )( col ).count );
          else 
             pre_data( b ) := Amount( pre_tab.cells( b )( col ).amount );
             post_data( b ) := Amount( post_tab.cells( b )( col ).amount );
          end if;   
       end loop;
       url := Costs_Standard_Charts.Make_Univariate_Chart(
         plotter_url  => Model.WSC.Global_Settings.Charts_URL,
         title        => Lookup( title, lang ),
         subtitle     => Lookup( subtitle, lang ),
         data1        => pre_data,
         data2        => post_data,
         printer      => Pretty_Print'Access,
         x_axis_label => Lookup( x_axis_label, lang ),
         y_axis_label => Lookup( "Total Cost", lang ),
         ctype        => bar,
         system       => both,
         size         => size );
       return url;
   end Make_Chart;
   
   function To_String( 
         tab    : Table_Type;
         name   : String;
         description : String; 
         lang   : Languages;
         print_counts : Boolean;
         cells_as_ints : Boolean := False
        ) return Unbounded_String is
            
      function Fmt( a : Cell_Type ) return String is
      begin
         if( cells_as_ints )then
            if( print_counts )then
               return Web_Format( Integer( a.count ), lang );
            else
               return Web_Format( Integer( a.amount ), lang );
            end if;
         else
            if( print_counts )then
               return Web_Format( Amount( a.count ), lang );
            else
               return Web_Format( Amount( a.amount ), lang );
            end if;
         end if;
      end Fmt;
            
      t : Unbounded_String;
   begin
      t := t & "<span class='smallText'>" & description & "</span>" & LINE_BREAK;
      t := t & "<table class='datatable' cellpadding='8' >" & LINE_BREAK;
      t := t & "   <caption>" & Lookup( name, lang ) & "</caption>" & LINE_BREAK;
      t := t & "   <thead>" & LINE_BREAK;
      t := t & "      <tr class='headerRow'>" & LINE_BREAK;
      t := t & "          <th width='20%'></th>";
      for v in Values_Range loop 
          t := t & "<th>" & Prettify_Image( Censor_String( Values_Range'Image( v ))) & "</th>";
      end loop;
      t := t & "      </tr>" & LINE_BREAK;
      t := t & "   </thead>" &  LINE_BREAK;
      t := t & "   <tbody>" & LINE_BREAK;
      
      for b in Breakdown_Range loop
         t := t & "      <tr>" & LINE_BREAK;
         t := t & "         <th>" & Prettify_Image( Censor_String( Breakdown_Range'Image( b ))) & "</th>";
         for v in Values_Range loop 
             t := t & "<td>" & Fmt( tab.cells( b )( v )) & "</td>"; 
         end loop;
         t := t & "</tr>" & LINE_BREAK;
      end loop;
      t := t & "   <tr class='tableRowTotals'>" & LINE_BREAK;
      t := t & "      <th width='20%'>" & Lookup( "totals", lang ) & "</th>";
      for v in Values_Range loop 
         t := t & "<td class='totals'>" & Fmt( tab.totals( v )) & "</td>"; 
      end loop;
      t := t & "      </tr>" & LINE_BREAK;
      t := t & "   </tbody>" & LINE_BREAK;
      t := t & "</table>" & LINE_BREAK;
      return t;
   end To_String;

end Costs_Tabulator.Web_IO;
