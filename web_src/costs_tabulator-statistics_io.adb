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
with Ada.Text_IO;

with GNATColl.Traces;

with T_Utils.Standard_Chart_Generator;
with Model.WSC.Global_Settings;
with Model.WSC.Stats_Table_Commons;

with Base_Model_Types;
with Table_Stats_IO;
with Templates_Parser;
with Text_Utils;
with WSC_DB_Data;
with Web_Utils;


package body Costs_Tabulator.Statistics_IO is
   
   use Text_Utils;
   use WSC_DB_Data;
   use Model.UK_Format_Utils;
   use Base_Model_Types;
   
   package stats_commons renames Model.WSC.Stats_Table_Commons;
   package euws renames Model.WSC.Global_Settings; 
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "COSTS_TABULATOR.STATISTICS_IO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;
   
   function Retrieve_Cell( r : Run; wave : Waves; table_name : Unbounded_String; b : Breakdown_Range; v : Values_Range; sysno : Positive ) return WSC_DB_Data.Table_Stats is
      cell : WSC_DB_Data.Table_Stats;
      wave_str    : constant Unbounded_String := TuS( Waves'Image( wave ));                  
   begin
      cell := Table_Stats_IO.Retrieve_By_PK( 
                     r.run_id, 
                     r.username, 
                     table_name, 
                     Breakdown_Range'Pos( b ), 
                     Values_Range'Pos( v ), 
                     wave_str, 
                     sysno );
       if( r.num_iterations = 1 )then
         cell.rmin_1 := 0.0;
         cell.rmax_1 := 0.0;
         cell.rmed_1 := 0.0;
         cell.sddev_1 := 0.0;
         cell.dec1_1 := 0.0;
         cell.dec10_1 := 0.0;
         cell.rmin_2 := 0.0;
         cell.rmax_2 := 0.0;
         cell.rmed_2 := 0.0;
         cell.sddev_2 := 0.0;
         cell.dec1_2 := 0.0;
         cell.dec10_2 := 0.0;
         cell.rmin_3 := 0.0;
         cell.rmax_3 := 0.0;
         cell.rmed_3 := 0.0;
         cell.sddev_3 := 0.0;
         cell.dec1_3 := 0.0;
         cell.dec10_3 := 0.0;
         cell.rmin_4 := 0.0;
         cell.rmax_4 := 0.0;
         cell.rmed_4 := 0.0;
         cell.sddev_4 := 0.0;
         cell.dec1_4 := 0.0;
         cell.dec10_4 := 0.0;
         cell.rmin_5 := 0.0;
         cell.rmax_5 := 0.0;
         cell.rmed_5 := 0.0;
         cell.sddev_5 := 0.0;
         cell.dec1_5 := 0.0;
         cell.dec10_5 := 0.0;
         cell.rmin_6 := 0.0;
         cell.rmax_6 := 0.0;
         cell.rmed_6 := 0.0;
         cell.sddev_6 := 0.0;
         cell.dec1_6 := 0.0;
         cell.dec10_6 := 0.0;
       end if;
       return cell;
   end Retrieve_Cell;
   
   function Retrieve_Col_Total( r : Run; wave : Waves; table_name : Unbounded_String; v : Values_Range; sysno : Positive ) return WSC_DB_Data.Table_Stats is
      cell : WSC_DB_Data.Table_Stats;
      wave_str    : constant Unbounded_String := TuS( Waves'Image( wave ));                  
   begin
      cell := Table_Stats_IO.Retrieve_By_PK( 
                     r.run_id, 
                     r.username, 
                     table_name, 
                     -999, 
                     Values_Range'Pos( v ), 
                     wave_str, 
                     sysno );
       return cell;
   end Retrieve_Col_Total;
   
   function To_String( 
         wsc_run        : Run;      
         wave           : Waves;
         table_name     : Unbounded_String;
         sysno          : Positive;
         print_counts   : Boolean;
         name           : String;
         description    : String ) return Unbounded_String is
      which : constant Positive := ( if print_counts then 2 else 1 );
      as_real : constant Boolean := not print_counts;
      t     : Unbounded_String;
      divisor : constant Amount := ( if print_counts then 1.0 else 1_000_000.0/52.0  ); -- annual millions
   begin
      t := t & "<span class='smallText'>" & description & "</span>" & LINE_BREAK;
      t := t & "<table class='datatable' cellpadding='8' >" & LINE_BREAK;
      t := t & "   <caption>" & name & "</caption>" & LINE_BREAK;
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
         declare
            row_label : constant String := Prettify_Image( Censor_String( Breakdown_Range'Image( b )));
         begin
            t := t & "      <tr>" & LINE_BREAK;
            t := t & "         <th>" & Prettify_Image( Censor_String( Breakdown_Range'Image( b ))) & "</th>";
            for v in Values_Range loop 
               declare
                  col_label  : constant String := Prettify_Image( Censor_String( Values_Range'Image( v )));
                  id         : constant String := Censor_String( Breakdown_Range'Image( b ) & "-" & Values_Range'Image( v )); 
                  cell_title : constant String := name & " : " & row_label & " : " & col_label;
                  stats      : constant WSC_DB_Data.Table_Stats := Retrieve_Cell( wsc_run, wave, table_name, b, v, sysno );
               begin
                  t := t &  stats_commons.Stats_To_Table( 
                     stats => stats, 
                     title => cell_title,
                     id    => id,
                     class => "",
                     divisor => divisor,
                     as_real => as_real,
                     which   => which );
               end;
            end loop;
            t := t & "</tr>" & LINE_BREAK;
         end;
      end loop;
      t := t & "   <tr class='tableRowTotals'>" & LINE_BREAK;
      t := t & "      <th width='20%'>" & "Totals" & "</th>";
      for v in Values_Range loop 
         declare
            col_label : constant String := Prettify_Image( Censor_String( Values_Range'Image( v )));
            id : constant String := Censor_String( Values_Range'Image( v ) & "-" & "totals" ); 
            cell_title : constant String := name & " : " & col_label & " : " & " Totals";
            stats : constant WSC_DB_Data.Table_Stats := Retrieve_Col_Total( wsc_run, wave, table_name, v, sysno );
         begin
            t := t &  stats_commons.Stats_To_Table( 
                     stats => stats, 
                     title => cell_title,
                     id    => id,
                     divisor => divisor,
                     class => "totals",
                     as_real => as_real,
                     which   => which );
         end;
      end loop;
      t := t & "      </tr>" & LINE_BREAK;
      t := t & "   </tbody>" & LINE_BREAK;
      t := t & "</table>" & LINE_BREAK;
      return t;
   end To_String;

end Costs_Tabulator.Statistics_IO;
