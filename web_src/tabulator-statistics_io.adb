with Model.WSC.Global_Settings;
with Model;
with Model.WSC.Stats_Table_Commons;

with Base_Model_Types;
with Table_Stats_IO;
with Templates_Parser;
with Text_Utils;
with WSC_DB_Data;
with Web_Utils;

package body Tabulator.Statistics_IO is

   use Text_Utils;
   use WSC_DB_Data;
   use Model.UK_Format_Utils;
   use Base_Model_Types;

   package stats_commons renames Model.WSC.Stats_Table_Commons;
   package euws renames Model.WSC.Global_Settings; 

   function Retrieve_Cell( r : Run; wave : Waves; table_name : Unbounded_String; row : Row_Range; col : Col_Range ) return WSC_DB_Data.Table_Stats is
      cell : WSC_DB_Data.Table_Stats;
      wave_str    : constant Unbounded_String := TuS( Waves'Image( wave ));                  
   begin
      cell := Table_Stats_IO.Retrieve_By_PK( 
                     r.run_id, 
                     r.username, 
                     table_name, 
                     Row_Range'Pos( row ), 
                     Col_Range'Pos( col ), 
                     wave_str, 
                     1 );
       return cell;
   end Retrieve_Cell;

   function Retrieve_Row_Total( r : Run; wave : Waves; table_name : Unbounded_String; row : Row_Range ) return WSC_DB_Data.Table_Stats is
      cell : WSC_DB_Data.Table_Stats;
      wave_str    : constant Unbounded_String := TuS( Waves'Image( wave ));                  
   begin
      cell := Table_Stats_IO.Retrieve_By_PK( 
                     r.run_id, 
                     r.username, 
                     table_name, 
                     Row_Range'Pos( row ), 
                     -999, 
                     wave_str, 
                     1 );
       return cell;
   end Retrieve_Row_Total;

   function Retrieve_Col_Total( r : Run; wave : Waves; table_name : Unbounded_String; Col : Col_Range ) return WSC_DB_Data.Table_Stats is
      cell : WSC_DB_Data.Table_Stats;
      wave_str    : constant Unbounded_String := TuS( Waves'Image( wave ));                  
   begin
      cell := Table_Stats_IO.Retrieve_By_PK( 
                     r.run_id, 
                     r.username, 
                     table_name, 
                     -999, 
                     Col_Range'Pos( Col ), 
                     wave_str, 
                     1 );
       return cell;
   end Retrieve_Col_Total;
   
   function Retrieve_Total( r : Run; wave : Waves; table_name : Unbounded_String ) return WSC_DB_Data.Table_Stats is
      cell : WSC_DB_Data.Table_Stats;
      wave_str    : constant Unbounded_String := TuS( Waves'Image( wave ));                  
   begin
      cell := Table_Stats_IO.Retrieve_By_PK( 
                     r.run_id, 
                     r.username, 
                     table_name, 
                     -999,
                     -999, 
                     wave_str, 
                     1 );
       return cell;
   end Retrieve_Total;
   
   function To_String(
         wsc_run        : Run;      
         wave           : Waves;
         table_name     : Unbounded_String;
         name           : String;
         description    : String
          ) return Unbounded_String is
      
      t : Unbounded_String;
   begin
      t := t & "<p class='smallText'>" & description & "</p>" & LINE_BREAK;
      t := t & "<table class='datatable' cellpadding='8' width='100%'>" & LINE_BREAK;
      t := t & "   <caption>" & name & "</caption>" & LINE_BREAK;
      t := t & "   <thead>" & LINE_BREAK;
      t := t & "      <tr class='headerRow'><th></th><th colspan='3' align='center'>" & "Losers" & "</th><th>" & "No Change" & "</th> <th colspan='3' align='center'>" & "Gainers" & "</th><th></th></tr> " & LINE_BREAK;
      t := t & "      <tr class='headerRow'>" & LINE_BREAK;
      t := t & "          <th width='20%'></th><th width='10%' >&lt; -50</th><th width='10%' >-49.99 -10</th><th width='10%' >-9.99 - 1</th><th width='10%' >-</th><th width='10%' >1-9.99</th><th width='10%' >10 49 9</th><th width='10%' > &ge; 50</th><th width='10%' >" & "Totals" & "</th> " & LINE_BREAK;
      t := t & "      </tr>" & LINE_BREAK;
      t := t & "   </thead>" &  LINE_BREAK;
      t := t & "   <tbody>" & LINE_BREAK;
      rows:
      for row in Row_Range loop
         declare
            row_label : constant String := Prettify_Image( Censor_String( Row_Range'Image( row )));
         begin   
            t := t & "   <tr>" & Line_BREAK;
            t := t & "      <th width='20%'>" & row_label & "</th>" & LINE_BREAK;
            cols:
            for col in Col_Range loop
               declare
                  col_label  : constant String := Prettify_Image( Censor_String( Col_Range'Image( col )));
                  id         : constant String := Censor_String( Row_Range'Image( row ) & "-" & Col_Range'Image( col )); 
                  cell_title : constant String := name & " : " & row_label & " : " & col_label;
                  stats      : constant WSC_DB_Data.Table_Stats := Retrieve_Cell( wsc_run, wave, table_name, row, col );
               begin  
                  -- t := t & "<td>";
                  t := t &  stats_commons.Stats_To_Table( 
                     stats => stats, 
                     title => cell_title,
                     id    => id,
                     class => "",
                     divisor => 1.0,
                     as_real => True );
                  -- t := t & "</td>";
               end;
            end loop cols;
            
            declare
               id         : constant String := Censor_String( Row_Range'Image( row ) & "-" & "totals" ); 
               cell_title : constant String := name & " : " & row_label & " : " & " Totals";
               stats      : constant WSC_DB_Data.Table_Stats := Retrieve_Row_Total( wsc_run, wave, table_name, row );
            begin
               t := t & stats_commons.Stats_To_Table( 
                        stats => stats, 
                        title => cell_title,
                        id    => id,
                        divisor => 1.0,
                        class  => "totals",
                        as_real => True );
            end;
            t := t & LINE_BREAK & "</tr>" & LINE_BREAK;
         end; -- declare
      end loop rows;
      
      t := t & "   <tr class='tableRowTotals'>" & LINE_BREAK;
      t := t & "      <th width='20%'>" & "Totals" & "</th>";
      for col in Col_Range loop
         declare
            col_label : constant String := Prettify_Image( Censor_String( Col_Range'Image( col )));
            id : constant String := Censor_String( Col_Range'Image( Col ) & "-" & "totals" ); 
            cell_title : constant String := name & " : " & col_label & " : " & " Totals";
            stats : constant WSC_DB_Data.Table_Stats := Retrieve_Col_Total( wsc_run, wave, table_name, col );
         begin
            t := t &  stats_commons.Stats_To_Table( 
                     stats => stats, 
                     title => cell_title,
                     id    => id,
                     divisor => 1.0,
                     class => "totals",
                     as_real => True );
         end;
      end loop;
      declare
         id : constant String := "totals"; 
         cell_title : constant String := name & " :  Overal Total";
         stats : constant WSC_DB_Data.Table_Stats := Retrieve_Total( wsc_run, wave, table_name );
      begin
         t := t &  stats_commons.Stats_To_Table( 
                  stats => stats, 
                  title => cell_title,
                  id    => id,
                  class => "totals",
                  divisor => 1.0,
                  as_real => True );
      end;
      t := t & "      </tr>" & LINE_BREAK;

      t := t & "   </tbody>" & LINE_BREAK;
      t := t & "</table>" & LINE_BREAK;
      return t;
   end To_String;
   
end Tabulator.Statistics_IO;
