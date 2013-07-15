with GNATColl.Traces;

with WSC_DB_Data;
with Disaggregated_Data_Table_IO;
with Disaggregated_Data_Table_Cell_IO;
with Text_Utils;
with Base_Types;
with Table_Stats_IO;

package body Tabulator.DAO is
   
   use Text_Utils;
   use Base_Types;
   
   use WSC_DB_Data;
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "TABULATOR.DAO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   procedure Save( r : Run; iteration : Positive; wave : Waves; table_name : Unbounded_String; t : Table_Expression ) is
      dat_tab  : Disaggregated_Data_Table;
      cell : Disaggregated_Data_Table_Cell;
   begin
      dat_tab.model_table_name := table_name;
      dat_tab.username := r.username;
      dat_tab.run_id := r.run_id;
      dat_tab.iteration := iteration;
      Disaggregated_Data_Table_IO.Save( dat_tab );
      cell.model_table_name := table_name;
      cell.username := r.username;
      cell.run_id := r.run_id;
      cell.iteration := iteration;
      cell.system_number := 1;
      cell.wave := TuS( Waves'Image( wave ));
      for rr in Row_Range loop
         for cc in Col_Range loop
            cell.value1 := Real( t.cells( rr, cc ));
            cell.row_num := Row_Range'Pos( rr );
            cell.col_num := Col_Range'Pos( cc );
            Disaggregated_Data_Table_Cell_IO.Save( cell );
         end loop;
      end loop;
      cell.col_num := TOTAL_INDICATOR;            
      for rr in Row_Range loop
         cell.value1 := Real( t.row_totals( rr ));
         cell.row_num := Row_Range'Pos( rr );
         Disaggregated_Data_Table_Cell_IO.Save( cell );
      end loop;
      cell.row_num := TOTAL_INDICATOR;            
      for cc in Col_Range loop
         cell.value1 := Real( t.col_totals( cc ));
         cell.col_num := Col_Range'Pos( cc );
         Disaggregated_Data_Table_Cell_IO.Save( cell );
      end loop;
      cell.row_num := TOTAL_INDICATOR;            
      cell.col_num := TOTAL_INDICATOR;            
      cell.value1 := Real( t.total );
      Disaggregated_Data_Table_Cell_IO.Save( cell );
   end Save;
   
   function Retrieve( r : Run; iteration : Positive; wave : Waves; table_name : Unbounded_String ) return Table_Expression is
      use Disaggregated_Data_Table_Cell_List;
      t        : Table_Expression;
      dat_tab  : Disaggregated_Data_Table := Disaggregated_Data_Table_IO.Retrieve_By_PK( r.run_id, r.username, table_name, iteration );
      cells    : Disaggregated_Data_Table_Cell_List.Vector := Disaggregated_Data_Table_IO.Retrieve_Associated_Disaggregated_Data_Table_Cells( dat_tab, 1, wave );
      n        : Natural := Natural( cells.Length );
   begin
      for i in 1 .. n loop
         declare
            cell : Disaggregated_Data_Table_Cell := cells.Element( i );
         begin
            if( cell.row_num = TOTAL_INDICATOR ) and ( cell.col_num = TOTAL_INDICATOR )then
               t.total := Data_Type( cell.value1 );
            elsif cell.row_num = TOTAL_INDICATOR then
               t.col_totals( Col_Range'Val( cell.col_num )) := Data_Type( cell.value1 );
            elsif cell.col_num = TOTAL_INDICATOR then
               t.row_totals( Row_Range'Val( cell.row_num )) := Data_Type( cell.value1 );
            else
               t.cells( Row_Range'Val( cell.row_num ), Col_Range'Val( cell.col_num ) ) := Data_Type( cell.value1 );
            end if;
         end;
      end loop;
      return t;
   end Retrieve;
   
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
                     TOTAL_INDICATOR, 
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
                     Col_Range'Pos( Col ), 
                     TOTAL_INDICATOR, 
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
                     TOTAL_INDICATOR,
                     TOTAL_INDICATOR, 
                     wave_str, 
                     1 );
       return cell;
   end Retrieve_Total;
   


end Tabulator.DAO;
