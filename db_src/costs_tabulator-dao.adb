with WSC_DB_Data;
with Disaggregated_Data_Table_IO;
with Disaggregated_Data_Table_Cell_IO;
with Text_Utils;
with Base_Types;
with Maths_Functions;
with GNATColl.Traces;

package body Costs_Tabulator.DAO is
   
   use Text_Utils;
   use Base_Types;
   
   TOTAL_INDICATOR : constant := -999;
   
   package Maths_Funcs is new Maths_Functions( Real );
   
   use WSC_DB_Data;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "COSTS_TABULATOR.DAO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;
 
   function Near_Zero_Round( m : Data_Type ) return Real is
   begin
      if( m > -0.000001 ) and ( m < 0.000001 )then
         return 0.0;
      end if;
      return Real( m );
   end Near_Zero_Round;
   
   procedure Save( r : Run; sysno : Positive; iteration : Positive; wave : Waves; table_name : Unbounded_String; t : Table_Type ) is
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
      cell.system_number := sysno;
      cell.iteration := iteration;
      cell.wave := TuS( Waves'Image( wave ));
      for br in Breakdown_Range loop
         for vr in Values_Range loop
            cell.value1 := Near_Zero_Round( t.cells( br )( vr ).amount );
            cell.value2 := Near_Zero_Round( t.cells( br )( vr ).count );            
            cell.row_num := Breakdown_Range'Pos( br );
            cell.col_num := Values_Range'Pos( vr );
            Disaggregated_Data_Table_Cell_IO.Save( cell );
         end loop;
      end loop;
      cell.col_num := TOTAL_INDICATOR;            
      for vr in Values_Range loop
         cell.value1 := Real( t.totals( vr ).amount );
         cell.value2 := Real( t.totals( vr ).count );
         cell.row_num := -999;
         cell.col_num := Values_Range'Pos( vr );
         Disaggregated_Data_Table_Cell_IO.Save( cell );
      end loop;
   end Save;
   
   function Retrieve( r : Run; sysno : Positive; iteration : Positive; wave : Waves; table_name : Unbounded_String ) return Table_Type is
      use Disaggregated_Data_Table_Cell_List;
      t        : Table_Type;
      dat_tab  : Disaggregated_Data_Table := Disaggregated_Data_Table_IO.Retrieve_By_PK( r.run_id, r.username, table_name, iteration );
      cells    : Disaggregated_Data_Table_Cell_List.Vector := Disaggregated_Data_Table_IO.Retrieve_Associated_Disaggregated_Data_Table_Cells( dat_tab, sysno, wave );
      n        : Natural := Natural( cells.Length );
   begin
      for i in 1 .. n loop
         declare
            cell : Disaggregated_Data_Table_Cell := cells.Element( i );
         begin
            if cell.col_num = TOTAL_INDICATOR then
               t.totals( Values_Range'Val( cell.row_num )).amount := Data_Type( cell.value1 );
               t.totals( Values_Range'Val( cell.row_num )).count := Data_Type( cell.value2 );
            else
               t.cells( Breakdown_Range'Val( cell.col_num ))( Values_Range'Val( cell.row_num )).amount  := Data_Type( cell.value1 );
               t.cells( Breakdown_Range'Val( cell.col_num ))( Values_Range'Val( cell.row_num )).count  := Data_Type( cell.value2 );
            end if;
         end;
      end loop;
      return t;
   end Retrieve;

end Costs_Tabulator.DAO;
