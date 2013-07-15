with WSC_DB_Data;
with Disaggregated_Data_Table_IO;
with Disaggregated_Data_Table_Cell_IO;
with Text_Utils;
with Base_Types;
with WSC_DB_Data;
with Table_Stats_IO;
with GNATColl.Traces;

package body Model.WSC.Summary_Items_DAO is
   
   use Text_Utils;
   use Base_Types;
   
   TOTAL_INDICATOR : constant := -999;
   
   use WSC_DB_Data;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.SUMMARY_ITEMS_DAO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


  function Near_Zero_Round( m : Amount ) return Base_Types.Real is
   begin
      if( m > -0.000001 ) and ( m < 0.000001 )then
         return 0.0;
      end if;
      return Base_Types.Real( m );
   end Near_Zero_Round;
   
   procedure Save( r : Run; sysno : Positive; iteration : Positive; wave : Waves; t : Summary_Items_Array ) is
      dat_tab  : Disaggregated_Data_Table;
      cell : Disaggregated_Data_Table_Cell;
   begin
      dat_tab.model_table_name := SUMMARY_KEY;
      dat_tab.username := r.username;
      dat_tab.run_id := r.run_id;
      dat_tab.iteration := iteration;
      Disaggregated_Data_Table_IO.Save( dat_tab );
      cell.model_table_name := SUMMARY_KEY;
      cell.username := r.username;
      cell.run_id := r.run_id;
      cell.iteration := iteration;
      cell.system_number := sysno;
      cell.wave := TuS( Waves'Image( wave ));
      for vr in Summary_Items_Type loop
         cell.value1 := Near_Zero_Round( t( vr ));
         cell.row_num := Summary_Items_Type'Pos( vr );
         cell.col_num := TOTAL_INDICATOR;
         Disaggregated_Data_Table_Cell_IO.Save( cell );
      end loop;
   end Save;
   
   function Retrieve( r : Run; sysno : Positive; iteration : Positive; wave : Waves ) return Summary_Items_Array is
      use Disaggregated_Data_Table_Cell_List;
      t        : Summary_Items_Array;
      dat_tab  : Disaggregated_Data_Table := Disaggregated_Data_Table_IO.Retrieve_By_PK( r.run_id, r.username, SUMMARY_KEY, iteration );
      cells    : Disaggregated_Data_Table_Cell_List.Vector := Disaggregated_Data_Table_IO.Retrieve_Associated_Disaggregated_Data_Table_Cells( dat_tab, sysno, wave );
      n        : Natural := Natural( cells.Length );
   begin
      for i in 1 .. n loop
         declare
            cell : Disaggregated_Data_Table_Cell := cells.Element( i );
         begin
            t( Summary_Items_Type'Val( cell.row_num )) := Amount( cell.value1 );
         end;
      end loop;
      return t;
   end Retrieve;

   function Retrieve( 
      wsc_run      : Run; 
      measure      : Summary_Statistics_Type;
      summary_item : Summary_Items_Type;
      system       : Positive; 
      iteration    : Positive := 1 
      ) return Abs_Waves_Array is
   use Waves_Package;
      wa    : Waves_Array := ( others => 0.0);      
      wave  : Waves;
   begin
      for year in wsc_run.start_year .. wsc_run.end_year loop
         wave := Wave_From_Year( year );
         if( measure = no_statistic )then
            declare
               cell : Disaggregated_Data_Table_Cell;
            begin
               cell := Disaggregated_Data_Table_Cell_IO.Retrieve_By_PK( 
                  wsc_run.run_id, 
                  wsc_run.username, 
                  SUMMARY_KEY,
                  Summary_Items_Type'Pos( summary_item ),
                  -999,
                  TuS( Waves'Image( wave )),
                  iteration,
                  system );
               wa( wave ) := Amount( cell.value1 ); 
            end;
         else
            declare
               stats : WSC_DB_Data.Table_Stats;
            begin
               stats := Table_Stats_IO.Retrieve_By_PK( 
                  wsc_run.run_id, 
                  wsc_run.username, 
                  SUMMARY_KEY, 
                  Summary_Items_Type'Pos( summary_item ), 
                  -999, 
                  TuS( wave'Img ), 
                  system );
               case measure is
                  when no_statistic => null;
                  when mean => wa( wave ) := stats.Rmean_1;
                  when minimum => wa( wave ) := stats.Rmin_1; 
                  when maximum => wa( wave ) := stats.Rmax_1; 
                  when median => wa( wave ) := stats.Rmed_1; 
                  when standard_deviation => wa( wave ) := stats.Sddev_1;
                  when decile_1 => wa( wave ) := stats.Dec1_1; 
                  when decile_10 => wa( wave ) := stats.Dec10_1;
               end case;
               Log( "on wave " & wave'Img & " measure " & measure'Img & " data " & Format( wa( wave )));
            end;
         end if;
      end loop;
      return wa;
   end Retrieve;
   
   function Retrieve( 
      wsc_run   : Run; 
      measure   : Summary_Statistics_Type; 
      system    : Positive; 
      wave      : Waves;
      iteration : Positive := 1 
      ) return Summary_Items_Array is
      a  : Summary_Items_Array;
   begin
      if( measure = no_statistic )then
         a := Retrieve( wsc_run, system, iteration, wave );
      else
         declare
            stats : WSC_DB_Data.Table_Stats;
         begin
            for st in Summary_Items_Type loop
               stats := Table_Stats_IO.Retrieve_By_PK( 
                  wsc_run.run_id, 
                  wsc_run.username, 
                  SUMMARY_KEY, 
                  Summary_Items_Type'Pos( st ), 
                  -999, 
                  TuS( wave'Img ), 
                  system );
               case measure is
                  when no_statistic => null;
                  when mean => a( st ) := stats.Rmean_1;
                  when minimum => a( st ) := stats.Rmin_1; 
                  when maximum => a( st ) := stats.Rmax_1; 
                  when median => a( st ) := stats.Rmed_1; 
                  when standard_deviation => a( st ) := stats.Sddev_1;
                  when decile_1 => a( st ) := stats.Dec1_1; 
                  when decile_10 => a( st ) := stats.Dec10_1;
               end case;
            end loop;
         end;
      end if;
      return a;
   end Retrieve;
   
end Model.WSC.Summary_Items_DAO;
