with Ada.Assertions;
with Ada.Containers;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Unbounded; 
with Ada.Text_IO;
with Gnat.OS_Lib;
with System;

with Base_Model_Types;
with Base_Types;
with Maxima_And_Totals_IO;
with Model.WSC.Run_Declarations;
with Personal_Income_IO;
with Personal_Results_IO;
with Text_Utils;
with GNATColl.Traces;
with Wsc_Db_Data;
with DB_Commons;

package body Model.WSC.Results.DAO is
   
   use Model.WSC.Run_Declarations;
   use Text_Utils;
   package d renames db_commons;
   package uio renames Ada.Strings.Unbounded.Text_IO;
   package tio renames Ada.Text_IO;
   package rio renames Base_Model_Types.Real_IO;
   use Base_Types;
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Wsc_Db_Data;
   use Ada.Assertions;
     
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.RESULTS.DAO" );

   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   function Highest_Values_Of( 
      wsc_run : Run;
      -- start_wave : Waves;
      end_wave   : Waves; 
      sysno      : System_Number; 
      iteration  : Iteration_Number;
      pno        : Sernum_Value ) return Calculated_Incomes_Array  is
   use Ada.Containers;
      query : Unbounded_String;
      incomes : Wsc_Db_Data.Personal_Income_List.Vector;
      v : Calculated_Incomes_Array := ( others => 0.0 ); 
      n : Natural;
   begin
      -- query := query & " wave >= '" & start_wave'Img & "' and ";
      query := query & " year_from_wave( wave )<= year_from_wave( '" & end_wave'Img & "' )   and ";
      query := query & " username='" & wsc_run.username & "' and ";
      query := query & " run_id=" & wsc_run.Run_Id'Img & " and ";
      query := query & " pid='" & To_Unbounded_String(  pno ) & "' and ";
      query := query & " sysno=" & sysno'Img & " and ";
      query := query & " iteration=" & iteration'Img;
      incomes := Personal_Income_IO.Highest_Values_Of( TS( query ));
      n := Natural( incomes.Length );
      -- Assert( n <= 1, "should always return 0 or 1 records; was " & n'Img );
      Log( "Highest_Value_Of :: got query as " & TS( query ) & "| records found: " & n'Img );
      for i in 1 .. n loop
        declare
           inc  : constant Personal_Income := incomes.Element( i );
        begin
           v( inc.income_type ) := inc.value;
        end;
      end loop;
      Log( "returning from highest values " );
      return v;
   end Highest_Values_Of;
   
   function Value_Of( 
      wsc_run : Run; 
      wave      : Waves; 
      sysno     : System_Number;
      iteration : Iteration_Number;
      pno       : Sernum_Value; 
      which     : Calculated_Incomes;
      default   : Amount := 0.0 ) return Amount is
      wave_str              : Unbounded_String := TuS( wave'Img );
      personal_income_item  : Wsc_Db_Data.Personal_Income;
   begin
      personal_income_item := Personal_Income_IO.Retrieve_By_PK(
                  wsc_run.username,
                  wsc_run.run_id,
                  pno,
                  sysno,
                  iteration,
                  wave_str,
                  which );                  
      return personal_income_item.value;
   end Value_Of;

   procedure Delete_For_Run( wsc_run : Run ) is
      c : d.Criteria;
   begin
      Personal_Income_IO.Add_Username( c, wsc_run.username );
      Personal_Income_IO.Add_Run_Id( c, wsc_run.Run_Id );
      Personal_Income_IO.Delete( c );
      Personal_Results_IO.Delete( c );
   end Delete_For_Run;

   
   procedure Set( 
      wsc_run      : Run; 
      hh           : Model.WSC.Household.Household; 
      sysno        : System_Number; 
      iteration    : Iteration_Number;
      res          : Household_Result;
      min_age      : Age_Range := Age_Range'First;
      incomes_only : Boolean := False ) is
      personal_results_item : Wsc_Db_Data.Personal_Results;
      personal_income_item : Wsc_Db_Data.Personal_Income;
      wave_str : Unbounded_String := TuS( hh.hdata.wave'Img );
   begin
      Log( "Model.WSC.Results.DAO.Set entered" );
      personal_results_item.username := wsc_run.username;
      personal_results_item.run_id := wsc_run.Run_Id;
      personal_results_item.wave := wave_str;
      personal_results_item.hid := hh.hid;
      personal_results_item.sysno := sysno;
      personal_results_item.iteration := iteration;
      
      personal_income_item.username := wsc_run.username;
      personal_income_item.run_id := wsc_run.Run_Id;
      personal_income_item.wave := wave_str;
      personal_income_item.sysno := sysno;        
      personal_income_item.iteration := iteration;        
      personal_income_item.hid := hh.hid;

      for buno in 1 .. res.res.num_benefit_units loop
         personal_results_item.buno := buno;
         for adno in 1 .. res.benefit_units( buno ).res.num_people loop
            declare
               pr  : Personal_Result renames res.benefit_units( buno ).people( adno );
            begin
               if( hh.benefit_units( buno ).adults( adno ).age >= min_age )then
                  personal_results_item.adno := adno;
                  Log( "Set :: adding for adno |" & adno'Img & " pid |" & res.benefit_units( buno ).people( adno ).sernum'Img & "| wave |" & hh.hdata.wave'Img & "|" );
                  if( hh.benefit_units( buno ).adults( adno ).pid < 0 )then
                     Log( "negative pid for hh : " & Model.WSC.Household.To_String( hh ));
                  end if;
                  personal_results_item.pid := hh.benefit_units( buno ).adults( adno ).pid;
                  if( not incomes_only )then
                     personal_results_item.la_contributions         := pr.la_contributions;
                     personal_results_item.client_contributions     := pr.client_contributions;
                     personal_results_item.gross_care_costs         := pr.gross_care_costs;
                     personal_results_item.total_payments_to_date   := pr.total_payments_to_date;
                     personal_results_item.remaining_capital_stock  := pr.remaining_capital_stock;
                     personal_results_item.disposable_income        := pr.disposable_income;
                     personal_results_item.net_income               := pr.net_income;
                     personal_results_item.marginal_rate            := pr.marginal_rate;
                     personal_results_item.capital_contribution     := pr.capital_contribution;
                     personal_results_item.minimum_income_guarantee := pr.minimum_income_guarantee;
                     personal_results_item.passes_residential_means_test  := pr.passes_residential_means_test;
                     personal_results_item.passes_non_residential_means_test := pr.passes_non_residential_means_test;
                     personal_results_item.hours_of_care_la        := pr.hours_of_care_la;
                     personal_results_item.hours_of_care_private   := pr.hours_of_care_private;
                     personal_results_item.uap                     := pr.uap;
                     
                     Log( "saving #161" );
                     Personal_Results_IO.Save( personal_results_item, True );
                     Log( "saved #161" );
                  end if;
                  for i in Calculated_Incomes loop
                     if( pr.income( i ) /= 0.0  )then
                        personal_income_item.pid := personal_results_item.pid;
                        personal_income_item.buno := buno;
                        personal_income_item.adno := adno;                     
                        personal_income_item.income_type := i; 
                        personal_income_item.value := pr.income( i );
                        Log( "saving #172" );
                        Personal_Income_io.Save( personal_income_item, True );
                        Log( "saved #172" );
                     end if;
                  end loop;
               end if;
            end;
         end loop;
      end loop;
   end Set;

   function Get( 
      wsc_run  : Run; 
      wave     : Waves; 
      sysno    : System_Number;
      iteration  : Iteration_Number;
      pid      : Sernum_Value ) return Personal_Result is
      use Ada.Containers;
      personal_results_item : Wsc_Db_Data.Personal_Results;
      personal_income_list  : Wsc_Db_Data.Personal_Income_List.Vector;
      pr                    : Personal_Result;
      wave_str              : Unbounded_String := TuS( wave'Img );
      last_wave_str         : Unbounded_String := TuS(  Waves'Pred( wave )'Img );
      crit                  : d.Criteria;
      m_crit                : d.Criteria;
      n                     : Natural;
      max_items_list        : Wsc_Db_Data.Maxima_And_Totals_List.Vector;
      max_items_elem        : Wsc_Db_Data.Maxima_And_Totals;
   begin
      Log( "Get: for username = |" & TS( wsc_run.username ) & "| run_id |" & wsc_run.run_id'Img & "| pid |" & pid'Img & "| wave |" & TS( wave_str ) & "| " );               
      personal_results_item := Personal_Results_IO.Retrieve_By_PK(
                  wsc_run.username,
                  wsc_run.run_id,
                  sysno,
                  iteration,
                  pid,
                  wave_str ); 
      if personal_results_item /= Null_Personal_Results then
         pr.sernum  := personal_results_item.pid;
         Assert( pr.sernum = pid, " sernum mismatch in Get wanted =" & pid'Img & " retrieved = " &  pr.sernum'Img );
         pr.la_contributions  :=  personal_results_item.la_contributions ;
         pr.client_contributions  :=  personal_results_item.client_contributions; 
         pr.gross_care_costs  :=  personal_results_item.gross_care_costs; 
         pr.total_payments_to_date  :=  personal_results_item.total_payments_to_date; 
         pr.remaining_capital_stock  :=  personal_results_item.remaining_capital_stock; 
         pr.disposable_income  :=  personal_results_item.disposable_income; 
         pr.net_income  :=  personal_results_item.net_income; 
         pr.marginal_rate  :=  personal_results_item.marginal_rate; 
         pr.capital_contribution  :=  personal_results_item.capital_contribution; 
         pr.minimum_income_guarantee  :=  personal_results_item.minimum_income_guarantee; 
         
         pr.passes_residential_means_test := personal_results_item.passes_residential_means_test;
         pr.passes_non_residential_means_test := personal_results_item.passes_non_residential_means_test;
         pr.passes_residential_capital_test := personal_results_item.passes_residential_capital_test;
         pr.passes_non_residential_capital_test := personal_results_item.passes_non_residential_capital_test;
         pr.passes_residential_income_test := personal_results_item.passes_residential_income_test;
         pr.passes_non_residential_income_test := personal_results_item.passes_non_residential_income_test;
         
         pr.hours_of_care_la  :=  personal_results_item.hours_of_care_la; 
         pr.hours_of_care_private  :=  personal_results_item.hours_of_care_private; 
         pr.uap := personal_results_item.uap;
         Personal_Income_IO.Add_Username( crit, wsc_run.username );
         Personal_Income_IO.Add_Run_Id( crit, wsc_run.run_id );
         Personal_Income_IO.Add_Pid( crit, pid );
         Personal_Income_IO.Add_Wave( crit, wave_str );
         Personal_Income_IO.Add_Sysno( crit, sysno );
         Personal_Income_IO.Add_Iteration( crit, iteration );
         personal_income_list := Personal_Income_IO.Retrieve( crit );
         n := Natural( personal_income_list.Length );
         for i in 1 .. n loop
            declare
               inc : Wsc_Db_Data.Personal_Income := personal_income_list.Element( i );
            begin
               pr.income( inc.income_type ) :=  inc.value; 
            end;
         end loop;
         Maxima_And_Totals_IO.Add_Username( m_crit, wsc_run.username );
         Maxima_And_Totals_IO.Add_Run_Id( m_crit, wsc_run.run_id );
         Maxima_And_Totals_IO.Add_Pid( m_crit, pid );
         Maxima_And_Totals_IO.Add_Sysno( m_crit, sysno );
         Maxima_And_Totals_IO.Add_Iteration( m_crit, iteration );
         
         Maxima_And_Totals_IO.Add_Wave( m_crit, wave_str, d.le ); -- in some circumstances, might be last-wave
         
         max_items_list := Maxima_And_Totals_IO.Retrieve( m_crit );
         Assert( max_items_list.Length = 1, "  should always return 1 max items list element ; was " & max_items_list.Length'Img );
         max_items_elem := max_items_list.Element( 1 );
         Log( "got max_items_elem as " & Wsc_Db_Data.To_String( max_items_elem )); 
         pr.lifetime_gross_payments :=  max_items_elem.lifetime_gross_payments;
         pr.lifetime_client_contributions :=  max_items_elem.lifetime_client_contributions; 
         pr.lifetime_la_contributions :=  max_items_elem.lifetime_la_contributions; 
         pr.lifetime_capital_contributions :=  max_items_elem.lifetime_capital_contributions; 
         pr.highest_la_contribution :=  max_items_elem.highest_la_contribution; 
      else
         pr.sernum := -9;
         Log( "Get: got null results for wave " & wave'Img & " pid " & pid'Img );
      end if;
      return pr;
   end Get;
   
   function Get( 
      wsc_run : Run; 
      hh        : Model.WSC.Household.Household;
      sysno     : System_Number;
      iteration : Iteration_Number ) return Household_Result is
      hres : Household_Result;
   begin
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 .. hh.benefit_units( buno ).num_adults loop
            declare
               use Ada.Containers;
               bu                    : Benefit_Unit renames hh.benefit_units( buno );
               ad                    : Person renames bu.adults( adno );
            begin
               hres.benefit_units( buno ).people( adno ) := Get( wsc_run, hh.hdata.wave, sysno, iteration, ad.pid );
            end;
         end loop;
      end loop;
      Accumulate( hh, hres, wsc_run );
      Log( "After Get: returning with " & To_String( hres ));
      return hres;
   end Get;
   
   procedure Write_Quoted( 
      f        : Ada.Text_IO.File_Type;
      s        : Unbounded_String ) is
   begin
      tio.Put( f, "'" );
      uio.Put( f, s );
      tio.Put( f, "'" );      
   end Write_Quoted;

   procedure Write_Quoted( 
      f        : Ada.Text_IO.File_Type;
      s        : String ) is
   begin
      tio.Put( f, "'" );
      tio.Put( f, s );
      tio.Put( f, "'" );      
   end Write_Quoted;
   
   procedure Dump_As_SQL(
      res_file   : Ada.Text_IO.File_Type;
      inc_file  : Ada.Text_IO.File_Type;
      sysno     : System_Number;
      iteration : Iteration_Number;
      wsc_run : Run;
      is_first  : Boolean;
      is_last   : Boolean;
      hh        : Model.WSC.Household.Household; 
      res       : Household_Result ) is
      l_is_first, l_is_last : Boolean;
   begin
      for buno in 1 .. res.res.num_benefit_units loop
         for adno in 1 .. res.benefit_units( buno ).res.num_people loop
            declare
               pr  : Personal_Result renames res.benefit_units( buno ).people( adno );
            begin
               l_is_first := buno = 1 and adno = 1 and is_first and sysno = System_Number'First;
               l_is_last := buno = res.res.num_benefit_units and 
                            adno = res.benefit_units( buno ).res.num_people and
                            is_last and
                            sysno = System_Number'Last;
               Dump_As_SQL(
                  res_file,
                  inc_file,
                  hh.wave,
                  sysno,
                  iteration,
                  hh.hid,
                  buno,
                  adno,
                  wsc_run,
                  l_is_first,
                  l_is_last,
                  pr );
            end;
         end loop;
      end loop;
   end Dump_As_SQL;
    
   --
   -- FIXME DUP of the one in personal_results_io
   --
   INSERT_PART : constant String := "insert into personal_results (" &
         "username, run_id, sysno, iteration, pid, wave, hid, buno, adno, " &
         "passes_non_residential_capital_test," &
         "passes_non_residential_income_test, " &
         "passes_residential_capital_test, "&
         "passes_residential_income_test, " &
         "passes_residential_means_test, "&
         "passes_non_residential_means_test, " &
         "la_contributions, " & 
         "client_contributions, "&
         "gross_care_costs, "&
         "total_payments_to_date, "&
         "disposable_income, "&
         "net_income, " &
         "marginal_rate," &
         "capital_contribution, " &
         "minimum_income_guarantee, " & 
         "hours_of_care_la, " &
         "hours_of_care_private, " &
         "uap, " &
         "remaining_capital_stock " &
         ") values " ;

   procedure Dump_As_SQL(
      res_file  : Ada.Text_IO.File_Type;
      inc_file  : Ada.Text_IO.File_Type;
      wave      : Waves;
      sysno     : System_Number;
      iteration : Iteration_Number;
      hid       : Sernum_Value;
      buno      : Benefit_Unit_Number;
      adno      : Person_Number;
      wsc_run : Run;
      is_first  : Boolean;
      is_last   : Boolean;
      res       : Personal_Result ) is
   use Ada.Text_IO;
      last_income : Calculated_Incomes := Calculated_Incomes'First;
   begin
      -- if( is_first )then
      Put_Line( res_file, INSERT_PART & "( " );
      -- end if;
      Write_Quoted( res_file, wsc_run.username );
      Put( res_file, "," );
      Put( res_file, wsc_run.Run_Id'Img );
      Put( res_file, "," );
      Int_IO.Put( res_file, sysno, 1 );
      Put( res_file, "," );
      Int_IO.Put( res_file, iteration, 1 );
      Put( res_file, "," );
      Put( res_file, res.sernum'Img );
      Put( res_file, "," );
      Write_Quoted( res_file, wave'Img );
      Put( res_file, "," );
      Write_Quoted( res_file, hid'Img );
      Put( res_file, "," );
      Int_IO.Put( res_file, buno, 1 );
      Put( res_file, "," );
      Int_IO.Put( res_file, adno, 1 );
      Put( res_file, "," );
      
      Int_IO.Put( res_file, Means_Test_Result'Pos( res.passes_non_residential_capital_test ), 1 );
      Put( res_file, "," );
      Int_IO.Put( res_file, Means_Test_Result'Pos( res.passes_non_residential_income_test ), 1 );
      Put( res_file, "," );
      Int_IO.Put( res_file, Means_Test_Result'Pos( res.passes_residential_capital_test ), 1 );
      Put( res_file, "," );
      Int_IO.Put( res_file, Means_Test_Result'Pos( res.passes_residential_income_test ), 1 );
      Put( res_file, "," );
      Int_IO.Put( res_file, Means_Test_Result'Pos( res.passes_residential_means_test ), 1 );
      Put( res_file, "," );
      Int_IO.Put( res_file, Means_Test_Result'Pos( res.passes_non_residential_means_test ), 1 );
      Put( res_file, "," );
      rio.Put( res_file, res.la_contributions );
      Put( res_file, "," );
      rio.Put( res_file, res.client_contributions );
      Put( res_file, "," );
      rio.Put( res_file, res.gross_care_costs );
      Put( res_file, "," );
      rio.Put( res_file, res.total_payments_to_date );
      Put( res_file, "," );
      rio.Put( res_file, res.disposable_income );
      Put( res_file, "," );
      rio.Put( res_file, res.net_income );
      Put( res_file, "," );
      rio.Put( res_file, res.marginal_rate );
      Put( res_file, "," );
      rio.Put( res_file, res.capital_contribution );
      Put( res_file, "," );
      rio.Put( res_file, res.minimum_income_guarantee );
      Put( res_file, "," );
      rio.Put( res_file, res.hours_of_care_la );
      Put( res_file, "," );
      rio.Put( res_file, res.hours_of_care_private );
      Put( res_file, "," );
      Int_IO.Put( res_file, UAP_Level'Pos( res.uap ), 1 );
      Put( res_file, "," );
      rio.Put( res_file, res.remaining_capital_stock );
      Put( res_file, " );" );
      New_Line( res_file );
      for i in reverse Calculated_Incomes loop
         if( res.income( i ) /= 0.0 )then
            last_income := i;
            exit;
         end if;
      end loop;
      for i in Calculated_Incomes'First .. last_income loop
         if( res.income( i ) /= 0.0 )then
            Put_Line( inc_file, "insert into personal_income values( " );
            Write_Quoted( inc_file, wsc_run.username );
            Put( inc_file, "," );
            Put( inc_file, wsc_run.Run_Id'Img );
            Put( inc_file, "," );
            Write_Quoted( inc_file, To_Unbounded_String( res.sernum ));
            Put( inc_file, "," );
            Int_IO.Put( inc_file, sysno, 1 );
            Put( inc_file, "," );
            Int_IO.Put( inc_file, iteration, 1 );
            Put( inc_file, "," );
            Write_Quoted( inc_file, wave'Img );
            Put( inc_file, "," );
            Int_IO.Put( inc_file, Calculated_Incomes'Pos( i ));
            Put( inc_file, "," );
            Write_Quoted( inc_file, To_Unbounded_String( hid ));
            Put( inc_file, "," );
            Int_IO.Put( inc_file, buno, 1 );
            Put( inc_file, "," );
            Int_IO.Put( inc_file, adno, 1 );
            Put( inc_file, "," );
            rio.Put( inc_file, res.income( i ));
            Put( inc_file, " );" );
            New_Line( inc_file );
         end if;
      end loop;
   end Dump_As_SQL;

end Model.WSC.Results.DAO;
