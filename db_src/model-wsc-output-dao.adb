with GNATColl.Traces;

with Text_Utils;

package body Model.WSC.Output.DAO is
   
   use Text_Utils;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.OUTPUT.DAO" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   procedure Save( r : Run; iteration : Positive; wave : Waves; tables : Outputs_Rec ) is
      
      use Summary_Items_Package;
      comp_Cell    : Compare_Cell       := current_cell;
      cell_op      : Cell_Compare_Type  := counter;
      value_To_Use : Summary_Items_Type := disposable_income;
       
   begin
     Summary_DAO.Save( r, 1, iteration, wave, tables.summary_items_1 );
     Summary_DAO.Save( r, 2, iteration, wave, tables.summary_items_2 );
     Summary_DAO.Save( r, 3, iteration, wave, tables.summary_difference );
     
     Costs_By_Decile_DAO.Save( r, 1, iteration, wave, COSTS_BY_DECILE_KEY, tables.costs_by_decile( 1 ));   
     Costs_By_Decile_DAO.Save( r, 2, iteration, wave, COSTS_BY_DECILE_KEY, tables.costs_by_decile( 2 ));
     Costs_By_Decile_DAO.Save( r, 3, iteration, wave, COSTS_BY_DECILE_KEY, 
        Household_Costs_By_Decile.Difference( tables.costs_by_decile( 1 ), tables.costs_by_decile( 2 )));
     
     Costs_By_Tenure_DAO.Save( r, 1, iteration, wave, COSTS_BY_TENURE_KEY, tables.costs_by_tenure( 1 ));   
     Costs_By_Tenure_DAO.Save( r, 2, iteration, wave, COSTS_BY_TENURE_KEY, tables.costs_by_tenure( 2 ));
     Costs_By_Tenure_DAO.Save( r, 3, iteration, wave, COSTS_BY_TENURE_KEY, 
        Household_Costs_By_Tenure.Difference( tables.costs_by_tenure( 1 ), tables.costs_by_tenure( 2 )));

     Costs_By_Age_Band_DAO.Save( r, 1, iteration, wave, COSTS_BY_AGE_BAND_KEY, tables.costs_by_age_band( 1 ));   
     Costs_By_Age_Band_DAO.Save( r, 2, iteration, wave, COSTS_BY_AGE_BAND_KEY, tables.costs_by_age_band( 2 ));
     Costs_By_Age_Band_DAO.Save( r, 3, iteration, wave, COSTS_BY_AGE_BAND_KEY, 
        Household_Costs_By_Age_Band.Difference( tables.costs_by_age_band( 1 ), tables.costs_by_age_band( 2 )));
     declare
         tab : Gain_Lose_By_Tenure.Table_Expression;
      begin
         tab := Gain_Lose_By_Tenure.Express_Table( 
             tables.gains_by_tenure, 
             comp_cell, 
             cell_op, 
             value_to_use );
         Gain_Lose_By_Tenure_DAO.Save( r, iteration, wave, GAIN_LOSE_BY_TENURE_KEY, tab );
      end;
      declare
         tab : Gain_Lose_By_Decile.Table_Expression;
      begin
         tab := Gain_Lose_By_Decile.Express_Table( 
             tables.gains_by_decile, 
             comp_cell, 
             cell_op, 
             value_to_use );
         Gain_Lose_By_Decile_DAO.Save( r, iteration, wave, GAIN_LOSE_BY_DECILE_KEY, tab );
      end;
      declare
         tab : Gain_Lose_By_Age_Band.Table_Expression;
      begin
         tab := Gain_Lose_By_Age_Band.Express_Table( 
             tables.gains_by_age_band, 
             comp_cell, 
             cell_op, 
             value_to_use );
         Gain_Lose_By_Age_Band_DAO.Save( r, iteration, wave, GAIN_LOSE_BY_AGE_BAND_KEY, tab );
      end;
   end Save;
   
   procedure Retrieve( r : Run; iteration : Positive; wave : Waves; tables : in out Outputs_Rec ) is
   begin
      
     -- tables.summary_items_1 := Summary_DAO.Retrieve( r, 1, no_statistic, iteration, wave );
     -- tables.summary_items_2 := Summary_DAO.Retrieve( r, 2, no_statistic, iteration, wave );
     -- tables.summary_difference := Summary_DAO.Retrieve( r, 3, iteration, wave );
     null; -- TODO
   end Retrieve;
   
end Model.WSC.Output.DAO;
