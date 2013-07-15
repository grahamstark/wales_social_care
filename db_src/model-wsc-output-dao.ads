with Costs_Tabulator.DAO;
with Tabulator.DAO;
with Model.WSC.Summary_Items_DAO;
with Tabulator_Commons;
with WSC_Enums;
with Ada.Strings.Unbounded;

package Model.WSC.Output.DAO is
   use WSC_Enums;
   use Tabulator_Commons;
   use Ada.Strings.Unbounded;
   
   COSTS_BY_DECILE_KEY       : constant Unbounded_String := To_Unbounded_String( "costs_by_decile" );
   COSTS_BY_TENURE_KEY       : constant Unbounded_String := To_Unbounded_String( "costs_by_tenure" );
   COSTS_BY_AGE_BAND_KEY     : constant Unbounded_String := To_Unbounded_String( "costs_by_age_band" );
   
   GAIN_LOSE_BY_DECILE_KEY   : constant Unbounded_String := To_Unbounded_String( "gain_lose_by_decile" );
   GAIN_LOSE_BY_AGE_BAND_KEY : constant Unbounded_String := To_Unbounded_String( "gain_lose_by_age_band" );
   GAIN_LOSE_BY_TENURE_KEY   : constant Unbounded_String := To_Unbounded_String( "gain_lose_by_tenure" );
   
   package Summary_DAO renames Model.WSC.Summary_Items_DAO;

   package Costs_By_Tenure_DAO is new 
      Household_Costs_By_Tenure.DAO;
   package Costs_By_Decile_DAO is new 
      Household_Costs_By_Decile.DAO;
   package Costs_By_Age_band_DAO is new 
      Household_Costs_By_Age_Band.DAO;

   package Gain_Lose_By_Tenure_DAO is new 
      Gain_Lose_By_Tenure.DAO;
   package Gain_Lose_By_Decile_DAO is new 
      Gain_Lose_By_Decile.DAO;
   package Gain_Lose_By_Age_band_DAO is new 
      Gain_Lose_By_Age_band.DAO;
   
   procedure Save( r : Run; iteration : Positive; wave : Waves; tables : Outputs_Rec );
   procedure Retrieve( r : Run; iteration : Positive; wave : Waves; tables : in out Outputs_Rec );
   
end Model.WSC.Output.DAO;
