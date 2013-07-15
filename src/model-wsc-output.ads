with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Deallocation;

with Costs_Tabulator;
with Inequality_Generator;
with Tabulator;
with Tabulator_Commons;
with T_Utils;
with Poverty_Tabulator;

with WSC_Enums;

with Model.WSC.Household;
with Model.WSC.Results;
with WSC_Enums;
with Model.WSC.Gain_Lose;
with Model.WSC.Inequality;
with Model.WSC.Budget;
with Model.WSC.Run_Declarations;
with Model.WSC.Parameters;

package Model.WSC.Output is
   use Model.WSC.Household;
   use Model.WSC.Results;
   use WSC_Enums;
   use Model.WSC.Gain_Lose;
   use Model.WSC.Inequality;
   use Model.WSC.Budget;
   use Model.WSC.Run_Declarations;
   use Model.WSC.Parameters;
   
   type Breakdown_Target is ( no_breakdown, by_decile, by_age_of_head, by_tenure );
   subtype Disaggregated_Breakdown_Target is Breakdown_Target range by_age_of_head .. Breakdown_Target'Last;

   type Amount_Array is array( System_Number'Range ) of Amount;
   
   type Summary_Statistics_Type is (
      average_gain,
      percent_gaining,
      percent_losing,
      total_cost,
      -- change_in_poverty_rate,
      change_in_gini,
      hh_count,
      pers_count );
      
   type Summary_Statistics_Array is array( Summary_Statistics_Type ) of Amount;
     
   type Outputs_Rec is tagged record
      
      initialised             : Boolean := False;
      is_null                 : Boolean := False;
      
      summary_statistics      : Summary_Statistics_Array := ( Others => 0.0 );
      summary_items_1         : Summary_Items_Array := ( Others => 0.0 );
      summary_items_2         : Summary_Items_Array := ( Others => 0.0 );
      --
      -- we need this because of all the iterations - so we can tell the highest and lowest possible
      -- change.
      --
      summary_difference      : Summary_Items_Array := ( Others => 0.0 );
      
      poverty_line_per_person : Amount_Array;
      
      ineq                    : Lorenz_And_Gini_By_Sys_Array;
      
      gains_by_decile         : Gain_Lose_By_Decile.Table_Type;
      gains_by_tenure         : Gain_Lose_By_Tenure.Table_Type;
      gains_by_age_band       : Gain_Lose_By_Age_Band.Table_Type;
      
      ineq_by_decile          : Lorenz_And_Gini_By_Decile_And_Sys_Array;
      ineq_by_tenure          : Lorenz_And_Gini_By_Tenure_And_Sys_Array;
      ineq_by_age_band        : Lorenz_And_Gini_By_Age_Band_And_Sys_Array;
      
      costs_by_decile         : Household_Costs_By_Decile_Array;
      costs_by_tenure         : Household_Costs_By_Tenure_Array;
      costs_by_age_band       : Household_Costs_By_Age_Band_Array;
      
   end record;

   function Pretty_Print( t : Breakdown_Target ) return String;
   procedure Print_Outputs( filename : String; outputs : in out Outputs_Rec );
   procedure Initialise_Outputs( outputs : in out Outputs_Rec );
   
   function Is_Initialised( outputs : Outputs_Rec ) return Boolean;

   -- type Outputs_Array is tagged private;

   -- type Outputs_Array_Access is access Outputs_Array;

   -- procedure Free( o : in out Outputs_Array_Access );

   procedure Add_To_Outputs( 
      hh_ref   : Positive;
      hh       : Model.WSC.Household.Household; 
      year     : Simulation_Years;
      weight   : Amount;
      result_1 : Household_Result; 
      result_2 : Household_Result;
      outputs  : in out Outputs_Rec );
   
   -- function Get_Null_Output return Outputs_Array_Access;
   
   -- function Get_Output_For_Year( outa : Outputs_Array_Access; year : Simulation_Years ) return Outputs_Rec;
   
   type Years_Array is array( Simulation_Years range <> ) of Simulation_Years;
   
   -- function Get_Available_Years( o : Outputs_Array ) return Years_Array;
   
-- private  
   
   -- function Compare_Outputs_Rec( l,r : Outputs_Rec ) return Boolean;
-- 
   -- package Outputs_By_Year is new 
      -- Ada.Containers.Ordered_Maps(
         -- Key_Type     => Simulation_Years,
         -- Element_Type => Outputs_Rec,
         -- "<"          => "<",
         -- "="          => Compare_Outputs_Rec );
         -- 
   -- type Outputs_Array is new Outputs_By_Year.Map with null record;
   -- procedure Free_Outputs_Array is new Ada.Unchecked_Deallocation( 
      -- Object => Outputs_Array,
      -- Name => Outputs_Array_Access );

   
end Model.WSC.Output;
