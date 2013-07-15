with Ada.Containers.Ordered_Maps;

with Model.WSC.Household;
with WSC_Enums;
with Model.WSC.Run_Declarations;

package Model.WSC.Results is

   use Model.WSC.Household;
   use WSC_Enums;
   use Model.WSC.Run_Declarations;

   
   type Personal_Result is record
      sernum                            : Sernum_Value;
      is_residential                    : Boolean;
      receives_social_care              : Boolean := False;

      lifetime_gross_payments           : Amount := 0.0;
      lifetime_client_contributions     : Amount := 0.0;
      lifetime_la_contributions         : Amount := 0.0;
      lifetime_capital_contributions    : Amount := 0.0;
      
      highest_la_contribution           : Amount := 0.0; 
       
      income                            : Calculated_Incomes_Array := ( others => 0.0 );      
      highest_previous_income           : Calculated_Incomes_Array := ( others => 0.0 );      
      
      tarriff_income                        : Amount := 0.0;
      
      passes_non_residential_capital_test  : Means_Test_Result := not_applicable;
      passes_non_residential_income_test   : Means_Test_Result := not_applicable;
      passes_residential_capital_test      : Means_Test_Result := not_applicable;
      passes_residential_income_test       : Means_Test_Result := not_applicable;
      
      la_contributions                  : Amount := 0.0;
      client_contributions              : Amount := 0.0;
      gross_care_costs                  : Amount := 0.0;
      total_payments_to_date            : Amount := 0.0;
      disposable_income                 : Amount := 0.0;
      net_income                        : Amount := 0.0;
      marginal_rate                     : Amount := 0.0;
      capital_contribution              : Amount := 0.0;
      minimum_income_guarantee          : Amount := 0.0;
      summary                           : Summary_Items_Array;
      costs_summary                     : Costs_Array;
      intermediate                      : Auxiliary_Results;
      passes_residential_means_test     : Means_Test_Result := not_applicable;
      passes_non_residential_means_test : Means_Test_Result := not_applicable;
      hours_of_care_la                  : Amount := 0.0;
      hours_of_care_private             : Amount := 0.0;
      uap                               : UAP_Level := UAP_Level'First;
      remaining_capital_stock           : Amount := 0.0;
   end record;
                                                 
   function Which_Incomes_Received( a : Calculated_Incomes_Array ) return Income_Package.Set;
   type Personal_Array is array( Adult_Number ) of Personal_Result;

   type Benefit_Unit_Results_Data is record
      num_people               : Person_Count;
      receives_social_care     : Boolean;
      income                   : Calculated_Incomes_Array := ( others => 0.0 );

      tarriff_income           : Amount := 0.0;

      la_contributions         : Amount := 0.0;
      client_contributions     : Amount := 0.0;
      gross_care_costs         : Amount := 0.0;
      total_payments_to_date   : Amount := 0.0;
      remaining_capital_stock  : Amount := 0.0;
      disposable_income        : Amount := 0.0;
      net_income               : Amount := 0.0;
      marginal_rate            : Amount := 0.0;
      capital_contribution     : Amount := 0.0;
      minimum_income_guarantee : Amount := 0.0;
      equivalence_scale        : Amount := 0.0;
      hours_of_care_la         : Amount := 0.0;
      hours_of_care_private    : Amount := 0.0;

      lifetime_gross_payments           : Amount := 0.0;
      lifetime_client_contributions     : Amount := 0.0;
      lifetime_la_contributions         : Amount := 0.0;
      lifetime_capital_contributions    : Amount := 0.0;
      
      summary                  : Summary_Items_Array;
      costs_summary            : Costs_Array;
      intermediate             : Auxiliary_Results;
   end record;
   
   type Benefit_Unit_Result is record
      res                      : Benefit_Unit_Results_Data;
      people                   : Personal_Array;
      people_last_period       : Personal_Array;
   end record;
   
   -- procedure Add renames Base_Model_Types.Add_To_Map;

   type Benefit_Unit_Array is array( Benefit_Unit_Number ) of  Benefit_Unit_Result;

   type Household_Results_Data is record
      sernum                   : Sernum_Value;
      num_benefit_units        : Benefit_Unit_Count;
      receives_social_care     : Boolean;
      income                   : Calculated_Incomes_Array := ( others => 0.0 );
      la_contributions         : Amount := 0.0;
      tarriff_income           : Amount := 0.0;
      client_contributions     : Amount := 0.0;
      gross_care_costs         : Amount := 0.0;
      total_payments_to_date   : Amount := 0.0;
      remaining_capital_stock  : Amount := 0.0;
      disposable_income        : Amount := 0.0;
      net_income               : Amount := 0.0;
      marginal_rate            : Amount := 0.0;
      capital_contribution     : Amount := 0.0;
      minimum_income_guarantee : Amount := 0.0;
      equivalence_scale        : Amount := 0.0;
      hours_of_care_la         : Amount := 0.0;
      hours_of_care_private    : Amount := 0.0;

      lifetime_gross_payments            : Amount := 0.0;
      lifetime_client_contributions      : Amount := 0.0;
      lifetime_la_contributions          : Amount := 0.0;
      lifetime_capital_contributions     : Amount := 0.0;

      costs_summary            : Costs_Array;
      summary                  : Summary_Items_Array;
      intermediate             : Auxiliary_Results;
   end record;

   
   type Household_Result is record
      is_used_as_example       : Boolean;
      res                      : Household_Results_Data;
      benefit_units            : Benefit_Unit_Array;
   end record;
   
   type Household_Result_Access is access Household_Result;
   procedure Free( hh : in out Household_Result_Access );
   
   type Household_Results_Array is array( Waves ) of Household_Result;
   
   procedure Zero( hh : out Household_Result; clear_historical : Boolean );
   procedure Zero( res : in out Personal_Result; clear_historical : Boolean );
   procedure Accumulate( 
      hh       : Model.WSC.Household.Household; 
      res      : in out Household_Result; 
      wsc_run : Run );
      
   function Difference( res1, res2 : Household_Result ) return Household_Result;
   
   function To_String( intermediate : Auxiliary_Results; indent : String ) return String;
   
   function To_String( 
      res : Personal_Result; 
      include_non_zeros : Boolean := False ) return String;
      
   function To_String( 
      bures : Benefit_Unit_Result; 
      include_non_zeros : Boolean := False ) return String;

   function To_String( 
      res : Household_Result; 
      include_non_zeros : Boolean := False ) return String;

   function Calculate_Income( 
      gross_income     : Incomes_Array;
      calc_income      : Calculated_Incomes_Array;
      which_to_include : Included_Incomes_Array ) return Amount;
  
   function Calculate_Income(
      bu               : Benefit_Unit; 
      res              : Benefit_Unit_Result;
      which_to_include : Included_Incomes_Array ) return Amount;
      
   function Compare_Results_Equal( pre, post : Personal_Result ) return Boolean;
  
   function Make_Summary_Items( 
     ad  : Person;
     res : Personal_Result ) return Summary_Items_Array;
     
     
 
private     
    
   -- type Results_DB is tagged private;
   -- 
    -- procedure Set( db : in out Results_DB; wave : Waves; res : Household_Results_Data );
    -- function Get( db : Results_DB; wave : Waves; hno : Sernum_Value ) return Household_Results_Data;
    -- function HH_Result_Exists( db : Results_DB; wave : Waves; hno : Sernum_Value ) return Boolean;
    -- 
    -- procedure Set( db : in out Results_DB; wave : Waves; res : Personal_Result );
    -- function Get( db : Results_DB; wave : Waves; pno : Sernum_Value ) return Personal_Result;
    -- function Personal_Result_Exists( db : Results_DB; wave : Waves; pno : Sernum_Value ) return Boolean;
    -- function Highest_Value_Of( db : Results_DB; start_wave, end_wave : Waves; pno : Sernum_Value; which : Calculated_Incomes ) return Amount;
    -- function Value_Of( db : Results_DB; wave : Waves; pno : Sernum_Value; which : Calculated_Incomes; default : Amount := 0.0 ) return Amount;
 -- 
    -- procedure Set( db : in out Results_DB; wave : Waves; hid : Sernum_Value; buno : Benefit_Unit_Number; res : Benefit_Unit_Results_Data );
    -- function Get( db : Results_DB; wave : Waves; hid : Sernum_Value;  buno : Benefit_Unit_Number ) return Benefit_Unit_Results_Data;
 -- 
    -- procedure Set( db : in out Results_DB; wave : Waves; res : Household_Result );
    -- function Get( db : Results_DB; hh : Model.WSC.Household.Household ) return Household_Result;
    -- function BU_Result_Exists( db : Results_DB; wave : Waves; hid : Sernum_Value;  buno : Benefit_Unit_Number ) return Boolean;
    -- 
    -- function Get_Empty_DB return Results_DB;
    -- 
   -- 
   -- type Wave_And_Id is record
      -- wave : Waves;
      -- id  : Sernum_Value;
   -- end record;
-- 
   -- type Wave_Hid_And_BuNo is record
      -- wave : Waves;
      -- hid  : Sernum_Value;
      -- buno : Benefit_Unit_Number;
   -- end record;
-- 
  -- 
   -- function Compare( l, r : Wave_And_Id ) return Boolean;
   -- function Compare( l, r : Wave_Hid_And_BuNo ) return Boolean;
   -- function Equal( l, r : Household_Results_Data ) return Boolean;
   -- function Equal( l, r : Benefit_Unit_Results_Data ) return Boolean;
   -- function Equal( l, r : Personal_Result ) return Boolean;
  -- 
   -- package BU_Data_Package is new Ada.Containers.Ordered_Maps(
      -- Key_Type   => Wave_Hid_And_BuNo,
      -- Element_Type => Benefit_Unit_Results_Data,
      -- "<" =>  Compare,
      -- "=" =>  Equal );      
-- 
   -- subtype BU_Map is BU_Data_Package.Map;
-- 
   -- package Pers_Data_Package is new Ada.Containers.Ordered_Maps(
      -- Key_Type   => Wave_And_Id,
      -- Element_Type => Personal_Result,
      -- "<" =>  Compare,
      -- "=" =>  Equal );      
-- 
   -- subtype Pers_Map is Pers_Data_Package.Map;
   -- 
   -- package HH_Data_Package is new Ada.Containers.Ordered_Maps(
      -- Key_Type   => Wave_And_Id,
      -- Element_Type => Household_Results_Data,
      -- "<" =>  Compare,
      -- "=" =>  Equal );      
-- 
   -- subtype HH_Map is HH_Data_Package.Map;
   -- 
   -- type Results_DB is tagged record
      -- hh : HH_Map;
      -- bu : BU_Map;
      -- pers : Pers_Map;
   -- end record;
  
end Model.WSC.Results;
