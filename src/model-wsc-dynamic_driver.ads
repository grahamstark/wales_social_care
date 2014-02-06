with Ada.Strings.Unbounded;

with Model.WSC.Run_Settings;
with Model.WSC.Household.Database;
with Model.WSC.Output;
with Model.WSC.Parameters;
with Model.WSC.Results;
with Model.WSC.Run_Declarations;
with Model.WSC.Static_Calculator;
with WSC_Enums;
with Model.WSC.Household.Weights;

package Model.WSC.Dynamic_Driver is

   use Ada.Strings.Unbounded;
   use WSC_Enums;
   use Model.WSC.Run_Settings;
   use Model.WSC.Run_Declarations;
   use Model.WSC.Output;
   use Model.WSC.Parameters;
   use Model.WSC.Household;
   use Model.WSC.Results;
   
   use Model.WSC.Household.Database;
   use Model.WSC.Static_Calculator;
      
   package mww renames Model.WSC.Household.Weights;
      
   procedure Run_Model(
      wsc_run        : in Run;
      pre_sys        : in Parameters_Array;
      post_sys       : in Parameters_Array;
      state          : out State_Type;
      monitor        : in out Model_Monitor'Class );
private
   -- for unit testing
   procedure Make_Needs_Score_Thresholds(
      wsc_run        : in Run;
      db          : in out DB_Type;
      wave        : Waves;
      sysno       : System_Number;
      iteration   : Positive;
      proportions : UAP_Array;
      calculator  : Make_Needs_Score_Access;
      thresholds  : out UAP_Array;
      weighter    : mww.Weighter;
      monitor     : in out Model_Monitor'Class );
   
end Model.WSC.Dynamic_Driver;
