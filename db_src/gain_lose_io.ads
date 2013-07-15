--
-- Created by ada_generator.py on 2012-07-03 11:21:08.948413
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

with WSC_Enums;
with Base_Model_Types;

package Gain_Lose_IO is
  
   package d renames DB_Commons; 
   
   -- use Base_Types;
   use Ada.Strings.Unbounded;
   use Wsc_Db_Data;
   use WSC_Enums;
   use Base_Model_Types;

   function Retrieve_Income( 
      run_id    : Positive;
      username  : Unbounded_String;
      iteration : Positive;
      wave      : Waves;
      which     : Incomes_Type;
      op        : d.operation_type:= d.LT;
      max_results : Positive := 10 ) return Wsc_Db_Data.Gain_Lose_List.Vector;

   function Retrieve_Result( 
      run_id    : Positive;
      username  : Unbounded_String;
      iteration : Positive;
      wave      : Waves;
      which     : Results_Field_Name;
      op        : d.operation_type:= d.LT;
      max_results : Positive := 10 ) return Wsc_Db_Data.Gain_Lose_List.Vector;
      
      
end Gain_Lose_IO;
