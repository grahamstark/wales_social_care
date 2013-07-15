with WSC_DB_Data;

with Ada.Strings.Unbounded;

package Model.WSC.Stats_Table_Commons is

   use Ada.Strings.Unbounded;

   --
   -- fixme dup of Model.WSC.Output.Web_IO
   --
   function Stats_To_Table( 
      stats      : WSC_DB_Data.Table_Stats; 
      title      : String;
      id         : String;
      divisor    : Amount;
      class      : String;
      as_real    : Boolean;
      which      : WSC_DB_Data.Stats_Selection := 1 ) return Unbounded_String;
   
      
end Model.WSC.Stats_Table_Commons;
