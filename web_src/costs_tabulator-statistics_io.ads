with Ada.Strings.Unbounded;
with Run_IO;
with Model.WSC.Run_Declarations;
with WSC_Enums;
--
-- I'm in child package/generics hell here, but can't see another way to do this ..
--
generic
    
package Costs_Tabulator.Statistics_IO is
   
   use Ada.Strings.Unbounded;
   use Model.WSC.Run_Declarations;
   use WSC_Enums;
   
   function To_String(
         wsc_run        : Run;     
         wave           : Waves;
         table_name     : Unbounded_String;
         sysno          : Positive;
         print_counts   : Boolean;
         name           : String;
         description    : String ) return Unbounded_String;

end Costs_Tabulator.Statistics_IO;
