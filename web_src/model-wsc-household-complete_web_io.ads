with WSC_Enums;
with Model.WSC.Run_Declarations;
with Model.WSC.Household.Database;
with Templates_Parser;

package Model.WSC.Household.Complete_Web_IO is

   use WSC_Enums;
   use Model.WSC.Run_Declarations;
   use Model.WSC.Household.Database;
   use Templates_Parser;
      
   function Write_Household( 
      db                 : DB_Type; 
      wsc_run            : Run; 
      hid                : Sernum_Value; 
      pid                : Sernum_value;
      iteration          : Positive; 
      wave               : Waves;
      extra_translations : Translate_Set ) return String;
         
end Model.WSC.Household.Complete_Web_IO;
