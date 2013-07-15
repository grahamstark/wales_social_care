with Ada.Strings.Unbounded;
with Model.WSC.Run_Declarations;
with WSC_Enums;
with Model.WSC.Parameters;

package Model.WSC.Summary_Items_DAO is

   use Ada.Strings.Unbounded;
   use Model.WSC.Run_Declarations;
   use WSC_Enums;
   use Model.WSC.Parameters;
   
   SUMMARY_KEY : constant Unbounded_String := To_Unbounded_String( "summary_items" );
 
   procedure Save( r : Run; sysno : Positive; iteration : Positive; wave : Waves; t : Summary_Items_Array );
   
   function Retrieve( 
      wsc_run      : Run; 
      measure      : Summary_Statistics_Type;
      summary_item : Summary_Items_Type;
      system       : Positive; 
      iteration    : Positive := 1 
      ) return Abs_Waves_Array;

   function Retrieve( 
      wsc_run   : Run; 
      measure   : Summary_Statistics_Type; 
      system    : Positive; 
      wave      : Waves;
      iteration : Positive := 1 
      ) return Summary_Items_Array;
      
end Model.WSC.Summary_Items_DAO;
