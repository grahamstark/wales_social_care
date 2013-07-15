with AWS.Parameters;

package Model.WSC.Household.Web_IO is
 
   function To_String( pers : Person; print_regressors : Boolean := False ) return String;
   function To_String( hh : Household ) return String;
   function To_String( hdata : Household_Data ) return String;
   
   type Direction is ( prev, curr, next );

   function Personal_Url( 
      wave      : Waves;
      hid       : Sernum_Value;
      pid       : Sernum_Value;
      iteration : Positive ) return String;
   
   function Person_To_URL( 
      wave            : Waves;
      pers            : Person; 
      iteration       : Positive := 1; 
      which_direction : Direction := curr ) return String;

   procedure Get_Person_Ids_From_URL(
      params       : AWS.Parameters.List; 
      wave         : out Waves; 
      hid          : out Sernum_Value;
      pid          : out Sernum_Value;
      iteration    : out Positive );

end Model.WSC.Household.Web_IO;
