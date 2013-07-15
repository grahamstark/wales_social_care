with Model.WSC.Run_Declarations;
with Ada.Calendar;

package Model.WSC.Parameters.DAO is
   use Model.WSC.Run_Declarations;
   use Ada.Calendar;
   
   procedure Write( r : Run; start_year : Year_Number; end_year : Year_Number; sys : Parameters_Array );
   function Read( r : Run; start_year : Year_Number; end_year : Year_Number ) return Parameters_Array;
   
end Model.WSC.Parameters.DAO;
