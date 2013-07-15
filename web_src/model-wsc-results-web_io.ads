with Model.WSC.Household;

package Model.WSC.Results.Web_IO is

   use Model.WSC.Household;
   
   function To_String( p1, p2, diff : Personal_Result ) return String;
   
   function To_String( h1, h2, diff : Household_Result ) return String;
   
end Model.WSC.Results.Web_IO;
