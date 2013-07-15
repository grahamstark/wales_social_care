with AUnit.Test_Suites;

with WSC_Tests;
with Model.WSC.Household.Transitions.Tests;
with DOM_And_Parameter_Tests;
with Ada.Command_Line;
with Model.WSC.Static_Calculator.Tests;
--
-- Driver routine for the test suite. See the AUnit v3 documentation.
--
function WSC_Suite return AUnit.Test_Suites.Access_Test_Suite is
   
   use AUnit.Test_Suites;
   
   result : Access_Test_Suite := new Test_Suite;
   
begin
   if( Ada.Command_Line.Argument_Count >= 3 ) and then
        ( Ada.Command_Line.Argument( 3 ) = "full" )then
         Add_Test( result, new WSC_Tests.Test_Case );
         Add_Test( result, new Model.WSC.Household.Transitions.Tests.Test_Case );
         Add_Test( result, new Model.WSC.Static_Calculator.Tests.Test_Case );
   end if; 
   Add_Test( result, new Dom_And_Parameter_Tests.Test_Case );
   
   return result;
end WSC_Suite;
