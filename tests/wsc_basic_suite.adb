with AUnit.Test_Suites;
with Ada.Text_IO;

-- with WSC_Tests;
-- with Ada.Command_Line;
-- with Maths_And_Probit_Tests;
-- with Model.WSC.Dynamic_Driver.Tests;
with Model.WSC.Static_Calculator.Tests;
-- with Model.WSC.Household.Transitions.Tests;
-- with DOM_And_Parameter_Tests;
-- with Model.WSC.Results.DAO.Tests;
-- with GNAT.Command_Line;

--
-- Driver routine for the test suite. See the AUnit v3 documentation.
--
function WSC_Basic_Suite return AUnit.Test_Suites.Access_Test_Suite is
--   use GNAT.Command_Line;
   use AUnit.Test_Suites;
   use Ada.Text_IO;

   result : Access_Test_Suite := new Test_Suite;
   
begin
   Put_Line( "WSC_Basic_Suite entered" );   

--    loop
--       Put_Line( "parsing params" );
       -- case Getopt ("s r d t m") is
          -- when ASCII.NUL => exit;
          -- when 's' =>
--             Add_Test( result, new Model.WSC.Results.DAO.Tests.Test_Case );
--         when 'r' => 
--            Add_Test( result, new Model.WSC.Dynamic_Driver.Tests.Test_Case );
--         when 'd' => 
            Add_Test( result, new Model.WSC.Static_Calculator.Tests.Test_Case );
--         when 't' => 
--            Add_Test( result, new Model.WSC.Household.Transitions.Tests.Test_Case );
--         when 'm' => 
--            Add_Test( result, new Maths_And_Probit_Tests.Test_Case );
         -- when others =>
            -- raise Program_Error;         -- cannot occur!
      -- end case;
   -- end loop;   
   return result;
   -- exception
      -- when Invalid_Switch    => Put_Line ("Invalid Switch " & Full_Switch);
      -- when Invalid_Parameter => Put_Line ("No parameter for " & Full_Switch);
end WSC_Basic_Suite;
