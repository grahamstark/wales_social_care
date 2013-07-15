--
-- Created by ada_generator.py on 2012-07-24 19:03:56.029201
-- 
with AUnit.Test_Suites; use AUnit.Test_Suites;

with Wsc_Db_Test;

function Suite return Access_Test_Suite is
        result : Access_Test_Suite := new Test_Suite;
begin
        Add_Test( result, new Wsc_Db_Test.test_Case ); -- Adrs_Data_Ada_Tests.Test_Case
        return result;
end Suite;
