--
--  $Author: graham_s $
--  $Date: 2011-04-06 15:56:14 +0100 (Wed, 06 Apr 2011) $
--  $Revision: 11372 $
--
pragma License( Modified_GPL );

with Ada.Command_Line;
with Ada.Direct_IO;
with Ada.Calendar;

with AUnit.Assertions;   
with Ada.Text_IO;

with Base_Model_Types;

with Model.WSC.Household;
with Model.WSC.Household.Web_IO;
with Model.WSC.Household.Examples;
with Model.WSC.Results.Web_IO;
with Model.WSC.Results;
with Model.WSC.Run_Declarations;
with Event_Counter;
with Event_Counter.Web_IO;
with T_Utils;

with GNATColl.Traces;

package body Model.WSC.HTML_Tests is
   
   use AUnit.Test_Cases.Registration;
   use AUnit.Assertions;
   use Ada.Text_IO;
   use Model.WSC.Results;
   use Base_Model_Types;
   
   default_settings : Model.WSC.Run_Declarations.Run;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.HTML_TESTS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

 
   
   procedure Test_Make_HTML( test_case : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Ada.Calendar;
      use Model.WSC.Household.Examples;
      hh                : Model.WSC.Household.Household := Get_Household( single_retired_person );
      year              : Year_Number;
      pers_results      : Personal_Result;
      hh_result         : Household_Result;
      ad                : Model.WSC.Household.Person renames hh.benefit_units( 1 ).adults( 1 );
      f                 : File_Type;
   begin      
      Create( f, Out_File, "tmp/html_dump.html" );
      Put_Line( "HOUSEHOLD DATA - ");
      Put_Line( Model.WSC.Household.Web_IO.To_String( hh.hdata ));
      Put_Line( "ADULT DATA - ");
      Put_Line( Model.WSC.Household.Web_IO.To_String( ad ));
      Put_Line( "COMPLETE SYSTEM " );
      Put_Line( f, Model.WSC.Results.Web_IO.To_String( hh, hh_result, hh_result, hh_result ));
      Close( f );
   end Test_Make_HTML;

   procedure Set_Up( tc : in out Test_Case) is
   begin
      Put_Line( "Model.WSC.Static_Calculator.Tests:: SET UP" );
      -- rand_list.Reset;
      default_settings := Model.WSC.Run_Declarations.Read_Settings( Ada.Command_Line.Argument( 1 ));
   end Set_Up;      

   --------------------
   -- Register_Tests --
   --------------------
   procedure Register_Tests( tc : in out Test_Case ) is
      use AUnit.Test_Cases.Registration;
   begin
      Put_Line( "Static_Tests: register tests" );
      Register_Routine( tc, Test_Make_HTML'Access, "Test Make HTML" );
   end Register_Tests;

   ----------
   -- Name --
   ----------
   function Name ( tc : Test_Case ) return Message_String is
   begin
      return Format( "Model.WSC.Static_Calculator.Tests" );
   end Name;

end Model.WSC.HTML_Tests;
