with AUnit.Reporter.Text;
with AUnit.Run;
with Ada.Text_IO;

with WSC_Basic_Suite;

procedure WSC_Basic_Harness is

   use Ada.Text_IO;

   procedure Run is new AUnit.Run.Test_Runner( WSC_Basic_Suite );
   
   reporter : AUnit.Reporter.Text.Text_Reporter;
   
begin
   Put_Line( "starting" );
   Run( reporter );
end WSC_Basic_Harness;
