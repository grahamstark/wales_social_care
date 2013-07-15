with WSC_Suite;
with AUnit.Run;
with AUnit.Reporter.Text;


procedure WSC_Harness is

   procedure Run is new AUnit.Run.Test_Runner( WSC_Suite );
   
   reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run( reporter );
end WSC_Harness;
