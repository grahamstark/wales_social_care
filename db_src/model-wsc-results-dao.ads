with Model.WSC.Run_Declarations;
with Model.WSC.Parameters;
with Ada.Text_IO;

package Model.WSC.Results.DAO is
   
   use Model.WSC.Run_Declarations;
   use Model.WSC.Parameters;
   
   function Highest_Values_Of( 
      wsc_run    : Run;
      -- start_wave : Waves;
      end_wave   : Waves; 
      sysno      : System_Number; 
      iteration  : Iteration_Number;
      pno        : Sernum_Value ) return Calculated_Incomes_Array;
      
   function Value_Of( 
      wsc_run   : Run; 
      wave      : Waves; 
      sysno     : System_Number; 
      iteration : Iteration_Number;
      pno       : Sernum_Value; 
      which     : Calculated_Incomes; 
      default   : Amount := 0.0 ) return Amount;

   procedure Set( 
      wsc_run   : Run; 
      hh        : Model.WSC.Household.Household; 
      sysno     : System_Number; 
      iteration : Iteration_Number;
      res       : Household_Result;
      min_age   : Age_Range := Age_Range'First;
      incomes_only : Boolean := False );
      
   function Get( 
      wsc_run   : Run; 
      hh        : Model.WSC.Household.Household; 
      sysno     : System_Number;
      iteration : Iteration_Number ) return Household_Result;
      
   procedure Delete_For_Run( wsc_run : Run );
   
   function Get( 
      wsc_run   : Run; 
      wave      : Waves; 
      sysno     : System_Number; 
      iteration : Iteration_Number;
      pid       : Sernum_Value ) return Personal_Result;
      
   procedure Dump_As_SQL(
      res_file  : Ada.Text_IO.File_Type;
      inc_file  : Ada.Text_IO.File_Type;
      wave      : Waves;
      sysno     : System_Number;
      iteration : Iteration_Number;
      hid       : Sernum_Value;
      buno      : Benefit_Unit_Number;
      adno      : Person_Number;
      wsc_run   : Run;
      is_first  : Boolean;
      is_last   : Boolean;
      res       : Personal_Result );
      
   procedure Dump_As_SQL(
      res_file  : Ada.Text_IO.File_Type;
      inc_file  : Ada.Text_IO.File_Type;
      sysno     : System_Number;
      iteration : Iteration_Number;
      wsc_run   : Run;
      is_first  : Boolean;
      is_last   : Boolean;
      hh        : Model.WSC.Household.Household; 
      res       : Household_Result );
     
end Model.WSC.Results.DAO;
