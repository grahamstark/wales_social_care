with Ada.Strings.Unbounded;
with Model.WSC.Run_Declarations;
with WSC_Enums;
with Model.WSC.Parameters;

generic

package Costs_Tabulator.DAO is

   use Ada.Strings.Unbounded;
   use Model.WSC.Run_Declarations;
   use WSC_Enums;
   use Model.WSC.Parameters;
   procedure Save( r : Run; sysno : Positive; iteration : Positive; wave : Waves; table_name : Unbounded_String; t : Table_Type );
   function Retrieve( r : Run; sysno : Positive; iteration : Positive; wave : Waves; table_name : Unbounded_String ) return Table_Type;
   
end Costs_Tabulator.DAO;
