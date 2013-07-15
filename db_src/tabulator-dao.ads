with Ada.Strings.Unbounded;
with Model.WSC.Run_Declarations;
with WSC_Enums;

with WSC_DB_Data;

generic

package Tabulator.DAO is

   use Ada.Strings.Unbounded;
   use Model.WSC.Run_Declarations;
   use WSC_Enums;

   TOTAL_INDICATOR : constant := -999;
   
   procedure Save( r : Run; iteration : Positive; wave : Waves; table_name : Unbounded_String; t : Table_Expression );
   function Retrieve( r : Run; iteration : Positive; wave : Waves; table_name : Unbounded_String ) return Table_Expression;
   
   function Retrieve_Cell( r : Run; wave : Waves; table_name : Unbounded_String; row : Row_Range; col : Col_Range ) return WSC_DB_Data.Table_Stats; 
   
end Tabulator.DAO;
