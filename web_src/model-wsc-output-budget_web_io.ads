with Ada.Strings.Unbounded;

with Model.WSC.Formatting;
with General_Chart_Constants;
with HTML_Utils;
with IO_Commons;
with T_Utils.Web_IO;
with T_Utils;
with Tabulator_Commons;
with Text_Utils;
with WSC_Enums;
   
package Model.WSC.Output.Budget_Web_IO is

   use Ada.Strings.Unbounded;
   use Text_Utils;
   use Tabulator_Commons;
   use Model.WSC.Formatting;
   use IO_Commons;
   use WSC_Enums;
   use General_Chart_Constants;
   
   function Get_Summary(
      outputs       : Outputs_Rec; 
      lang          : Languages          := Languages'First ) return Unbounded_String;  
   
   function Get_Budget_Chart_Grid(
      outputs       : Outputs_Rec; 
      lang          : Languages          := Languages'First ) return Unbounded_String;
   
   function Get_Large_Budget_Chart_Page(
      outputs        : Outputs_Rec; 
      breakdown      : Disaggregated_Breakdown_Target   := Disaggregated_Breakdown_Target'First;
      col            : Costs_Type;
      print_counts   : Boolean            := False;
      lang           : Languages          := Languages'First ) return Unbounded_String;
      
   function Get_Budget_Table( 
      outputs        : Outputs_Rec; 
      sysno          : System_Number      := 1;
      breakdown      : Disaggregated_Breakdown_Target   := Disaggregated_Breakdown_Target'First;
      print_counts   : Boolean            := False;
      do_differences : Boolean            := False;
      lang           : Languages          := Languages'First ) return Unbounded_String;
      
   function Get_Budget_Chart_Popup_Link(
      outputs       : Outputs_Rec; 
      breakdown     : Breakdown_Target;
      lang          : Languages := Languages'First ) return Unbounded_String;
      
end  Model.WSC.Output.Budget_Web_IO;
