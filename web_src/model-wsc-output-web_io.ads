-- with Model.WSC.Output.Inequality_Web_IO;
with Ada.Strings.Unbounded;
with Ada.Calendar;

with Model.WSC.Formatting;
with Model.WSC.Output.Budget_Web_IO;
with Model.WSC.Output.Gain_Lose_Web_IO;
--  with Model.WSC.Output.Poverty_Web_IO;

with HTML_Utils;
with IO_Commons;
with T_Utils.Web_IO;
with T_Utils;
with Tabulator_Commons;
with Templates_Parser;
with Text_Utils;
with General_Chart_Constants;
with Tabulator.Statistics_IO;

package Model.WSC.Output.Web_IO is

   use Ada.Strings.Unbounded;
   use Text_Utils;
   use Tabulator_Commons;
   use Model.WSC.Formatting;
   use IO_Commons;
   use Templates_Parser;
   use Ada.Calendar;
   use General_Chart_Constants;
   
   function Draw_Time_Series_Fan_Chart( 
      wsc_run      : Run;
      summary_item : Summary_Items_Type; 
      system       : Pre_Or_Post;
      size         : Chart_Size;
      is_svg       : Boolean ) return Unbounded_String;
      
   function Get_Single_Table( 
      wsc_run      : Run;
      summary_item : Summary_Items_Type;
      do_comparisons : Boolean ) return Unbounded_String;      
      
   function Get_Summary_Table( 
      outputs        : Outputs_Rec;
      lang           : Languages        := Languages'First ) return Unbounded_String;

   function Get_All_Years_Summary( wsc_run : Run ) return Unbounded_String;      

   function Get_Summary_Control_Section(
      sysno          : Positive      := 1;
      do_differences : Boolean            := False;
      lang           : Languages          := Languages'First) return Unbounded_String;

   function Get_Stats_Summary_Page(
      wsc_run : Run;
      sysno   : Positive;
      wave    : Waves ) return Unbounded_String;

   function Convert_Summary_Results_To_CSV( wsc_run : Run ) return Unbounded_String;
   
   function Get_Budget_Table(      
      wsc_run        : Run;
      wave           : Waves;
      sysno          : System_Number      := 1;
      breakdown      : Disaggregated_Breakdown_Target   := Disaggregated_Breakdown_Target'First;
      print_counts   : Boolean            := False;
      do_differences : Boolean            := False ) return Unbounded_String;
      
  function Get_Budget_Control_Section(
      sysno          : System_Number      := 1;
      breakdown      : Disaggregated_Breakdown_Target   := Disaggregated_Breakdown_Target'First;
      print_counts   : Boolean            := False;
      do_differences : Boolean            := False;
      lang           : Languages          := Languages'First) return Unbounded_String;
      
  function Get_Gain_Lose_Table(
      wsc_run : Run;
      wave    : Waves;
      which   : Breakdown_Target ) return Unbounded_String;
      
   function Get_Gain_Lose_Control_Section(
      breakdown        : Disaggregated_Breakdown_Target   := Disaggregated_Breakdown_Target'First;
      comp_cell        : Compare_Cell       := current_cell;
      cell_op          : Cell_Compare_Type  := counter;
      value_To_Use     : Summary_Items_Type := disposable_income;
      lang             : Languages := Languages'First;
      advanced_version : Boolean := False ) return Unbounded_String;
      
   type Output_Page_Type is ( stats_page, summary_page, gain_lose_page, budget_page, examples_page, inequality_page, poverty_page );  
   
   function Get_Output_Menu( 
      start_year : Year_Number;
      end_year   : Year_Number;
      which_year : Year_Number;
      which_page : Output_Page_Type;
      lang       : Languages ) return Unbounded_String;
   
   
   function Get_Output_Page( 
                             title           : Unbounded_String;
                             translations    : Translate_Set;
                             which_page      : Output_Page_Type;
                             start_year      : Year_Number;
                             end_year        : Year_Number;
                             which_year      : Year_Number;
                             breadcrumb      : Unbounded_String;
                             control_section : Unbounded_String;
                             gallery         : Unbounded_String;
                             content         : Unbounded_String;
                             lang            : Languages ) return Unbounded_String;
                              
  package WSC_HTML is new HTML_Utils( Rate=>Rate, Counter_Type=>Counter_Type );
   
end  Model.WSC.Output.Web_IO;
