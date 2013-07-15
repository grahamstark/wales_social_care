--
-- copyright(c) 2009 Graham Stark/ Virtual Worlds (graham.stark@virtual-worlds.biz)
--
-- ////////////////////////////////
--
-- This is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 3, or (at your option)
-- any later version.
-- 
-- It is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this software; see the file docs/gpl_v3.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street,
-- Boston, MA 02110-1301, USA.
-- 
-- /////////////////////////////
pragma License( Modified_GPL );

with Ada.Strings.Unbounded;
with Text_Utils;
with Ada.Text_IO;
with General_Chart_Constants;
--
-- FIXME : Generic on Language somehow, then move to tax-benefit-code
--

generic 
   
   type Languages is (<>); 
   
   with function Lookup( key : String; lang : Languages ) return String;
   
   with function Format_One_Cell( 
      complete_table : Table_Type;
      row            : Row_Range; 
      col            : Col_Range; 
      a              : Data_Type; 
      ctype          : Type_Of_Cell;
      cells_as_ints  : Boolean;
      lang           : Languages ) return String;

package Tabulator.Web_IO is
   
   use Text_Utils;
   use Ada.Strings.Unbounded;
   
   function Make_Chart_By_Row(
      tab          : Table_Type;
      title        : String;
      subtitle     : String;
      value_to_use : Cell_Values_Range;
      cell_op      : Cell_Compare_Type;
      x_axis_label : String;
      chart_type   : General_Chart_Constants.Chart_Type;
      chart_size   : General_Chart_Constants.Chart_Size;
      lang         : Languages ) return Unbounded_String;

   function Make_Chart_By_Col(
      tab          : Table_Type;
      title        : String;
      subtitle     : String;
      value_to_use : Cell_Values_Range;
      use_totals   : Boolean;
      row_target   : Row_Range;
      chart_type   : General_Chart_Constants.Chart_Type;
      chart_size   : General_Chart_Constants.Chart_Size;
      lang         : Languages ) return Unbounded_String;
   
   function To_String( 
         complete_table : Table_Type;
         tab            : Table_Expression;
         name           : String;
         description    : String;
         lang           : Languages;
         cells_as_ints  : Boolean := True ) return Unbounded_String;

end Tabulator.Web_IO;
