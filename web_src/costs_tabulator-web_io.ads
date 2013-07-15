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
--
-- FIXME : Generic on Language somehow.
--

with Ada.Strings.Unbounded;
with Text_Utils;
with Ada.Text_IO;
with Model.WSC.Formatting;
with General_Chart_Constants;
with WSC_Enums;

-- FIXME MAKE GENERIC ON LANGUAGES

generic

package Costs_Tabulator.Web_IO is
   
   use Text_Utils;
   use Ada.Strings.Unbounded;
   use Model.WSC.Formatting;
   use WSC_Enums;
   
   function Make_Chart(
      pre_tab      : Table_Type;
      post_tab     : Table_Type;
      title        : String;
      subtitle     : String;
      print_counts : Boolean;
      col          : Values_Range;
      x_axis_label : String;
      chart_type   : General_Chart_Constants.Chart_Type;
      size         : General_Chart_Constants.Chart_Size;
      lang         : Languages ) return Unbounded_String;
   
   function To_String( 
         tab         : Table_Type;
         name        : String;
         description : String; 
         lang        : Languages;
         print_counts : Boolean;
         cells_as_ints : Boolean := False ) return Unbounded_String;


end Costs_Tabulator.Web_IO;
