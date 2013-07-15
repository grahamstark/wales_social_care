--
-- copyright(c) 2011 Graham Stark/ Virtual Worlds (graham.stark@virtual-worlds.biz)/ Howard Reed, Landman Economics (howard@landman-economics.co.uk)
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

with Ada.Text_IO;
with Ada.Containers.Ordered_Maps;

with Maths_Functions;

with Model.Run_Settings;
with Model.WSC.Run_Declarations;
with Model.WSC.Household.Database;


package Model.WSC.Household.Weights is

   use Model.WSC.Run_Declarations;   
  
   type Weighter is tagged private;

   function Get_Weight( w : Weighter; hno : Sernum_Value; wave : Waves ) return Amount;
 
   procedure Create(
      w              : in out Weighter;
      db             : in out Model.WSC.Household.Database.DB_Type;
      wsc_run        : Run; 
      monitor        : in out Model.Run_Settings.Model_Monitor'Class;
      iterations     : out Positive;
      error          : out Eval_Error_Type );

private
   
   package Sernum_Weight_Package is new Ada.Containers.Ordered_Maps(
      Key_Type   => Sernum_Value,
      Element_Type => Amount,
      "<" => "<",
      "=" => "=" );      

   subtype Weights_Map is Sernum_Weight_Package.Map;
      
   type Weights_Map_Array is array( Waves ) of Weights_Map;
   
   type Weighter is tagged record
      use_only_base_weight : Boolean := True;
      weights : Weights_Map_Array;
   end record;

end Model.WSC.Household.Weights;
