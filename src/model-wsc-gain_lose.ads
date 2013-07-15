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

with Ada.Containers.Vectors;

with Tabulator;
with Tabulator_Commons;
with T_Utils;
with Model.WSC.Household;
with Model.WSC.Household;
with Model.WSC.Results;
with WSC_Enums;

--
-- Instances of the generic Tabulator for Wider
--
package Model.WSC.Gain_Lose is

   use Model.WSC.Household;
   use Model.WSC.Results;
   use WSC_Enums;
   use Tabulator_Commons;
   
   subtype Scale_Range is Positive range 1 .. 7;
   
   type Gain_Lose_Scale_Type is array( Scale_Range ) of Amount;
   
   Gain_Lose_Scale : constant Gain_Lose_Scale_Type := 
      ( -50.0, -10.0, -1.0, 1.0, 10.0, 50.0, Amount'Last );
      
   --
   -- these aren't really needed for Wider
   --
   type Example is record
      year    : Simulation_Years;
      wave    : Waves;
      hid     : Sernum_Value;
      family  : Benefit_Unit_Count := 0; -- count since could meaningfully be zero
      pno     : Sernum_Value := 0;
   end record;
   
   type Aggregate_Cell_Breakdown is record
      tenure         : Tenure_Array  := ( others=>0.0 ) ;
      marital_status : Marital_status_type_Array := ( others=>0.0 );
      gender         : Gender_type_Array := ( others=>0.0 );
      decile         : Deciles_Array  := ( others=>0.0 ) ;
      Age_Band       : Age_Band_Array  := ( others=>0.0 ) ;
   end record;
   
   type Cell_Breakdown is record
      tenure              : Tenure_Type ;
      marital_status      : Marital_status_type;
      gender              : Gender_type;
      decile              : Decile_Number;
      age_of_head         : Adult_Age_Band;
    end record;
   
   procedure Increment_Cell_Breakdowns( 
      current_breakdown : in out Aggregate_Cell_Breakdown; 
      new_breakdown     : Cell_Breakdown;
      weight            : Amount );

   package Gain_Lose_By_Tenure is new Tabulator(
         Data_Type                => Amount, 
         Example_Type             => Example,
         Row_Range                => Tenure_Type,
         Col_Range                => Scale_Range,
         Cell_Values_Range        => Summary_Items_Type,
         Values_Array             => Summary_Items_Array,
         Max_Examples             => 2,
         Scale_Array              => Gain_Lose_Scale_Type,
         Cell_Breakdown           => Cell_Breakdown,
         Aggregate_Cell_Breakdown => Aggregate_Cell_Breakdown,
         Increment_Cell_Breakdown => Increment_Cell_Breakdowns
   );
   
   package Gain_Lose_By_Decile is new Tabulator(
         Data_Type                => Amount, 
         Example_Type             => Example,
         Row_Range                => Decile_Number,
         Col_Range                => Scale_Range,
         Cell_Values_Range        => Summary_Items_Type,
         Values_Array             => Summary_Items_Array,
         Max_Examples             => 2,
         Scale_Array              => Gain_Lose_Scale_Type,
         Cell_Breakdown           => Cell_Breakdown,
         Aggregate_Cell_Breakdown => Aggregate_Cell_Breakdown,
         Increment_Cell_Breakdown => Increment_Cell_Breakdowns
   );


   package Gain_Lose_By_Age_Band is new Tabulator(
         Data_Type                => Amount, 
         Example_Type             => Example,
         Row_Range                => Adult_Age_Band,
         Col_Range                => Scale_Range,
         Cell_Values_Range        => Summary_Items_Type,
         Values_Array             => Summary_Items_Array,
         Max_Examples             => 2,
         Scale_Array              => Gain_Lose_Scale_Type,
         Cell_Breakdown           => Cell_Breakdown,
         Aggregate_Cell_Breakdown => Aggregate_Cell_Breakdown,
         Increment_Cell_Breakdown => Increment_Cell_Breakdowns
   );

end  Model.WSC.Gain_Lose;
