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
with T_Utils;
with Costs_Tabulator;
with Model.WSC.Household;
with Model.WSC.Household;
with Model.WSC.Results;
with Model.WSC.Parameters;
with WSC_Enums;

package Model.WSC.Budget is
   
   use Model.WSC.Household;
   use Model.WSC.Results;
   use Model.WSC.Parameters;
   use WSC_Enums;

   package Household_Costs_By_Tenure is new Costs_Tabulator(
      Data_Type => Amount,
      Breakdown_Range => Tenure_Type,
      Values_Range => Costs_Type,
      Values_Array => Costs_Array
   );
   package Household_Costs_By_Decile is new Costs_Tabulator(
      Data_Type => Amount,
      Breakdown_Range => Decile_Number,
      Values_Range => Costs_Type,
      Values_Array => Costs_Array
   );
   package Household_Costs_By_Age_Band is new Costs_Tabulator(
      Data_Type => Amount,
      Breakdown_Range => Adult_Age_Band,
      Values_Range => Costs_Type,
      Values_Array => Costs_Array
   );
   
   type Household_Costs_By_Tenure_Array is array( System_Number'Range ) of Household_Costs_By_Tenure.Table_Type;
   type Household_Costs_By_Decile_Array is array( System_Number'Range ) of Household_Costs_By_Decile.Table_Type;
   type Household_Costs_By_Age_Band_Array is array( System_Number'Range ) of Household_Costs_By_Age_Band.Table_Type;

end Model.WSC.Budget;
