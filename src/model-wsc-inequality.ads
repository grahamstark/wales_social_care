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

with Inequality_Generator;
with Ada.Containers.Vectors;
with T_Utils;
with Model.WSC.Household;
with Model.WSC.Household;
with Model.WSC.Results;
with Model.WSC.Parameters;
with WSC_Enums;

--
-- interfaces to and instances of the Inequality_Generator code for the Mefisto
-- project. Allow ginis and lorenzes to be generated for various subgroups.
-- 
package Model.WSC.Inequality is
   use Model.WSC.Household;
   use Model.WSC.Results;
   use Model.WSC.Parameters;
   use WSC_Enums;

   type Inequality_Record is record
      tenure             : Tenure_Type ;
      decile             : Decile_Number;
      age_of_head        : Adult_Age_Band;
      income             : Amount;
      population         : Amount;
   end record;

   package Inequality_Package is new Ada.Containers.Vectors( 
      Element_Type => Inequality_Record, 
      Index_Type   => Positive );
   subtype Inequality_List is Inequality_Package.Vector;
   
   procedure Print_Inequality_List( filename : String; ineq : Inequality_List );
   
   package Be_Inequality is new Inequality_Generator( Amount=>Amount, Rate=>Rate );

   LORENZ_BIN_SIZE : constant := 25;

   type Lorenz_And_Gini is record
      gini : Rate := 0.0; -- FIXME not needed dup of inequality_measures below 
      lorenz : be_inequality.Quantile_List;
      inequality_measures : be_inequality.Inequality_Array;
   end record;

   type Lorenz_And_Gini_By_Tenure_Array is array( Tenure_Type ) of  Lorenz_And_Gini;
   type Lorenz_And_Gini_By_Decile_Array is array( Decile_Number ) of  Lorenz_And_Gini;
   type Lorenz_And_Gini_By_Age_Band_Array is array( Adult_Age_Band ) of  Lorenz_And_Gini;
   
   type  Lorenz_And_Gini_By_Sys_Array is array( System_Number'Range ) of Lorenz_And_Gini;

   type  Lorenz_And_Gini_By_Tenure_And_Sys_Array is array( System_Number'Range ) of Lorenz_And_Gini_By_Tenure_Array;
   type  Lorenz_And_Gini_By_Decile_And_Sys_Array is array( System_Number'Range ) of Lorenz_And_Gini_By_Decile_Array;
   type  Lorenz_And_Gini_By_Age_Band_And_Sys_Array is array( System_Number'Range ) of Lorenz_And_Gini_By_Age_Band_Array;

end Model.WSC.Inequality;
