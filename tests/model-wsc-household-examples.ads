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
--
-- This child package simply returns a pre-loaded test household for the unit tests.
--

pragma License( Modified_GPL );

with WSC_Enums;

package Model.WSC.Household.Examples is

   use WSC_Enums;
   
   type Example_Type is (
      single_retired_person,
      couple_bu_retired,
      young_single,
      old_sick_single_male,
      cpag_terry_and_julie, -- 2012 edn p 478
      cpag_angelina_and_michael,
      age_uk_indira,
      age_uk_sarah,
      zero_income );
   
   function Make_Retired_Adult( 
      hh        : Household;
      age       : Age_Range; 
      sex       : Gender_Type; 
      pid       : Sernum_Value; 
      is_couple : Boolean; 
      health    : Health_Status_Type ) return Person;
      
   function Get_Household( which : Example_Type ) return Household;

end Model.WSC.Household.Examples;
