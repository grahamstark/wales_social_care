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

with BHPS;
with BHPS_Enums;
with BHPS.XLookup;
with BHPS_Indexes;

with Model.WSC.Household;
with Model.WSC.Regressors;
with WSC_Enums;
--
-- This contains code to create people and households for the model dataset from raw BHPS data.
--
package Model.WSC.BHPS_Data_Creation_Libs is

   use BHPS;
   use BHPS_Enums;
   use BHPS_Indexes;
   use BHPS.XLookup;
   use Model.WSC.Regressors;

   package wscm renames Model.WSC.Household;
   
   function Make_Activities_Of_Daily_Living_Score( adl_x_d : Adlad_Type; adl_x : Adla_Type ) return Amount;
   
   function Make_Individual_Regressors(       
      ad                         : BHPS.Adult; 
      ad_last_period             : BHPS.Adult; 
      ad_next_period             : BHPS.Adult;
      summary_status_next_period : Summary_Status_Type;
      region                     : BHPS_Enums.Region_Type;
      nkids                      : Integer ) return Regressors_Array;
   
   procedure Set_Is_Cared_For( hh : BHPS.Household; mhh : in out wscm.Household );
   
   procedure Set_Partner_Just_Died( hh : BHPS.Household; mhh : in out wscm.Household );
   
   procedure Create_Household( 
      hh                          : BHPS.Household_Access; 
      mhh                         : in out wscm.Household; 
      num_non_empty_benefit_units : out Natural;
      someone_missing             : out Boolean;      
      wave                        : Waves );
   
   function Failed_HH_Interview( hresp : Hhresp_Rec; wave : Waves ) return Boolean;
   
   function Wave_From_Char( c : Character ) return WSC_Enums.Waves;
   
end Model.WSC.BHPS_Data_Creation_Libs;
