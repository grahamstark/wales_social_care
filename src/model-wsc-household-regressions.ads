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

with Model.WSC.Regressors;

--
-- This package contains implementations of Howard's preferred regressions
-- The outputs are simply vector products of the regressors times the cofficients;
-- the probit score is not converted into a boolean here. 
-- Standard regressions return a vector-product/standard deviation pair.
-- 
package Model.WSC.Household.Regressions is
   
   type Regression_Results is record
      vp : Amount := 0.0;
      sd : Amount := 0.0;
   end record;
 
   function Hours_Of_Care_Regression( 
      ad : Person; 
      region : Region_Type ) return Regression_Results;

   function Health_Better_Probit( 
      ad : Person; 
      region : Region_Type ) return Amount;

   function Health_Worse_Probit( 
      ad : Person; 
      region : Region_Type ) return Amount;
      
   function Informal_Care_From_Non_Householder_Probit( 
      ad : Person; ad_last_period : Person; 
      wave : Waves ) return Amount;
      
   function Informal_Care_From_Householder_Probit( 
      ad : Person; 
      ad_last_period : Person; 
      region : Region_Type; 
      use_lagged_dep_vars : Boolean ) return Amount;  
      
   function ADL_Base_Probit( 
      ad : Person; 
      region : Region_Type; 
      which : Task_Type ) return Amount;
      
   function Get_ADL_Change_Probit( 
      ad : Person; 
      which : Task_Type; 
      direction : Change_Direction_Type; 
      region : Region_Type ) return Amount; 
      
   function Dies_This_Period_Probit( 
      ad : Person; 
      region : Region_Type ) return Amount;
      
   function HH_Split_Probit( 
      ad : Person; 
      region : Region_Type; 
      wave : Waves ) return Amount;
      
   function Working_Probit( 
      ad : Person; 
      region : Region_Type; 
      wave : Waves ) return Amount;
      
   function Rent_To_Own_Probit( 
      ad : Person; 
      region : Region_Type;
      wave : Waves ) return Amount;
      
   function Own_To_Rent_Probit( 
      ad : Person; 
      region : Region_Type; 
      wave : Waves ) return Amount;
      
   function Retire_Probit( 
      ad : Person; 
      region : Region_Type; 
      wave : Waves ) return Amount;
      
   function Reenter_Work_Probit( 
      ad : Person; 
      region : Region_Type; 
      wave : Waves ) return Amount;
      
   function Recieving_Any_Care_Probit( 
      ad                    : Person; 
      receiving_last_period : Boolean;
      use_lags              : Boolean ) return Amount;
      
   function Has_Wealth_Probit( 
      ad : Person ) return Amount;

   function Log_Wealth_Regression( 
      ad : Person; 
      region : Region_Type ) return Regression_Results;
      
   function Recieving_Care_Probit( 
      ad  : Person; 
      ct  : Care_Type ) return Amount;

   function To_Care_Probit( ad : Person ) return Amount;
      
   --
   --  > probit aa $agepoly $health $adlall $tenure $regdums if age>=65 & sex==1;
   --  
   function Receives_AA_Probit( 
      ad                  : Person; 
      receipt_last_period : Amount ) return Amount;
   function Receives_DLA_Probit( 
      ad : Person; 
      receipt_last_period : Amount ) return Amount;

   function Private_Care_Demand_Probit( ad : Person ) return Amount;

end Model.WSC.Household.Regressions;
