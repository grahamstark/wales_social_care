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

-- 
-- This holds uprating data from 1990 to 2020.
--
-- Based on an OBR forecast of [DATE] in ../../forecasts/obr/ , plus later wage data
-- see uprating_data_driver.rb for details of how this was put together
--
with Ada.Calendar;
with WSC_Enums;


package Model.WSC.Uprate is

   use Ada.Calendar;
   use WSC_Enums;
   
   subtype Forecast_Year is Year_Number range 1990 .. 2031;
   
   function Get_Level( 
      which : Forecast_Element; 
      y     : Forecast_Year; 
      m     : Month_Number ) return Amount;
      
   function Get_Ratio_Between( 
      which   : Forecast_Element; 
      start_y : Forecast_Year; 
      start_m : Month_Number; 
      end_y   : Forecast_Year; 
      end_m   : Month_Number ) return Amount;
      
   function Get_Ratio_Between_WHP( 
      start_y : Forecast_Year; 
      start_m : Month_Number; 
      end_y   : Forecast_Year; 
      end_m : Month_Number ) return Amount;
      
   function To_String( which : Forecast_Element ) return String;
      
end Model.WSC.Uprate;
