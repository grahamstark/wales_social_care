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

with Model.WSC.Run_Settings;
with Model.WSC.Run_Declarations;
with Maths_Functions;
with Model.WSC.Uprate;
with Model.WSC.Household.Database;
with Ada.Containers.Ordered_Maps;
--
-- This package takes a household in this period and the previous one, and ages it one year
-- Apply_Regressions runs death and divorce probits, and ols regressions for health and disability status
-- Split_Up_Household takes an aged and regressed household and returns a vector (possibly 0 long) of new households 
-- for the next period.
--
package Model.WSC.Household.Transitions_Basic is
   
   --
   -- FIXME this has been hacked to compress the bottom 5 age ranges into one
   -- and is generally a mess. Needs to be mergesd with Model.WSC.Household.Weights and 
   -- then deleted.
   --
   
   WEIGHT_MULTIPLIER : constant Amount := 3016376.67 / 3250.93;
      -- this is Wales popn 2011 from Daffodil, divided by a weighted count of
      -- the population using extended_2 weights
   
   use Model.WSC.Run_Declarations;  

   package Weights_Map_Package is new Ada.Containers.Ordered_Maps(
      Key_Type => Sernum_Value,
      Element_Type => Amount,
      "<" => "<",
      "=" => "=" );
   subtype Weights_Map is Weights_Map_Package.Map;
    
   procedure Age_And_Uprate( 
      hh : in out Household; 
      wsc_run : Run;
      wave     : Waves;
      uprate_only : Boolean );
      
   procedure Create_Weights(
      wsc_run             : Run;
      db                  : in out Model.WSC.Household.Database.DB_Type;
      wave                : Waves;
      target_populations  : Vector;
      iterations          : out Positive;
      error               : out Eval_Error_Type;
      weights             : out Weights_Map );

   procedure Create_Weights_File(
      weights_file_name : String;
      from_wave         : Waves;
      wsc_run : Run );
   
   procedure Create_Initial_Care_Home_Population(       
      db      : in out Model.WSC.Household.Database.DB_Type;
      wsc_run : Run;   
      wave    : Waves );
      
   procedure Create_Simulation_Data(
      in_db_name     : String;
      out_db_name    : String;
      wsc_run        : Run; 
      monitor        : Model.WSC.Run_Settings.Model_Monitor'Class );

   subtype R_Weights_Range is Positive range 1 .. 1434;
   subtype R_Weights_Vector is Vector( R_Weights_Range );
   
   -- subtype Calmar_Targets_Range is Positive range 1 .. 39;
   subtype Calmar_Targets_Range is Positive range 1 .. 39;
   subtype Populations_Range is Calmar_Targets_Range range 1 .. 30;
   subtype Short_Populations_Range is Calmar_Targets_Range range 1 .. 22;
   subtype Calmar_Targets_Vector is Vector( Calmar_Targets_Range );
   subtype Compressed_Targets_Range is Positive range 1 .. 31;   
   subtype Compressed_Targets_Vector is Vector( Compressed_Targets_Range );
   
   subtype Full_Weights_Vector is Vector( Calmar_Targets_Range );
   
   type All_R_Weights_Array is array( Simulation_Waves ) of R_Weights_Vector;
   
   procedure Load_R_Weights( filename : String; weights : in out All_R_Weights_Array ); 

   function Get_Uprate_Amount( 
      interview_date : Time; 
      wave           : Waves;
      which          : Uprate_Targets; 
      wsc_run : Run ) return Rate;

   function Growth_Between( base_wave, wave1 : Waves; v1, v2 : Amount ) return Amount;   
   
   function Get_Targets_For_Wave( wave : Simulation_Waves ) return Calmar_Targets_Vector;

   function Get_Death_Probability( wave : Simulation_Waves; pers : Person ) return Probability;


private       
   
   function Get_Targets_From_Person( hh : Household; pers : Person ) return Calmar_Targets_Vector;

   -- zero if no reported problem; otherwse 1 .. 5 for 65-69 .. 85+ age band
   -- see Daffodil http://www.daffodilcymru.org.uk/index.php?pageNo=1062&PHPSESSID=05nhl3dd73jprvqufe12t80lt5&at=a&sc=1&loc=1&np=1
   function Unable_To_Manage_Some_Activity( ad : Person ) return Natural;

end Model.WSC.Household.Transitions_Basic;
