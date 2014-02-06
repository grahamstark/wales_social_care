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

with Model.WSC.Run_Settings;
with Model.WSC.Run_Declarations;
with Maths_Functions;
with Model.WSC.Uprate;
with Model.WSC.Household.Database;
with Transition_Events;

--
-- This package takes a household in this period and the previous one, and ages it one year
-- Apply_Regressions runs death and divorce probits, and ols regressions for health and disability status
-- Split_Up_Household takes an aged and regressed household and returns a vector (possibly 0 long) of new households 
-- for the next period.
--
package Model.WSC.Household.Transitions is

   use Ada.Text_IO;
   
   use Model.WSC.Run_Declarations;
   package ev_count renames Transition_Events.Transition_Events_Counter;

   procedure Create_Simulation_Data( 
      wsc_run           : Run; 
      monitor           : in out Model.WSC.Run_Settings.Model_Monitor'Class;
      do_reweighting    : Boolean;
      include_care_home : Boolean;
      event_count       : in out ev_count.Recorder;
      iteration         : Iteration_Number;
      uprate_type       : Type_Of_Uprating );
      
   procedure Create_Simulation_Data( 
      wsc_run           : Run; 
      monitor           : in out Model.WSC.Run_Settings.Model_Monitor'Class;
      uprate_type       : Type_Of_Uprating );
      
   
   -- procedure Age_Household( 
      -- hh             : Household; 
      -- wsc_run : Run;
      -- new_households : out Household_List;
      -- event_count    : in out ev_count.Recorder );
      
   procedure Create_Initial_Care_Home_Population(       
      db          : in out Model.WSC.Household.Database.DB_Type;
      wsc_run     : Run;   
      wave        : Waves;
      auto_update : Boolean );
      
   type Wealth_Random_Type is ( has_wealth, sig_wealth_male, sig_wealth_female );
   type Wealth_Random_Array is array( Wealth_Random_Type ) of Rate;
   
   function Date_From_Wave(
      wave      : Waves;
      base_date : Time ) return Time;
     
   --
   --
   procedure Infer_Wealth( 
      hh           : in out Household;
      wsc_run      : Run;
      event_count  : in out ev_count.Recorder;
      capital_rand : Wealth_Random_Array );    

   function Get_Uprate_Amount( 
      old_date  : Time;
      new_date  : Time; 
      which     : Uprate_Targets; 
      wsc_run   : Run ) return Rate;
      
   procedure Uprate( 
      hh          : in out Household; 
      wsc_run     : Run;
      uprate_type : Type_Of_Uprating );

   procedure Age( 
      hh             : in out Household; 
      wsc_run : Run;
      event_count    : in out ev_count.Recorder );


private -- unit testing only

   procedure Impute_Care_Amounts( 
      hh             : in out Household; 
      hh_last_period : Household;
      wsc_run : Run;
      event_count    : in out ev_count.Recorder );
   
   procedure Kill_Household_Members( 
      hh             : in out Household; 
      wsc_run : Run;
      event_count    : in out ev_count.Recorder );
      
     procedure Split_Up_Household( 
      hh             : Household; 
      wsc_run : Run;
      new_households : out Household_List;
      event_count    : in out ev_count.Recorder );
      
   procedure Predict_Health_Needs_And_Employment_Status( 
      hh          : in out Household;
      wsc_run : Run;
      event_count : in out ev_count.Recorder  );
      
   
   -- zero if no reported problem; otherwse 1 .. 5 for 65-69 .. 85+ age band
   -- see Daffodil http://www.daffodilcymru.org.uk/index.php?pageNo=1062&PHPSESSID=05nhl3dd73jprvqufe12t80lt5&at=a&sc=1&loc=1&np=1
   function Unable_To_Manage_Some_Activity( ad : Person ) return Natural;

end Model.WSC.Household.Transitions;
