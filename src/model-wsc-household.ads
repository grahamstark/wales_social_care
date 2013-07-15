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

with Ada.Calendar;
with Ada.Containers.Vectors;

with T_Utils;

with Model.WSC.Regressors;
with WSC_Enums;

--
-- A simple derived BHPS dataset, designed to me small enough to hold a reasonable amount in memory. Patterned after
-- the old IFS/ FRS/FES one.
--
package Model.WSC.Household is

   use Ada.Calendar;
   use WSC_Enums;
   use Model.WSC.Regressors;
   --
   -- FIXME this are copies from the BHPS declarations, and elswhere
   -- we could move these to common definitions somewhere?
   --   
   
   
   WEIGHT_MULTIPLIER : constant Amount := 3016376.67 / 3250.93;
   -- this is Wales popn 2011 from Daffodil, divided by a weighted count of
   -- the population using extended_2 weights, wave R
   
   subtype Households_Per_Wave_Count  is Natural range 0 .. 6_000;
   subtype Households_Per_Wave_Number is Households_Per_Wave_Count range 1 .. Households_Per_Wave_Count'Last;
   type Sernum_Array                  is array( Households_Per_Wave_Number ) of Sernum_Value;
   
   subtype Child_Count         is Natural range 0 .. 10;
   subtype Child_Number        is Child_Count range 1 .. Child_Count'Last;
   
   subtype Adult_Count         is Natural range 0 .. 10;
   subtype Adult_Number        is Adult_Count range 1 .. Adult_Count'Last;
   
   subtype Person_Count        is Natural range 0 .. (Child_Count'Last + Adult_Count'Last);
   subtype Person_Number       is Person_Count range 1 .. Person_Count'Last;

   subtype Benefit_Unit_Count  is Natural range 0 .. 12; -- Person_Count'Last;
   subtype Benefit_Unit_Number is Benefit_Unit_Count range 1 .. Benefit_Unit_Count'Last;
   
   subtype Hours_Count         is Natural range 0 .. (7 * 24);

   type Diff_And_Help is record
      help : Help_Needed_Type := Help_Needed_Type'First;
      diff : Difficulty_Type := Difficulty_Type'First;
   end record;
    
   type Fitness_Array is array( Task_Type ) of Diff_And_Help;

   type Household_Data is record
      interview_date                : Time;
      current_simulated_date        : Time;
      wave                          : Waves;
      hid                           : Sernum_Value;
      origin_hid                    : Sernum_Value;
      tenure                        : Tenure_Type   := Tenure_Type'First;  
      region                        : Region_Type   := Region_Type'First;
      gross_rent                    : Amount := 0.0; -- rentg Gross rent including Housing Benefit  or rent Net amount of last rent payment 
      net_rent                      : Amount := 0.0; -- rent Net amount of last rent payment 
      mortgage_outstanding          : Amount := 0.0;
      gross_housing_costs           : Amount := 0.0;
      net_housing_costs             : Amount := 0.0;
      total_income                  : Amount := 0.0;
      house_value                   : Amount := 0.0;
      other_property_value          : Amount := 0.0;
      mortgage_payment              : Amount := 0.0;
      years_outstanding_on_mortgage : Natural := 0; 
      weights                       : Weights_Array := ( others => 0.0 );
      ct_band                       : Council_Tax_Band := Council_Tax_Band'First; -- hsctax
      has_full_sample               : Boolean := False;
   end record;

   
   type Person is tagged record
      wave                                          : Waves;
      pid                                           : Sernum_Value     := MISSING_SERNUM;
      hid                                           : Sernum_Value     := MISSING_SERNUM;
      pno                                           : Person_Number    := Person_Number'First;
      age                                           : Age_Range        := Age_Range'First;
      sex                                           : Gender_Type      := Gender_Type'First;
      activities_of_daily_living_score              : Amount           := 0.0;
      health_score                                  : Amount           := 0.0;  
      years_in_residential_care                     : Integer          := -1;
      --
      -- FIXME : move regressors out of here and recalculate them as needed
      -- i.e. in the .Regressions package
      regressors                                    : Regressors_Array := ( others => 0.0 );
      respondent_weights                            : Weights_Array    := ( others => 0.0 );
      enumeration_weights                           : Weights_Array    := ( others => 0.0 );
      current_income                                : Incomes_Array    := ( others => 0.0 );
      -- annual_income                                 : Incomes_Array    := ( others => 0.0 );
      hdsp                                          : Head_Or_Spouse   := Neither;
      has_full_sample                               : Boolean          := False;
      receives_informal_care_from_household_member  : Boolean := False;
      receives_informal_care_from_non_householder   : Boolean := False;
      hours_of_care_recieved                        : Hours_Count     := Hours_Count'First; -- TODO NOT MAPPED
      hours_of_care_given                           : Hours_Count     := Hours_Count'First;
      dies_this_period                              : Boolean := False;
      seperates_this_period                         : Boolean := False;
      fitness                                       : Fitness_Array; -- TODO MAPPED??
      employment_status                             : Employment_Status_Type := Employment_Status_Type'First;
      usual_hours_worked_per_week                   : Hours_Count     := Hours_Count'First; -- TODO NOT MAPPED
      highest_qualification                         : Qualification_Type := Qualification_Type'First;
      is_disabled                                   : Boolean := False;
      marital_status                                : Marital_Status_Type := Marital_Status_Type'First;
      partner_status                                : Partner_Status_Type := Partner_Status_Type'First;
      health_status                                 : Health_Status_Type := Health_Status_Type'First;
      personal_wealth                               : Amount := 0.0;
      
      disability_living_allowance_mobility_level    : High_Low_Nil_Type := nil;
      disability_living_allowance_care_level        : High_Middle_Low_Nil_Type := nil;
      attendance_allowance_level                    : High_Low_Nil_Type := nil;
      
   end record;

   function To_String( pers : Person ) return String;
  
   --
   -- This creates a complete set of the regressors actually used in the RHS of 
   -- any of the equations. Compared to HR's setupBHPS.do, it omits any forward looking ones (split, retire, etc.)
   -- which appear only on the LHS and can't meaninfully be computed in a backward looking model.
   -- 
   procedure Recalculate_Regressors( 
      pers             : in out Person; 
      -- pers_last_period : in person; 
      num_adults       : Person_Count;
      num_children     : Person_Count;
      hdata            : Household_Data );
      
   type Adult_Array is array( Adult_Number ) of Person;
   type Child_Array is array( Child_Number ) of Person;
   
   type Benefit_Unit is tagged record
      num_children       : Child_Count  := 0;
      num_adults         : Adult_Count  := 0;
      num_people         : Person_Count := 0;
      adults             : Adult_Array;
      children           : Child_Array;
      position_of_head   : Adult_Count;
      position_of_spouse : Adult_Count;
   end record;
   
   function Is_Couple( bu : Benefit_Unit ) return Boolean;
   
   function Which_Incomes_Received( 
      pers          : Person;  
      use_current   : Boolean ) return Income_Package.Set;   
   
   function Which_Incomes_Received( 
      bu            : Benefit_Unit;
      use_current   : Boolean ) return Income_Package.Set;
   
      
   function To_String( bu : Benefit_Unit ) return String;
   procedure Delete_Adult( 
      bu  : in out Benefit_Unit;
      pno    : Person_Number;
      reason : Delete_Reason_Type );
   
   type Benefit_Unit_Array is array ( Benefit_Unit_Count ) of Benefit_Unit;

   
   function To_String( hdata : Household_Data ) return String;

   type Household is tagged record
      hid                  : Sernum_Value       := MISSING_SERNUM;
      wave                 : Waves              := Waves'First;
      hdata                : Household_Data;      
      num_benefit_units    : Benefit_Unit_Count := Benefit_Unit_Count'First;
      benefit_units        : Benefit_Unit_Array;
   end record;  
   
   type Household_Access is access Household;
   procedure Free( hh : in out Household_Access );
   
   function Weight( hh : Household ) return Amount;

   function Which_Incomes_Received( 
      hh            : Household;  
      use_current   : Boolean ) return Income_Package.Set;

   procedure Delete_Benefit_Unit( 
      hh : in out Household; 
      buno : Benefit_Unit_Number );
   
   procedure Remove_Empty_Benefit_Units( hh : in out Household );
   
   function To_String( hh : Household ) return String;
   
   --
   -- this is a function to make it easier to keep in sync if we kill people off or add them
   --
   function Num_People( hh : Household ) return Person_Count;
   function Num_Children( hh : Household ) return Person_Count;
   function Num_Adults( hh : Household ) return Person_Count;
   function Age_Of_Oldest_Member( hh : Household ) return Age_Range;
   
   procedure Find_Person_By_Pno( 
      hh    : Household; 
      pno   : Person_Count; 
      buno  : out Benefit_Unit_Number; 
      adno  : out Adult_Count;
      chno  : out Child_Count;
      found : out Boolean );

  procedure Find_Person_By_Pid( 
      hh    : Household; 
      pid   : Sernum_Value; 
      buno  : out Benefit_Unit_Number; 
      adno  : out Adult_Count;
      chno  : out Child_Count;
      found : out Boolean );
   
   type Person_By_Wave is array( waves ) of Person;
   type Household_By_Wave is array( waves ) of Household;
   
   package Household_List_Package is new Ada.Containers.Vectors(
      Element_Type => Household,
      Index_Type   => Positive
   );

   subtype Household_List is Household_List_Package.Vector;
   function Get_Age_Band( age : Age_Range ) return Age_Band_Type;

   type Person_Ordering_Record is record
      hid    : Sernum_Value;
      pid    : Sernum_Value;
      pno    : Person_Number;
      weight : Amount;
      v      : Amount;
   end record;
   
   function To_String( po : Person_Ordering_Record ) return String;
   
   package Person_Ordering_Package is new Ada.Containers.Vectors( 
      Element_Type => Person_Ordering_Record, 
      Index_Type => Positive );
   subtype Person_Ordering_List is Person_Ordering_Package.Vector;
   
   procedure Sort_By_V( list : in out Person_Ordering_List );
   function Population_Total( list : Person_Ordering_List ) return Amount;
   
   function Make_New_Sernum( wave : Waves; hhno : Households_Per_Wave_Number; n : Natural := 1 ) return Sernum_Value;
   
end Model.WSC.Household;
