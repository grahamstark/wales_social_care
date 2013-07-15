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
-- This holds wsc_run for a run that can be changed on a per-run basis, plus records
-- for reporting on health/status of a model run (not implemented yet).
--

pragma License( Modified_GPL );

with WSC_Enums;
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Text_Utils;
with Weighting_Commons;
with Ada.Containers.Vectors;
with Keyed_Text_Buffer;

package Model.WSC.Run_Declarations is

   use WSC_Enums;
   use Ada.Strings.Unbounded;
   use Ada.Calendar;
   use Text_Utils;
   use Weighting_Commons;
   use Keyed_Text_Buffer;
    
   type Uprate_Assumption is record
     percent_change : Rate;
     use_obr        : Boolean;
     element        : Forecast_Element;
   end record;
   
   NULL_UPRATE_ASSUMPTION : constant Uprate_Assumption := (
     percent_change => 0.0,
     use_obr        => False,
     element        => Forecast_Element'First
   );
   
   type Uprate_Assumption_Array is array( Uprate_Targets ) of Uprate_Assumption;
   
   type Run_Status_Type is ( neither, edited, running_or_queued, displayed );
   
   type Run_Type is ( simulation, data_creation );
   
   --
   -- record modelling dataset : 
   --
   type Dataset is record
      Name    : Unbounded_String := Null_Unbounded_String;
      Creator : Unbounded_String := Null_Unbounded_String;
      Title   : Unbounded_String := Null_Unbounded_String;
      Run_Id  : Natural := Natural'First;
   end record;
   --
   -- container for dataset : 
   --
   package Dataset_List is new Ada.Containers.Vectors
      (Element_Type => Dataset,
      Index_Type => Positive );
   --
   -- default value for dataset : 
   --
   Null_Dataset : constant Dataset := (
      Name    => Null_Unbounded_String,
      Creator => Null_Unbounded_String,
      Title   => Null_Unbounded_String,
      Run_Id  => Natural'First
   );
   
   type Run is tagged record

      run_id                : Natural; 
      username              : Unbounded_String := Null_Unbounded_String;
      
      title                 : Unbounded_String := Null_Unbounded_String;
      use_random_threshold  : Boolean := False;
      num_iterations        : Iteration_Number := Iteration_Number'First;
      uprate_assumptions    : Uprate_Assumption_Array;
      interest_rate_pct     : Rate := 0.0;
      real_terms            : Boolean;
      is_null_settings      : Boolean := False; -- for AWS session storage
      
      comparison_username   : Unbounded_String := Null_Unbounded_String;
      comparison_run_id     : Natural := 0;
      
      working_root          : Unbounded_String := Null_Unbounded_String;
      
      users_directory       : Unbounded_String := Null_Unbounded_String;
      output_directory      : Unbounded_String := TuS( "output" );
      dir_separator         : Unbounded_String := TuS( "/" );
      session_id            : Unbounded_String := Null_Unbounded_String;
      
      dataset_name          : Unbounded_String := TuS( "default" );
      default_run_dir_id    : Natural := 0;
      
      start_year            : Simulation_Years;
      end_year              : Simulation_Years;
      do_reweighting        : Boolean := False;
      weighting_function    : Distance_Function_Type;
      weighting_lower_bound : Rate := 0.0;
      weighting_upper_bound : Rate := 0.0;
      probit_thresholds     : Probit_Thresholds_Array := ( others => 0.50 );

      status                : Run_Status_Type := neither; 
      type_of_run           : Run_Type := simulation;
      
   end record;
   
   function Now_As_String return String;
   
   function Qualified_Users_Directory( this : in Run ) return String; 
   function Qualified_Output_Directory( this : in Run; iteration : Positive ) return String;
   
   --
   -- use 0 for iteration if you don't want '/'[iteration_number] appended
   --
   function Qualified_Run_Directory( this : in Run; iteration : Natural ) return String;
   
   function To_String( wsc_run : Run ) return String;

   function Make_Title( wsc_run : Run; as_html : Boolean := True ) return Unbounded_String;

   
   procedure Write_Settings( filename : String; wsc_run : Run );
   function Read_Settings( filename : String ) return Run;
   function To_Text_Buffer( wsc_run : Run ) return Text_Buffer;
   procedure Update_From_Text_Buffer( wsc_run : in out Run; buff : Text_Buffer );
   --
   -- returns first user directory (may be > 1 if > 1 iteration).
   --
   function Create_Directories_For_Run( r : Run ) return Unbounded_String;
   procedure Remove_Directories_For_Run( r : Run );
   
   package Run_List is new Ada.Containers.Vectors
      ( Element_Type => Run,
        Index_Type => Positive );

   NULL_RUN : constant Run := (
      run_id                => Positive'First, -- Unbounded_String =>= Null_Unbounded_String,
      username              => Null_Unbounded_String,
      title                 => Null_Unbounded_String,
      use_random_threshold  => False,
      num_iterations        => Iteration_Number'First,
      uprate_assumptions    => ( others => Null_Uprate_Assumption ),
      interest_rate_pct     => 0.0,
      real_terms            => False,
      is_null_settings      => True, -- for AWS session storage
      comparison_username   => Null_Unbounded_String,
      comparison_run_id     => Natural'First,
      
      working_root          => Null_Unbounded_String,
      
      users_directory       => Null_Unbounded_String,
      output_directory      => Null_Unbounded_String,
      dir_separator         => Null_Unbounded_String,
      session_id            => Null_Unbounded_String,
      
      dataset_name          => Null_Unbounded_String,
      default_run_dir_id    => 0,
      
      start_year            => Simulation_Years'First,
      end_year              => Simulation_Years'First,
      weighting_function    => Distance_Function_Type'First,
      weighting_lower_bound => 0.0,
      weighting_upper_bound => 0.0,
      probit_thresholds     => ( others => 0.00 ),
      status                => neither,
      type_of_run           => simulation,
      do_reweighting        => False
   );

   
end Model.WSC.Run_Declarations;
