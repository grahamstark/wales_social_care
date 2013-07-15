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
with Ada.Assertions;
with Ada.Command_Line;

with GNATColl.Traces;

with Model.WSC;
with Model.WSC.Household;
with Model.WSC.Household;
with Model.WSC.Regressors;
with Model.WSC.Household.Database;
with Model.WSC.Global_Settings;
with Model.WSC.Run_Declarations;

with WSC_Enums;
with Model.WSC.BHPS_Data_Creation_Libs;
with Base_Model_Types;
--
-- This is the driver program for bhps model dataset creation.
--
procedure Basic_Stats_Generator is

   package wsch renames Model.WSC.Household;
   package wscr renames  Model.WSC.Regressors;
   
   use Ada.Assertions;
   use Ada.Text_IO;
   use WSC_Enums;
   use Model.WSC.Household.Database;
   use Ada.Assertions;
   use Model.WSC.Household;
   use Base_Model_Types;
   
   package wscm renames Model.WSC.Household;
   
   this_wave : Waves;
   db        : DB_Type;
   default_settings : Model.WSC.Run_Declarations.Run;
   wales_only : Boolean;
   num_comparisons : Natural := 0;
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.DYNAMIC_DRIVER.WEB_RUNNER" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
   function Set_Up return Boolean is
   begin
      if( Ada.Command_Line.Argument_Count = 3 ) then
         default_settings := Model.WSC.Run_Declarations.Read_Settings( Ada.Command_Line.Argument( 1 ));
         Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 2 ));
         wales_only := Boolean'Value( Ada.Command_Line.Argument( 3 ));
         Model.WSC.Global_Settings.Initialise_Logging;
      else
         Put_Line( "usage: run_settings_file global_settings_file process_all (True|False )" );
         return False;
      end if;
      return True;
   end Set_Up;

   sernums        : Sernum_List;
   hh             : Household;
   num_households : Positive := 1;
   iteration : WSC_Enums.Iteration_Number := 1;
   res_pop : Amount := 0.0;
   pop     : Amount := 0.0;
begin
   if( not Set_Up )then
       Ada.Command_Line.Set_Exit_Status( 1 );
   else
      Open( db, Model.WSC.Global_Settings.Physical_Root & "data/wales/actual/",  actual, 1 );
      for wave in  q .. Waves_With_Data'Last loop
         this_wave := wave;
         pop := 0.0;
         res_pop := 0.0;
         db.Get_Household_Sernums( wave, sernums );
         num_households := Natural( sernums.Length );
         households:
         for hhno in 1 .. num_households loop
            Put_Line( "on household " & hhno'Img );
            hh := db.Get_Household( 
               wave => wave, 
               hid  => sernums.Element( hhno ));
            if hh.benefit_units( 1 ).adults( 1 ).years_in_residential_care > 0 then
               res_pop := res_pop + ( hh.weight );
            else
               pop := pop + ( Amount( hh.num_people ) * hh.weight );
            end if;
         end loop households;
         Put_Line( "on wave " & Waves'Image( wave ));
         Put_Line( "residential pop " & res_pop'Img );
         Put_Line( "pop " & pop'Img );
      end loop;
      db.Close;
   end if;
end Basic_Stats_Generator;
