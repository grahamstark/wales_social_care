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

with BHPS;
with BHPS_Enums;
with BHPS.Print;
with BHPS.Binary_IO;
with BHPS.XLookup;
with BHPS_Indexes;
with BHPS.State_Changes;

with Model.WSC;
with Model.WSC.Household;
with Model.WSC.BHPS_Data_Creation_Libs;
with Model.WSC.Household;
with Model.WSC.Regressors;
with Model.WSC.Household.Transitions;
with Model.WSC.Household.Database;
with Model.WSC.Global_Settings;
with Model.WSC.Run_Declarations;
with GNATColl.Traces;
with WSC_Enums;

--
-- This is the driver program for bhps model dataset creation.
--
procedure Make_WSC_Data_From_BHPS is

   package wsch renames Model.WSC.Household;
   package wscl renames  Model.WSC.BHPS_Data_Creation_Libs;
   package wscr renames  Model.WSC.Regressors;
   package trans renames Model.WSC.Household.Transitions;
   use Model.WSC.Household.Database;
   use BHPS;
   use Ada.Text_IO;
   use BHPS_Indexes;
   use BHPS_Index_Package;
   use BHPS_Map_Package;
   use BHPS.Binary_IO;
   use BHPS.XLookup;
   use Ada.Assertions;

   hh        : BHPS.Household_Access;
   this_wave : Waves;
   ptrs      : Record_Pointers_Array;
   xptr      : X_Wave_Lookup;
   index_map : BHPS_Index_Array;
   db        : DB_Type;
   default_settings : Model.WSC.Run_Declarations.Run;
   wales_only : Boolean;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MAKE_WSC_DATA_FROM_BHPS" );

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
         -- FIXME Open log config
      else
         Put_Line( "usage: run_settings_file global_settings_file wales_only (True|False )" );
         return False;
      end if;
      return True;
   end Set_Up;


   procedure Create_Model_Household( c : Index_Cursor ) is
   use BHPS_Enums;
      index : Index_Rec := Element( c );
      mhh : wsch.Household;
      num_non_empty_benefit_units : Natural;
      someone_missing : Boolean;
   begin
      hh.all := Load_Household( index, ptrs( this_wave ), this_wave );
      if( hh.hhresp.region2 /= wales ) and wales_only then
         return;
      end if;
      if( hh.hhresp.hid > 0 )then
         wscl.Create_Household( hh, mhh, num_non_empty_benefit_units, someone_missing, this_wave );
         Log( wsch.To_String( mhh ));
      end if;
      if( mhh.hdata.has_full_sample ) and ( num_non_empty_benefit_units > 0 )then
         Log( "starting generating regressors for hh " & hh.hid'Img );
         for b in 1 .. mhh.num_benefit_units loop
            declare
               bu         : BHPS.Benefit_Unit := Get_Benefit_Unit( hh, b );
            begin
               each_bhps_adult:
               for adno in 1 .. bu.Num_With_Adult_Records loop
                  declare
                     ad                   : BHPS.Adult := bu.Get_With_Adult_Records( adno );
                     ad_last_period       : BHPS.Adult;
                     ad_next_period       : BHPS.Adult;
                     regressors           : wscr.Regressors_Array;
                     next_period_wave_loc : Wave_Location;
                  begin
                     if( this_wave > Waves'First )then
                        declare
                           hh_last_period : BHPS.Household_Access := new BHPS.Household;
                           last_wave      : Waves := Waves'Pred( this_wave );
                           last_index_map : BHPS_Index := index_map( last_wave );
                           wave_loc       : Wave_Location := Get_Household_Id( xptr, last_wave, ad.pid );
                           last_index     : Index_Rec ;
                        begin
                           if wave_loc.pno > 0  and wave_loc.hid > 0 and wave_loc.individual_interview_ok and wave_loc.household_interview_ok then
                              last_index := last_index_map.Element( wave_loc.hid );
                              hh_last_period.all := Load_Household( last_index, ptrs( last_wave ), last_wave );
                              ad_last_period := hh_last_period.Get_Person( wave_loc.pno );
                           end if;
                           Free( hh_last_period );
                        end;
                     end if;
                     if( this_wave < Waves_With_Data'Last )then
                        declare
                           hh_next_period  : BHPS.Household_Access := new BHPS.Household;
                           next_wave      : Waves := Waves'Succ( this_wave );
                           next_index_map : BHPS_Index := index_map( next_wave );
                           next_index     : Index_Rec;
                        begin
                           next_period_wave_loc := Get_Household_Id( xptr, next_wave, ad.pid );
                           if next_period_wave_loc.pno > 0 and
                              next_period_wave_loc.hid > 0 and
                              next_period_wave_loc.individual_interview_ok and
                              next_period_wave_loc.household_interview_ok then
                              next_index := next_index_map.Element( next_period_wave_loc.hid );
                              hh_next_period.all := Load_Household( next_index, ptrs( next_wave ), next_wave );
                              Log( "looking for pno " & next_period_wave_loc.pno'Img & " wave " & next_wave & " href " & hh_next_period.hid'Img & " wave_loc.hid " & next_period_wave_loc.hid'Img );
                              ad_next_period := hh_next_period.Get_Person( next_period_wave_loc.pno );
                           end if;
                           Free( hh_next_period );
                        end;
                     end if;
                     regressors := wscl.Make_Individual_Regressors(
                        ad,
                        ad_last_period,
                        ad_next_period,
                        next_period_wave_loc.summary_status,
                        hh.hhresp.region,
                        hh.hhresp.nkids );
                     -- FIXME: not needed??
                     Log( "regressors are " & wscr.Regressors_Package.To_String( regressors ));
                     if( ad.indresp.pid > 0 )then
                        declare
                           buno  : Benefit_Unit_Count;
                           adno  : Adult_Count;
                           chno  : Child_Count;
                           found : Boolean;
                         begin
                            wsch.Find_Person_By_Pid( mhh, WSC_Enums.Sernum_Value( ad.indresp.pid ), buno, adno, chno, found );
                            if( not found )then
                               Log( "nobody located in model for hh " & mhh.hid'Img & " pid " & ad.pid'Img & wsch.To_String( mhh ));
                               Assert( found, " nobody located in model for hh " & mhh.hid'Img & " pid " & ad.pid'Img );
                           end if;
                            if( adno > 0 )then
                               mhh.benefit_units( buno ).adults( adno ).regressors := regressors;
                            elsif( chno > 0 )then
                               mhh.benefit_units( buno ).children( chno ).regressors := regressors;
                            end if;
                         end;
                     end if;
                  end;
               end loop each_bhps_adult;
               each_bhps_child:
               for chno in 1 .. bu.Num_Children loop
                  declare
                     ad :BHPS.Adult := bu.Get_Child( chno );
                  begin
                     null;
                  end;
               end loop each_bhps_child;
            end;
         end loop;
         db.Write_Household( mhh );
      else
         Log( "hh data missing for " & mhh.hid'Img & " wave " & this_wave & " num_non_empty_benefit_units = " & num_non_empty_benefit_units'Img );
      end if;
   end Create_Model_Household;


   nhhs : Positive := 1;
begin
   if( not Set_Up )then
       Ada.Command_Line.Set_Exit_Status( 1 );
   else
      if( wales_only )then
         db.Create( "data/wales/actual/", actual, 1 );
      else
         db.Create( "data/full/actual/", actual, 1 );
      end if;
      hh := new BHPS.Household;
      Open( ptrs );
      xptr := Load_XWave;
      Restore_All_Indexes( "/mnt/data/bhps/bin/", index_map );
      for wave in  'b' .. Waves_With_Data'Last loop
         this_wave := wave;
         Log( "on wave " & wave );
         index_map( wave ).Iterate( Create_Model_Household'Access );
      end loop;
      Close( ptrs );
      Free( hh );

      for wave in WSC_Enums.g .. WSC_Enums.r loop
         Log( "Create_Initial_Care_Home_Population for wave " & wave'Img );
         trans.Create_Initial_Care_Home_Population( 
            DB          => db, 
            WSC_Run     => default_settings, 
            Wave        => wave,
            Auto_Update => True );
      end loop;

      db.Close;
   end if;
end Make_WSC_Data_From_BHPS;
