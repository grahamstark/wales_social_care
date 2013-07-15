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

with BHPS;
with BHPS_Enums;
with BHPS.Print;
with BHPS.Binary_IO;
with BHPS.XLookup;
with BHPS_Indexes;
with BHPS.State_Changes;

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
procedure Compare_Raw_And_Derived is

   package wsch renames Model.WSC.Household;
   package wscr renames  Model.WSC.Regressors;
   
   use Model.WSC.Household.Database;
   use BHPS;
   use Ada.Text_IO;
   use BHPS_Indexes;
   use BHPS_Index_Package;
   use BHPS_Map_Package;
   use BHPS.Binary_IO;
   use BHPS.XLookup;
   use Ada.Assertions;
   use type Base_Model_Types.Big_Integer;
   
   package wscm renames Model.WSC.Household;
   
   hh        : BHPS.Household_Access;
   this_wave : Waves;
   ptrs      : Record_Pointers_Array;
   xptr      : X_Wave_Lookup;
   index_map : BHPS_Index_Array;
   db        : DB_Type;
   default_settings : Model.WSC.Run_Declarations.Run;
   wales_only : Boolean;
   num_comparisons : Natural := 0;

    
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "COMPARE_RAW_AND_DERIVED" );
   
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
   
   procedure Compare_Incomes( ad : BHPS.Adult; mad : in out wscm.Person ) is
      use WSC_Enums;
      use BHPS_Enums;
      use Base_Model_Types;
      use Model.UK_Format_Utils;
      raw_pay : constant Amount := Amount( ad.indresp.paygyr );
   begin
      if( raw_pay > 0.0 )then
         -- Log( "annual gw " & Format( mad.annual_income( WSC_Enums.gross_wage )));
         Assert( mad.current_income( WSC_Enums.gross_wage ) = raw_pay, 
            " pay mismatch orig: " & Format( raw_pay ) &
            " derived: " & Format( mad.current_income( WSC_Enums.gross_wage )));
      end if;
   end Compare_Incomes;


   procedure Compare_People( ad : BHPS.Adult; mad : in out wscm.Person ) is
      use WSC_Enums;
      use BHPS_Enums;
   begin
      if( ad.indall.hgsex = male and mad.sex /= male )then
         Assert( False, "sex mismatch male/female ad.sex = " & ad.sex'Img & " mad " & mad.sex'Img );
      elsif( ad.indall.hgsex = female and mad.sex /= female )then
         Assert( False, "sex mismatch female/male ad.sex = " & ad.sex'Img & " mad " & mad.sex'Img );
      elsif( ad.indall.hgsex /= male and ad.indall.hgsex /= female )then
         Put_Line( "no  sex person " );
      end if;
      Assert( ad.age = mad.age, "age mismatch head" );
   end Compare_People;
   
   procedure Compare_To_Model_Data( c : Index_Cursor ) is 
   use BHPS_Enums;
   use WSC_Enums;
      index : Index_Rec := Element( c );
      mhh : wsch.Household;
      mwave : WSC_Enums.Waves := Model.WSC.BHPS_Data_Creation_Libs.Wave_From_Char( this_wave );
   begin
      hh.all := Load_Household( index, ptrs( this_wave ), this_wave );
      if( hh.hhresp.region2 /= wales ) and wales_only then
         return;
      end if;
      Put_Line( "on hhld " & hh.hid'Img & " wave " & this_wave & " mwave " & mwave'Img );
      
      if db.Household_Exists( 
            wave => mwave, 
            hid  => WSC_Enums.Sernum_Value( hh.hid ))then
         mhh := db.Get_Household( 
               wave => mwave, 
               hid  => WSC_Enums.Sernum_Value( hh.hid ));
         Assert( WSC_Enums.Sernum_Value( hh.hid ) = mhh.hid, " hids don't match raw=" & 
            WSC_Enums.Sernum_Value( hh.hid )'Img & " vs model " & mhh.hid'Img );
         num_comparisons := num_comparisons + 1;
         Assert( mhh.num_benefit_units = hh.num_benefit_units, "mismatch in bu counts " & 
            mhh.num_benefit_units'Img & " vs " 
            & hh.num_benefit_units'Img );
         for buno in 1 .. mhh.num_benefit_units loop
            
            declare
               bu         : BHPS.Benefit_Unit := Get_Benefit_Unit( hh, buno );
               mbu        : wscm.Benefit_Unit := mhh.benefit_units( buno );
            begin
               Put_Line( "BU NUMBER " & buno'Img );
               Assert( bu.num_people = 
                       mbu.num_people,
                       " bu people count mismatch " );
               Assert( bu.num_with_adult_records = 
                       mbu.num_adults,
                       " bu adult count mismatch " );
               Put_line( " mbu.position_of_head " & mbu.position_of_head'Img );
               Put_line( " mbu.position_of_spouse " & mbu.position_of_spouse'Img );
               for adno in 1 .. mhh.benefit_units( buno ).num_adults loop
                  declare
                     ad    : BHPS.Adult := bu.Get_With_Adult_Records( adno );
                     mad   : wscm.Person := mbu.adults( adno );
                  begin
                     Put_Line( wscm.To_String( mad ));
                     -- Put_Line( BHPS.Print.To_String( ad.indresp, 0, 0 ));
                     Put_Line( BHPS.Print.To_String( ad.indall, 1, 0 ));
                     Compare_People( ad, mad );
                     Compare_Incomes( ad, mad ); 
                  end;
               end loop; -- adults
             end;
         end loop; -- bus
      else
         Put_Line( "hhld wave=|" & this_wave & "| mwave | " & mwave'Img & 
            " hh.hid " & hh.hid'Img & " not found " );
      end if;
   end Compare_To_Model_Data;
   
   
   nhhs : Positive := 1;
   iteration : WSC_Enums.Iteration_Number := 1;
begin
   if( not Set_Up )then
       Ada.Command_Line.Set_Exit_Status (1);
   else
      Open( db, Model.WSC.Global_Settings.Physical_Root & "data/wales",  actual, 1 );
      Create( db, "working/users/test_user/data/pt1/", estimated, iteration );
      hh := new BHPS.Household;
      Open( ptrs );
      xptr := Load_XWave;
      Restore_All_Indexes( "/mnt/data/bhps/bin/", index_map ); 
      for wave in  'q' .. Waves_With_Data'Last loop
         this_wave := wave;
         Log( "on wave " & wave );
         index_map( wave ).Iterate( Compare_To_Model_Data'Access );
      end loop;
      Close( ptrs );
      Free( hh );
      db.Close;
   end if;
   Put_Line( num_comparisons'Img & " compared OK " );
end Compare_Raw_And_Derived;
