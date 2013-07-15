with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Assertions;

with Base_Model_Types;
with Costs_Tabulator.Text_IO;
with Costs_Tabulator;
with ONS_Definitions;
with Text_Utils;

with BHPS; 
with BHPS.Binary_IO;
with BHPS.Print;
with BHPS_Indexes;
with BHPS_Enums;
with BHPS.XLookup;

procedure BHPS_Tabulator is
use Ada.Text_IO;
use Ada.Strings.Unbounded;
use Ada.Command_Line;
use Base_Model_Types;
use Ada.Assertions;

use BHPS; 
use BHPS.Binary_IO;
use BHPS.Print;
use BHPS.XLookup;
use BHPS_Indexes;
use BHPS_Index_Package;
use BHPS_Map_Package;
use BHPS_Enums;


   type Tab_Component is ( in_next_wave, death, birth, institutionalised, dropped_out, other, population ); -- .... 
      
   type Table_Col is array ( Tab_Component ) of Amount;

   package BHPS_Tab is new Costs_Tabulator( 
      Data_Type       => Amount, 
      Breakdown_Range => ONS_Definitions.Age_Band, 
      Values_Range    => Tab_Component,
      Values_Array    => Table_Col ); 
   package BHPS_Tab_IO is new BHPS_Tab.Text_IO;
      
   subtype Table is BHPS_Tab.Table_Type;
   
   type Tables_All_Waves is array( Hgsex_Type, Waves ) of Table;

   tables        : Tables_All_Waves;
   ptrs          : Record_Pointers;
   this_wave     : Waves;
   x_index       : X_Wave_Lookup;
   hh                   : Household_Access;

   function Create_Data( indall : Indall_Rec; next_wave : Waves ) return Table_Col is
      data       : Table_Col := ( others => 0.0 );
      wv         : Wave_Location;
   begin
      data( population ) := 1.0;
      wv := x_index.Get_Household_Id( next_wave, indall.pid );
      case wv.summary_status is
         when dead => data( death ) := 1.0;
         when institutionalised => data( institutionalised ) := 1.0;
         when ok => data( in_next_wave ) := 1.0;
         when refused => data( dropped_out ) := 1.0;
         when other => data( other ) := 1.0;
      end case;
      -- check for different HHLD?
      return data;
   end Create_Data;
   
   
   procedure Load_Household( c : Index_Cursor ) is
      index : Index_Rec := Element( c );
      num_people : Person_Count := 0;
      bu         : Benefit_Unit;
      ad         : Adult;
      weight     : Amount;
      hh_weight  : Amount;
      ch         : BHPS.Child;
      num_bus    : Benefit_Unit_Count := 0;
      ppos       : Found_Person_Number_Array;
      age_band   : ONS_Definitions.Age_Band;
      data       : Table_Col := ( others => 0.0 );
      next_wave  : Waves := Waves'Succ( this_wave );
   begin
      Put_Line( "on hhld " & Sernum_Value'Image( index.SERNUM ));
      hh.all := Load_Household( index, ptrs, this_wave );
      if( hh.hhresp.region = wales )then
         ppos := hh.Get_All_Person_Numbers;
         -- for pno in 1 .. hh.Num_People loop
         --   ad := hh.Get_Person( ppos( pno ));
            -- all_adults.Insert( ad.pid, ad );
         -- end loop;
         num_bus := hh.Num_Benefit_Units;
         for b in 1 .. num_bus loop
            bu := Get_Benefit_Unit( hh, b );
            num_people := num_people + bu.Num_People;
            for adno in 1 .. bu.Num_With_Adult_Records loop
               ad := bu.Get_With_Adult_Records( adno );
               if this_wave = 'a' then -- enumeration, not respondent : we need a respondent flag!
                  weight := ad.indall.xewght;
               else
                  weight := ad.indall.xewght;
               end if;
               weight := 1.0;
               age_band := ONS_Definitions.Get_Age_Band( ad.indall.age );
               data := Create_Data( ad.indall, next_wave );
               BHPS_Tab.Add_Observation( tables( ad.indall.hgsex, this_wave ), age_band, weight, data );
            end loop;
            for chno in 1 .. bu.Num_Children loop
               ch := bu.Get_Child( chno );
               if this_wave = 'a' then -- enumeration, not respondent : we need a respondent flag!
                  weight := ch.indall.XEWGHT;
               else
                  weight := ch.indall.XEWGHT;
               end if;
               weight := 1.0;
               age_band := ONS_Definitions.Get_Age_Band( ch.indall.age );
               data := Create_Data( ch.indall, next_wave );
               BHPS_Tab.Add_Observation( tables( ch.indall.hgsex, this_wave ), age_band, weight, data );
               Assert( ch.age < 16, "age should be < 16 was " & Age_Range'Image( ch.age ));
            end loop;
            Assert( bu.Num_Children + bu.Num_With_Adult_Records = bu.num_people, 
               "child+adult mismatch for bu " & Benefit_Unit_Count'Image( b ) &
               " child " & Person_Count'Image( bu.Num_Children ) &
               " adult " & Person_Count'Image( bu.Num_With_Adult_Records ) &
               " total " & Person_Count'Image( bu.Num_People ));
         end loop;
      end if;
   end Load_Household;
      
   
   procedure Load_Adult( c : Index_Cursor ) is
      index : Index_Rec := Element( c );
      ad         : Adult := Load_Adult( index, ptrs, this_wave );
      weight     : Amount;
      age_band   : ONS_Definitions.Age_Band;
      data       : Table_Col := ( others => 0.0 );
      next_wave  : Waves := Waves'Succ( this_wave );
   begin
      Put_Line( "on person " & Sernum_Value'Image( index.SERNUM ));
      if this_wave = 'a' then -- enumeration, not respondent : we need a respondent flag!
         weight := ad.indall.xewght;
      else
         weight := ad.indall.xewght;
      end if;
      Put_Line( "weight is " & Amount'Image( weight ));
  
      if( ad.indall.pid > 0 )then
         age_band := ONS_Definitions.Get_Age_Band( ad.indall.age );
         data := Create_Data( ad.indall, next_wave );
         -- check for different HHLD?
         BHPS_Tab.Add_Observation( tables( ad.indall.hgsex, this_wave ), age_band, weight, data );
      end if;
   end Load_Adult;
   
   year        : Positive := 1991;
   f           : File_Type;
   index_map   : BHPS_Index;
begin
   Create( f, Out_File, Argument( 1 ));
   x_index := Load_XWave;
   hh := new Household;
   
   for wave in Waves_With_Data loop
      this_wave := wave;
      Restore_Complete_Index( "/mnt/data/bhps/bin/" & wave & "/index.bin", index_map ); 
      -- Restore_Complete_Index( "/mnt/data/bhps/bin/" & wave & "/individual_index.bin", index_map ); 
      Open( ptrs, wave );
      index_map.Iterate( Load_Household'Access );
      for sx in Hgsex_Type loop
         if( sx >= male )then
            Put_Line( f, sx'Img );
            New_Line( f );
            BHPS_Tab_IO.Print( f, "Table " & wave & " : " & sx'Img, tables( sx, wave ), 1, "Age Range", "Stuff", year, 1 );
            New_Line( f );
         end if;
      end loop;
      year := year + 1;
      Close( ptrs, wave );
      
   end loop;
   Free( hh ); 
   
   Close( f );   
end BHPS_Tabulator;

