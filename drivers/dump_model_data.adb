with Ada.Text_IO;
with Ada.Directories;
with Ada.Assertions;
with Ada.Command_Line;
with Ada.Calendar;

with Model;
with Model.WSC.Run_Settings;
with Model.WSC.Household;
with Model.WSC.Household.Database;
with Model.WSC.Household.Regressions;
with Model.WSC.Global_Settings;
with Model.WSC.Run_Declarations;

with Base_Model_Types;
with WSC_Enums;

procedure Dump_Model_Data is

   use Model.WSC.Household.Database;
   use Model.WSC.Household;
   use Base_Model_Types;
   use WSC_Enums;
   use Ada.Text_IO;
   use Model.UK_Format_Utils;
   use Model.WSC.Run_Declarations;
   
   wsc_run : Run; 
   sernums  : Sernum_List;
   in_db     : DB_Type;
   out_db   : DB_Type;
   hh       : Household;
   wave     : Waves := r;
   outf     : File_Type;
begin
   Create( outf, Out_File, Ada.Command_Line.Argument( 3 ));
   wsc_run := Model.WSC.Run_Declarations.Read_Settings( Ada.Command_Line.Argument( 1 ));
   Model.WSC.Global_Settings.Read_Settings( Ada.Command_Line.Argument( 2 ));
   
   Open( in_db, Model.WSC.Global_Settings.Physical_Root & "data/wales",  actual, 1 ); 
   in_db.Get_Household_Sernums( wave, sernums );

   in_db.Get_Household_Sernums( wave, sernums );
   declare
      num_households     : constant Positive := Positive( sernums.Length );
   begin
      households:
      for hhno in 1 .. num_households loop
         hh := in_db.Get_Household( 
            wave => r, 
            hid  => sernums.Element( hhno ));
         benunits:
         for buno in 1 .. hh.num_benefit_units loop
            for adno in 1 .. hh.benefit_units( buno ).num_adults loop
               -- if( adno.pid /= -9 )then
                  declare
                     ad : Person renames hh.benefit_units( buno ).adults( adno );
                  begin
                     Put_Line( outf, 
                        hh.hid'Img & "," & 
                        ad.pno'Img & "," &
                        ad.pid'Img & "," &
                        ad.sex'Img & "," &
                        ad.age'Img & "," &
                        Format( ad.current_income( attendance_allow )) & "," &
                        Format( ad.current_income( disab_liv_allowcare )) & "," &
                        Format( ad.current_income( disab_liv_allowmob )) & "," &
                        Format( ad.current_income( disab_liv_allow_dk )) & "," );
                        -- Format( ad.annual_income( attendance_allow )) & "," &
                        -- Format( ad.annual_income( disab_liv_allowcare )) & "," &
                        -- Format( ad.annual_income( disab_liv_allowmob )) & "," &
                        -- Format( ad.annual_income( disab_liv_allow_dk ))
                  end;
               -- end loop;
            end loop;
         end loop benunits;
      end loop households;
   end;
   Close( outf );
end Dump_Model_Data;
