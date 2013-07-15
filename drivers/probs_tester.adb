with Ada.Text_IO;
with Maths_Functions;
with Model.WSC.Household.Database;
with Model.WSC.Household;
with Base_Model_Types;
with WSC_Enums;
with Model.WSC.Household.Regressions;
with ONS_Definitions;

procedure Probs_Tester is

   use Ada.Text_IO;
   use ONS_Definitions;
   use WSC_Enums;
   use Base_Model_Types;

   type Age_Sex_Crosstab is array( ONS_Definitions.Age_Band, Gender_Type ) of Amount;
      
   death_rates : Age_Sex_Crosstab := ( others => ( others => 0.0 ));
   populations : Age_Sex_Crosstab := ( others => ( others => 0.0 ));
   
   procedure Generate_Death_Probits is
      package MF is new Maths_Functions( Amount );
         
      use Model.WSC.Household.Database;
      use Model.WSC.Household;
      use Base_Model_Types;
      use Model.WSC.Household.Regressions;
      
      age_band    : ONS_Definitions.Age_Band; 
      db          : DB_Type;
      hh          : Household;
      wave        : Waves := r;
      sernums     : Sernum_List;
   begin
      Open( db, "data/wales",  actual, 1 );
      db.Get_Household_Sernums( r, sernums ); 
      declare
         num_households     : constant Positive := Positive( sernums.Length );
      begin
         Households:
         for hhno in 1 .. num_households loop
            hh := db.Get_Household( 
               wave => r, 
               hid  => sernums.Element( hhno ));
            Benefit_Units:
            for buno in 1 .. hh.num_benefit_units loop
               Adults:
               for adno in 1 .. hh.benefit_units( buno ).num_adults loop
                  declare
                     ad         : Person renames hh.benefit_units( buno ).adults( adno );
                     dp         : Real := 0.0;
                     death_prob : Probability := 0.0;
                     weight     : Amount := hh.hdata.weights( extended_2 );
                  begin
                     -- age_range := Age_Band_From_Age( ad.age );
                     if( ad.age >= 65 ) and (ad.pid > 0 ) then
                        age_band := ONS_Definitions.Get_Age_Band( ad.age );
                        dp := Dies_This_Period_Probit( ad, hh.hdata.region );
                        death_prob := MF.Cumulative_Normal( dp ); 
                        Put_Line( 
                           hh.hid'Img & "," & 
                           ad.pid'Img & ", " & 
                           ad.age'Img & "," & 
                           age_band'Img & "," &
                           Gender_Type'Pos( ad.sex )'Img & "," 
                           & death_prob'Img );
                        populations( age_band, ad.sex ) := populations( age_band, ad.sex ) + weight;
                        death_rates( age_band, ad.sex ) := death_rates( age_band, ad.sex ) + death_prob*weight;
                     end if;
                  end;
               end loop Adults;
            end loop Benefit_Units;
         end loop Households;
      end;
      db.Close;
   end Generate_Death_Probits;

begin
   Put_Line( "HID,PID,AGE,ONS_AGE_BAND,SEX,P_DEATH" );
   Generate_Death_Probits;
   
   for age_band in age_65_to_74 .. age_85_plus loop
      for sex in male .. female loop
         if( populations( age_band, sex ) > 0.0 )then
            death_rates( age_band, sex ) := 1_000.0 * death_rates( age_band, sex ) / populations( age_band, sex );
         end if;
      end loop;
   end loop;
   for sex in male .. female loop
      Put_Line( sex'Img & " " );
      for age_band in age_65_to_74 .. age_85_plus loop
         Put( age_band'Img & " : " );
         Put_Line( populations( age_band, sex )'Img );
      end loop;
      New_Line;
   end loop;
   New_Line;
   New_Line;
   for sex in male .. female loop
      Put_Line( sex'Img & " " );
      for age_band in age_65_to_74 .. age_85_plus loop
         Put( age_band'Img & " : " );
         Put_Line( death_rates( age_band, sex )'Img  );
      end loop;
      New_Line;
   end loop;
   New_Line;
   
end Probs_Tester;
