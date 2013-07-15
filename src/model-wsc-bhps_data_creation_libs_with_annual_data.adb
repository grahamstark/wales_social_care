with Ada.Text_IO;
with Ada.Assertions;
with Base_Model_Types;
with BHPS_Enums;
with WSC_Enums;
with BHPS_Indexes;
with Ada.Calendar;
with GNATColl.Traces;
with Model.WSC.Household.Regressions;
with Model.WSC.Parameters.Historic;

package body Model.WSC.BHPS_Data_Creation_Libs is

   use Ada.Assertions;
   use Ada.Text_IO;
   use Base_Model_Types;
   use WSC_Enums;
   use Model.WSC.Regressors;
   use Model.WSC.Household.Regressions;
   
   WEEKS_PER_MONTH : constant Rate := 52.0/12.0;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.BHPS_DATA_CREATION_LIBS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
   function Make_Activities_Of_Daily_Living_Score( adl_x_d : Adlad_Type; adl_x : Adla_Type ) return Amount is
      score : Amount := 0.0;
   begin
     if( not Null_Or_Missing( adl_x ))then
        case adl_x_d is
           when fairly_difficult => score := 1.0;
           when very_difficult =>  score := 3.0;
           when others => null;
        end case;
        if( adl_x = with_help_from_someone_else ) then
           score := 5.0;
        elsif( adl_x = not_at_all ) then 
          score := 10.0;
        end if;
     end if;
     return score;
   end Make_Activities_Of_Daily_Living_Score;

   
   procedure Impute_Benefit_Levels( pers : in out wscm.Person ) is
   use Model.WSC.Parameters;
      LEEWAY : constant Amount := 1.05;
      params : Parameters_Rec := Historic.Get_Parameters( pers.wave );
      -- FIXME check year this changed to have split?
      unassigned_dla : constant Amount := pers.current_income( disab_livng_allwnce ) + pers.current_income(  disab_liv_allow_dk ); 
   begin
      pers.disability_living_allowance_care_level := nil;
      pers.disability_living_allowance_mobility_level := nil;
      pers.attendance_allowance_level := nil;

      if( pers.current_income( disab_liv_allowcare ) > 0.0 )then
         if( pers.current_income( disab_liv_allowcare ) <= params.benefits.dla.care.benefit_rate( low ) * LEEWAY )then 
            pers.disability_living_allowance_care_level := low;
         elsif( pers.current_income( disab_liv_allowcare ) <= params.benefits.dla.care.benefit_rate( middle ) * LEEWAY  ) then
            pers.disability_living_allowance_care_level:= middle;
         else
            pers.disability_living_allowance_care_level := high;
         end if;
         Log( "disab_liv_allowcare: comparing " & 
              Format( pers.current_income( disab_liv_allowcare )) & " with " & 
              Format( params.benefits.dla.care.benefit_rate( high )) & " and " & 
              Format( params.benefits.dla.care.benefit_rate( middle )) & " => " &
              pers.disability_living_allowance_care_level'Img );
      end if;
      
      if( pers.current_income( disab_liv_allowmob ) > 0.0 )then
         if( pers.current_income( disab_liv_allowmob ) <= params.benefits.dla.mobility.benefit_rate( low ) * LEEWAY )then 
            pers.disability_living_allowance_mobility_level := low;
         else
            pers.disability_living_allowance_mobility_level := high;
         end if;
         Log( "disab_liv_allowmob: comparing " & 
              Format( pers.current_income( disab_liv_allowmob )) & " with " & 
              Format( params.benefits.dla.mobility.benefit_rate( high )) & " and " & 
              Format( params.benefits.dla.mobility.benefit_rate( low )) & " => " &
              pers.disability_living_allowance_mobility_level'Img );
      end if;
      
      if( unassigned_dla > 0.0 )then
         if( pers.disability_living_allowance_mobility_level /= nil  ) and (  pers.disability_living_allowance_care_level = nil )then
            if( unassigned_dla <= params.benefits.dla.care.benefit_rate( low ) * LEEWAY )then 
               pers.disability_living_allowance_care_level := low;
            elsif( unassigned_dla <= params.benefits.dla.care.benefit_rate( middle ) * LEEWAY ) then
               pers.disability_living_allowance_care_level := middle;
            else
               pers.disability_living_allowance_care_level := high;
            end if;
        elsif( pers.disability_living_allowance_care_level /= nil  ) and ( pers.disability_living_allowance_mobility_level = nil )then
            if( unassigned_dla <= params.benefits.dla.mobility.benefit_rate( low ) * LEEWAY )then 
               pers.disability_living_allowance_mobility_level := low;
            else
               pers.disability_living_allowance_mobility_level := high;
            end if;
         else -- pick mob/care randomly
            declare
               p : Probability := Maths_Funcs.Random_0_To_1;
               -- from 2011 feb tabulator - cumulative proportions of 1 kind dla only - mob high/low care high/med/low all UK
               -- DLA_SINGLE_PROPNS : constant array( 1 .. 5 ) of Amount := (
                  -- 0.421822272215973,
                  -- 0.535433070866142,
                  -- 0.584926884139483,
                  -- 0.714285714285714,
                  -- 1.0 );
                  
            begin 
               --
               -- FIXME THIS ISN'T RIGHT: could be on both!
               --
               if( p < 0.535433070866142 ) then -- mob dla only cases as propn on only mob + only care - All UK
                  if( unassigned_dla <= params.benefits.dla.mobility.benefit_rate( low ) * LEEWAY )then 
                     pers.disability_living_allowance_mobility_level := low;
                  else
                     pers.disability_living_allowance_mobility_level := high;
                  end if;
               else   
                  if( unassigned_dla <= params.benefits.dla.care.benefit_rate( low ) * LEEWAY )then 
                     pers.disability_living_allowance_care_level := low;
                  elsif( unassigned_dla <= params.benefits.dla.care.benefit_rate( middle ) * LEEWAY ) then
                     pers.disability_living_allowance_care_level := middle;
                  else
                     pers.disability_living_allowance_care_level := high;
                  end if;
               end if;
            end; 
         end if;
         Log( "disab_liv_allow_dk: assigned unassigned_dla of " & Format( unassigned_dla ) & " to " &
              "mobility " & pers.disability_living_allowance_mobility_level'Img &
              " care " & pers.disability_living_allowance_care_level'Img);
      end if;

      if( pers.current_income( attendance_allow ) > 0.0 )then
         if( pers.current_income( attendance_allow ) > params.benefits.attendance_allowance.benefit_rate( high ) * LEEWAY )then 
            pers.attendance_allowance_level := high;
         else
            pers.attendance_allowance_level := low;
         end if;
         Log( "attendance_allow: comparing " & 
              Format( pers.current_income( attendance_allow )) & " with " & 
              Format( params.benefits.attendance_allowance.benefit_rate( high )) & " => " &
              pers.attendance_allowance_level'Img );
      end if;
        
   end Impute_Benefit_Levels;
   
   function Make_Individual_Regressors(       
      ad                         : BHPS.Adult; 
      ad_last_period             : BHPS.Adult; 
      ad_next_period             : BHPS.Adult;
      summary_status_next_period : Summary_Status_Type;
      region                     : BHPS_Enums.Region_Type;
      nkids                      : Integer ) return Regressors_Array is
      r :  Regressors_Array := ( others => 0.0 );
      r_next_period :  Regressors_Array := ( others => 0.0 );
      r_last_period :  Regressors_Array := ( others => 0.0 );
      rage : constant Amount := Amount( ad.age );
   begin
      r( const ) := 1.0;
      if( nkids > 0 )then
         r( hhkids ) := 1.0;
      end if;
      r( age ) := rage;
      r( age_2 ) := ( rage ** 2 ) / 100.0;
      r( age_3 ) := ( r( age_2 ) * rage ) / 100.0;
      r( age_4 ) := ( r( age_3 ) * rage ) / 100.0;
      r( age_5 ) := ( r( age_4 ) * rage ) / 100.0;
      
      if( ad.indall.hgsex = female ) then r( female ) := 1.0; end if;
      if( ad.indall.hgsex = male ) then r( male ) := 1.0; end if;
      if( ad.indresp.spinhh = yes ) then
         r( spinhh ) := 1.0;
      end if;
      if( ad_next_period.indsamp.lvwhy = deceased or summary_status_next_period = dead )then
         r( dead ) := 1.0;
      end if;
      case ad.age is
         when 40 .. 44 => r( age4044 ) := 1.0;
         when 45 .. 49 => r( age4549 ) := 1.0;
         when 50 .. 54 => r( age5054 ) := 1.0;
         when 55 .. 59 => r( age5559 ) := 1.0;
         when 60 .. 64 => r( age6064 ) := 1.0;
         when 65 .. 69 => r( age6569 ) := 1.0;
         when 70 .. 74 => r( age7074 ) := 1.0;
         when 75 .. 79 => r( age7579 ) := 1.0;
         when 80 .. 84 => r( age8084 ) := 1.0;
         when 85 .. 89 => r( age8589 ) := 1.0;
         when 90 .. Age_Range'Last => r( age90 ) := 1.0;
         when others => null;
      end case;
      if( ad.age >= 65 )then
         r( age65o ) := 1.0;
      end if;
      if( ad.age = 64 )then
         r( age64 ) := 1.0;
      end if;
      if( ad.age = 59 )then
         r( age59 ) := 1.0;
      elsif( ad.age = 66 )then
         r( age66 ) := 1.0;
      elsif( ad.age = 67 )then
         r( age67 ) := 1.0;
      elsif( ad.age = 68 )then
         r( age68 ) := 1.0;
      elsif( ad.age = 69 )then
         r( age69 ) := 1.0;
      elsif( ad.age = 70 )then
         r( age70 ) := 1.0;
      end if;
      
      if ad.indresp.mlstat = married then
        r( married ) := 1.0; 
      end if;
      if ad.indresp.ivlpar  = married_and_with_spouse or
          ad.indresp.ivlpar = cohabiting or 
          ad.indresp.ivlpar = in_civil_partnership then
         r( couple ) := 1.0;
      end if;
      if ad.indresp.spinhh = no and ad_last_period.indresp.spinhh = yes then
         r( seperate ) := 1.0;
      end if;
      
      case ad.indresp.region is
         when inner_london .. r_of_north => r( england ) := 1.0; 
         when wales            => r( wales ) := 1.0;
         when scotland         => r( scotland ) := 1.0;
         when northern_ireland => r( nireland ) := 1.0;
         when others => null;
      end case;
      
      case ad.indresp.hlstat is
         when dont_know | refused | proxy_and_or_phone | inapplicable | missing_or_wild
                          => r( hl_m )     := 1.0;
                             r( healthsc ) := 0.0;
         when excellent   => r( hl_exc )   := 1.0;
                             r( healthsc ) := 1.0;
         when good        => r( hl_good )  := 1.0;
                             r( healthsc ) := 2.0;
         when fair        => r( hl_fair )  := 1.0;
                             r( healthsc ) := 3.0;
         when poor        => r( hl_poor )  := 1.0;
                             r( healthsc ) := 4.0;
         when very_poor   => r( hl_vpoor ) := 1.0;
                             r( healthsc ) := 5.0;
      end case;

      
      case ad_next_period.indresp.hlstat is
         when dont_know | refused | proxy_and_or_phone | inapplicable | missing_or_wild
                          => r_next_period( hl_m )     := 1.0;
                             r_next_period( healthsc ) := 0.0;
         when excellent   => r_next_period( hl_exc )   := 1.0;
                             r_next_period( healthsc ) := 1.0;
         when good        => r_next_period( hl_good )  := 1.0;
                             r_next_period( healthsc ) := 2.0;
         when fair        => r_next_period( hl_fair )  := 1.0;
                             r_next_period( healthsc ) := 3.0;
         when poor        => r_next_period( hl_poor )  := 1.0;
                             r_next_period( healthsc ) := 4.0;
         when very_poor   => r_next_period( hl_vpoor ) := 1.0;
                             r_next_period( healthsc ) := 5.0;
      end case;
      
      r( trend ) := Amount( Year_From_Wave( ad.wave ) - 1998 );
      r( trend_2 ) := r( trend ) ** 2;
      r( trend_3 ) := r( trend ) ** 3;
      
      case ad.indresp.hldsbl is
         when yes => r( disabled ) := 1.0;
         when dont_know | refused | proxy_and_or_phone | inapplicable | missing_or_wild =>
           r( disab_m ) := 1.0;
         when others => null;
      end case;
      
      case ad.indresp.tenure is
         when proxy_and_or_phone | missing => r( ten_m ) := 1.0;
         when owned_outright => 
            r( owned_o ) := 1.0;
            r( owned ) := 1.0;
         when owned_with_mortgage => 
            r( owned_m ) := 1.0;
            r( owned ) := 1.0;
         when local_authority_rented => 
            r( la_rent ) := 1.0; 
            r( renter ) := 1.0;
            r( socrent ) := 1.0;
         when housing_assoc_rented => 
            r( ha_rent ) := 1.0;
            r( renter ) := 1.0;
            r( socrent ) := 1.0;
         when rented_from_employer | rented_private_unfurnished | rented_private_furnished | other_rented => 
            r( oth_rent ) := 1.0;
            r( renter ) := 1.0;
      end case;
      case ad_next_period.indresp.tenure is
         when proxy_and_or_phone | missing => 
            r_next_period( ten_m ) := 1.0;
         when owned_outright => 
            r_next_period( owned_o ) := 1.0;
            r_next_period( owned ) := 1.0;
         when owned_with_mortgage => 
            r_next_period( owned_m ) := 1.0;
            r_next_period( owned ) := 1.0;
         when local_authority_rented => 
            r_next_period( la_rent ) := 1.0; 
            r_next_period( renter ) := 1.0;
            r_next_period( socrent ) := 1.0;
         when housing_assoc_rented => 
            r_next_period( ha_rent ) := 1.0;
            r_next_period( renter ) := 1.0;
            r_next_period( socrent ) := 1.0;
         when rented_from_employer | rented_private_unfurnished | rented_private_furnished | other_rented => 
            r_next_period( oth_rent ) := 1.0;
            r_next_period( renter ) := 1.0;
      end case;
      if( r( renter ) = 1.0 and r_next_period( owned ) = 1.0 )then
         r( rent2own ) := 1.0;
      end if;
      if( r( owned ) = 1.0 and r_next_period( renter ) = 1.0 )then
         r( own2rent ) := 1.0;
      end if;
      
      if( ad.indresp.tenure in owned_outright .. owned_with_mortgage )then
         r( owned ) := 1.0;
      elsif( ad.indresp.tenure in local_authority_rented .. housing_assoc_rented )then
         r( socrent ) := 1.0;
      end if;
      if( r( renter ) = 1.0 and r_next_period( owned ) = 1.0 )then
         r( rent2own ) := 1.0;
      end if;
      if( r( owned ) = 1.0 and r_next_period( renter ) = 1.0 )then
         r( own2rent ) := 1.0;
      end if;

      r( adlsc_a ) := Make_Activities_Of_Daily_Living_Score( ad.indresp.adlad, ad.indresp.adla );
      r( adlsc_b ) := Make_Activities_Of_Daily_Living_Score( ad.indresp.adlbd, ad.indresp.adlb );
      r( adlsc_c ) := Make_Activities_Of_Daily_Living_Score( ad.indresp.adlcd, ad.indresp.adlc );
      r( adlsc_d ) := Make_Activities_Of_Daily_Living_Score( ad.indresp.adldd, ad.indresp.adld );
      r( adlsc_e ) := Make_Activities_Of_Daily_Living_Score( ad.indresp.adled, ad.indresp.adle );
      r( adlscore ) := r( adlsc_a ) +
         r( adlsc_b ) +
         r( adlsc_c ) +
         r( adlsc_d ) +
         r( adlsc_e );
      -- a 
      if( r( adlsc_a ) = 0.0 )then
         r( adla_none ) := 1.0;
      elsif( r( adlsc_a ) = 1.0 )then
         r( adla_diff ) := 1.0;
      elsif( r( adlsc_a ) = 3.0 )then
         r( adla_vdiff ) := 1.0;
      elsif( r( adlsc_a ) = 5.0 )then
         r( adla_help ) := 1.0;
      elsif( r( adlsc_a ) = 10.0 )then
         r( adla_not ) := 1.0;
      end if; 
      -- b
      if( r( adlsc_b ) >= 3.0 and r( adlsc_b ) <= 10.0 )then
         r( adlb_d2 ) := 1.0;
      end if;
      if( r( adlsc_b ) = 0.0 )then
         r( adlb_none ) := 1.0;
      elsif( r( adlsc_b ) = 1.0 )then
         r( adlb_diff ) := 1.0;
      elsif( r( adlsc_b ) = 3.0 )then
         r( adlb_vdiff ) := 1.0;
      elsif( r( adlsc_b ) = 5.0 )then
         r( adlb_help ) := 1.0;
      elsif( r( adlsc_b ) = 10.0 )then
         r( adlb_not ) := 1.0;
      end if; 
      if( r( adlsc_b ) >= 3.0 and r( adlsc_b ) <= 10.0 )then
         r( adlb_d2 ) := 1.0;
      end if;
      -- c
      if( r( adlsc_c ) >= 3.0 and r( adlsc_c ) <= 10.0 )then
         r( adlc_d2 ) := 1.0;
      end if;
      if( r( adlsc_c ) = 0.0 )then
         r( adlc_none ) := 1.0;
      elsif( r( adlsc_c ) = 1.0 )then
         r( adlc_diff ) := 1.0;
      elsif( r( adlsc_c ) = 3.0 )then
         r( adlc_vdiff ) := 1.0;
      elsif( r( adlsc_c ) = 5.0 )then
         r( adlc_help ) := 1.0;
      elsif( r( adlsc_c ) = 10.0 )then
         r( adlc_not ) := 1.0;
      end if; 
      if( r( adlsc_c ) >= 3.0 and r( adlsc_c ) <= 10.0 )then
         r( adlc_d2 ) := 1.0;
      end if;
      -- d
      if( r( adlsc_d ) >= 3.0 and r( adlsc_d ) <= 10.0 )then
         r( adld_d2 ) := 1.0;
      end if;
      if( r( adlsc_d ) = 0.0 )then
         r( adld_none ) := 1.0;
      elsif( r( adlsc_d ) = 1.0 )then
         r( adld_diff ) := 1.0;
      elsif( r( adlsc_d ) = 3.0 )then
         r( adld_vdiff ) := 1.0;
      elsif( r( adlsc_d ) = 5.0 )then
         r( adld_help ) := 1.0;
      elsif( r( adlsc_d ) = 10.0 )then
         r( adld_not ) := 1.0;
      end if; 
      if( r( adlsc_d ) >= 3.0 and r( adlsc_d ) <= 10.0 )then
         r( adld_d2 ) := 1.0;
      end if;
      -- b
      if( r( adlsc_e ) >= 3.0 and r( adlsc_e ) <= 10.0 )then
         r( adle_d2 ) := 1.0;
      end if;
      if( r( adlsc_e ) = 0.0 )then
         r( adle_none ) := 1.0;
      elsif( r( adlsc_e ) = 1.0 )then
         r( adle_diff ) := 1.0;
      elsif( r( adlsc_e ) = 3.0 )then
         r( adle_vdiff ) := 1.0;
      elsif( r( adlsc_e ) = 5.0 )then
         r( adle_help ) := 1.0;
      elsif( r( adlsc_e ) = 10.0 )then
         r( adle_not ) := 1.0;
      end if; 
      if( r( adlsc_e ) >= 3.0 and r( adlsc_e ) <= 10.0 )then
         r( adle_d2 ) := 1.0;
      end if;
      
      if( ad.indresp.aidhh = yes )then
         declare
            p : Integer := Regressors_Type'Pos( carepno1 ) + ad.pno - 1;
            c : Regressors_Type := Regressors_Type'Val( p );
         begin
            r( c ) := Amount( ad.indresp.aidhua );
         end;
      end if;
      
      if ad.indresp.f127 = disab_liv_allw_mob then
         r( dlamob ) := 1.0;   
      end if;
      if ad.indresp.f126 = disab_liv_allw_care then
         r( dlacare ) := 1.0;   
      end if;
      if ad.indresp.f128 = disab_liv_allw_dk then
         r( dladk ) := 1.0;   
      end if;
      if( r( dlamob ) + r( dlacare ) + r( dladk ) > 0.0 )then
         r( dlaany ) := 1.0;
      end if;
      if( ad.indresp.f119 = attendance_allow )then
         r( aa ) := 1.0;
      end if;
      -- health score - looking forward
      if( r_next_period( healthsc ) > r( healthsc ))then
         r( hl_worse ) := 1.0; --
      elsif( r_next_period( healthsc ) > r( healthsc ))then
         r( hl_better ) := 1.0; --
      end if;

      case ad.indresp.jbstat is
         when in_paid_employ | self_employed => r( working ) := 1.0;
         when retired => r( retired ) := 1.0;
         when others => null;
      end case;
      case ad_next_period.indresp.jbstat is
         when in_paid_employ | self_employed => r_next_period( working ) := 1.0;
         when retired => r_next_period( retired ) := 1.0;
         when others => null;
      end case;
      if( r( working ) = 1.0 and r_next_period( working ) = 0.0 )then
         r( retire ) := 1.0;
      end if;
      if( r( working ) = 0.0 and r_next_period( working ) = 1.0 )then
         r( reenter ) := 1.0;
      end if;
 
      case region is
         when 
            inapplicable |
            missing => null;
         when inner_london => r( reg_il ) := 1.0;
         when outer_london => r( reg_ol ) := 1.0;
         when r_of_south_east => r( reg_se ) := 1.0;
         when south_west => r( reg_sw ) := 1.0;
         when east_anglia => r( reg_ee ) := 1.0;
         when east_midlands => r( reg_em ) := 1.0;
         when west_midlands_conurbation => r( reg_wmc ) := 1.0;
         when r_of_west_midlands => r( reg_wmo ) := 1.0;
         when greater_manchester => r( reg_gm ) := 1.0;
         when merseyside => r( reg_me ) := 1.0;
         when r_of_north_west => r( reg_nwo ) := 1.0;
         when south_yorkshire => r( reg_sy ) := 1.0;
         when west_yorkshire => r( reg_wy ) := 1.0;
         when r_of_yorks_and_humberside => r( reg_yho ) := 1.0;
         when tyne_and_wear => r( reg_tw ) := 1.0;
         when r_of_north => r( reg_no ) := 1.0;
         when wales => r( reg_W ) := 1.0;
         when scotland => r( reg_S ) := 1.0;
         when northern_ireland => r( reg_NI ) := 1.0;
      end case;
      
      case ad.indresp.qfedhi is 
         when 
            proxy_respondent |
            proxy |
            missing => null;
         when
            higher_degree |
            first_degree |
            teaching_qf |
            other_higher_qf |
            nursing_qf   => r( hq_deg ) := 1.0;
            when gce_a_levels => r( hq_alev ) := 1.0;
         when gce_o_levels_or_equiv => r( hq_gcse ) := 1.0;
         when commercial_qf_no_o_levels |
            cse_grade_2_5_scot_grade_4_5 |
            apprenticeship |
            other_qf => r( hq_oth ) := 1.0;
         when no_qf |
            still_at_school_no_qf => null;
      end case;
      return r;
   end Make_Individual_Regressors;


   function Map_Aidhrs( aidhrs : Aidhrs_Type ) return Amount is
      hrs : Amount;
   begin
      case aidhrs is
         when dont_know |
               refused  |
               proxy_respondent |
               inapplicable |
               missing_or_wild      => hrs := 0.0;
         when s_0_4_hrs_per_week    => hrs := 2.0; 
         when s_5_9_hrs_per_week    => hrs := 7.5;
         when s_10_19_hrs_per_wk    => hrs := 15.0;
         when s_20_34_hrs_per_wk    => hrs := 27.0;
         when s_35_49_hrs_per_wk    => hrs := 42.0;
         when s_50_99_hrs_per_wk    => hrs := 75.0;
         when s_100_plus_hrs_per_wk => hrs := 100.0;
         when some_other_times |
              varies_under_20       => hrs := 10.0;
         when varies_20_hrs_plus    => hrs := 40.0;   
      end case;
      return hrs;
   end Map_Aidhrs;
   
   function Map_Naidxhh( naidxhh : Naidxhh_Type ) return Amount is
      p : Amount;
   begin
      case naidxhh is
         when dont_know |
         refused |
         proxy_respondent |
         inapplicable |
         missing_or_wild => p := 0.0;
         when one_person => p := 1.0;
         when two_people => p := 2.0;
         when three_people => p := 3.0;
         when four_people => p := 4.0;
         when five_people => p := 5.0;
         when six_people => p := 6.0;
         when seven_plus_people => p := 8.0;
      end case;
      return p;
   end Map_Naidxhh;
   
   procedure Set_Is_Cared_For( hh : BHPS.Household; mhh : in out wscm.Household ) is
      num_people       : BHPS.Person_Count := hh.Num_People;
      pnos             : Found_Person_Number_Array := hh.Get_All_Person_Numbers; 
      potential_carer  : BHPS.Adult;
      adno             : wscm.Adult_Count;
      chno             : wscm.Child_Count;
      buno             : wscm.Child_Number;
      found            : Boolean;
      total_num_cared_for : Amount := 0.0;
      care_per_person  : Amount;
      num_carers       : Amount := 1.0;
      total_hours_of_care_given : Amount := 0.0;
   begin
      for pno in 1 .. num_people loop 
         potential_carer := hh.Get_Person( pnos( pno ));
         declare 
            num_cared_for          : Amount := 0.0;
            num_external_cared_for    : Amount := 0.0;
         begin
            if( potential_carer.indresp.aidxhh = yes )then
               num_external_cared_for := Map_Naidxhh( potential_carer.indresp.naidxhh );
            end if;
            if( potential_carer.indresp.aidhh = yes )then       -- cares for someone
               num_cared_for := num_cared_for + 1.0;
               if( potential_carer.indresp.aidhua /= BHPS.MISS ) and ( potential_carer.indresp.aidhua > 0 )then -- id of 1st cared for person
                  wscm.Find_Person_By_Pno( mhh, potential_carer.indresp.aidhua, buno, adno, chno, found );
                  Assert( found, " p1: nobody located in model for hh " & mhh.hid'Img & " pno " & potential_carer.indresp.aidhua'Img );
                  if( adno > 0 )then
                     mhh.benefit_units( buno ).adults( adno ).regressors( rec_care ) := 1.0;
                     mhh.benefit_units( buno ).adults( adno ).receives_informal_care_from_household_member := True;
                  elsif( chno > 0 )then
                     mhh.benefit_units( buno ).children( chno ).regressors( rec_care ) := 1.0;
                     mhh.benefit_units( buno ).children( chno ).receives_informal_care_from_household_member := True;
                  end if;
               end if;
               if( potential_carer.indresp.aidhub /= BHPS.MISS ) and ( potential_carer.indresp.aidhub > 0 )then-- id of 2nd cared for person
                  wscm.Find_Person_By_Pno( mhh, potential_carer.indresp.aidhub, buno, adno, chno, found  );
                  if( not found )then
                     Put_Line( " p2: nobody located in model for hh " & mhh.hid'Img & "target pno " & potential_carer.indresp.aidhub'Img & " in hhld " &  wscm.To_String( mhh ));
                  end if;
                  Assert( found, "HALTING; err prev line " );
                  num_cared_for := num_cared_for + 1.0;
                  if( adno > 0 )then
                     mhh.benefit_units( buno ).adults( adno ).regressors( rec_care ) := 1.0;
                     mhh.benefit_units( buno ).adults( adno ).receives_informal_care_from_household_member := True;
                  elsif( chno > 0 )then
                     mhh.benefit_units( buno ).children( chno ).regressors( rec_care ) := 1.0;
                     mhh.benefit_units( buno ).children( chno ).receives_informal_care_from_household_member := True;
                  end if;
               end if;
               if( potential_carer.indresp.aidhuc /= BHPS.MISS ) and ( potential_carer.indresp.aidhuc > 0 )then -- 3rd
                  wscm.Find_Person_By_Pno( mhh, potential_carer.indresp.aidhuc, buno, adno, chno, found  );
                  Assert( found, " p3: nobody located in model for hh " & mhh.hid'Img & " pno " & potential_carer.indresp.aidhuc'Img );
                  num_cared_for := num_cared_for + 1.0;
                  if( adno > 0 )then
                     mhh.benefit_units( buno ).adults( adno ).regressors( rec_care ) := 1.0;
                     mhh.benefit_units( buno ).adults( adno ).receives_informal_care_from_household_member := True;
                  elsif( chno > 0 )then
                     mhh.benefit_units( buno ).children( chno ).regressors( rec_care ) := 1.0;
                     mhh.benefit_units( buno ).children( chno ).receives_informal_care_from_household_member := True;
                  end if;
               end if;
            end if;
            if(( num_cared_for + num_external_cared_for ) > 0.0 )then
               num_carers := num_carers + 1.0;
               total_hours_of_care_given := total_hours_of_care_given + Map_Aidhrs( potential_carer.indresp.aidhrs );
            end if;
            total_num_cared_for := total_num_cared_for + num_cared_for + num_external_cared_for;
         end; -- declare  
      end loop;
      if( total_num_cared_for > 0.0 )then
         care_per_person := total_hours_of_care_given / total_num_cared_for;
         for buno in 1 .. mhh.num_benefit_units loop
            for adno in 1 .. mhh.benefit_units( buno ).num_adults loop
               if( mhh.benefit_units( buno ).adults( adno ).receives_informal_care_from_household_member )then
                  mhh.benefit_units( buno ).adults( adno ).hours_of_care_recieved := wscm.Hours_Count( care_per_person );
               end if;
            end loop;
            for chno in 1 .. mhh.benefit_units( buno ).num_children loop
               if( mhh.benefit_units( buno ).children( chno ).receives_informal_care_from_household_member )then
                  mhh.benefit_units( buno ).children( chno ).hours_of_care_recieved := wscm.Hours_Count( care_per_person );
               end if;
            end loop;
         end loop;
      end if;
   end Set_Is_Cared_For;

   procedure Set_Partner_Just_Died( hh : BHPS.Household; mhh : in out wscm.Household ) is
      num_people : BHPS.Person_Count := hh.Num_People;
      pnos       : Found_Person_Number_Array := hh.Get_All_Person_Numbers; 
      ad         : BHPS.Adult;
      mad        : wscm.Person;
      adno       : wscm.Adult_Count;
      chno       : wscm.Child_Count;
      buno       : wscm.Child_Number;
      found      : Boolean;
   begin
      for pno in 1 .. num_people loop 
         ad := hh.Get_Person( pnos( pno ));
         wscm.Find_Person_By_Pno( mhh, pnos( pno ), buno, adno, chno, found );
         Assert( found, "Set_Partner_Just_Died pid not found for " & mhh.hid'Img & "  pnos( pno ) =" &  pnos( pno )'Img );
         mad := mhh.benefit_units( buno ).adults( adno );
         if( ad.indresp.hgspn /= BHPS.MISS ) and ( mad.regressors( dead ) = 1.0 )then     -- partner number
            wscm.Find_Person_By_Pno( mhh, ad.indresp.hgspn,  buno, adno, chno, found ); 
            mhh.benefit_units( buno ).adults( adno ).regressors( pardead ) := 1.0;
         end if;
      end loop;
   end Set_Partner_Just_Died;
   
   function B_Safe_Add( v1 : Amount; v2 : Amount := 0.0 ) return Amount is
      a1 : Amount := v1;
      a2 : Amount := v2;
   begin
      if( a1 = -9.0 ) or ( a1 = -8.0 ) or ( a1 = -3.0 )then
         a1 := 0.0;
      end if;
      if( a2 = -9.0 ) or ( a2 = -8.0 ) or ( a2 = -3.0 )then
         a2 := 0.0;
      end if;
      return a1 + a2;
   end B_Safe_Add;
   

   procedure Create_Incomes( ad : BHPS.Adult; mad : in out wscm.Person ) is

      type A12 is array( 1 .. 2 ) of Amount;
      
      function Make_Annual_And_Current( inc : Income_Rec ) return A12 is
         a : A12 := ( others => 0.0 );
      begin
         if inc.frval = 0.0 or inc.frw = 0.0 then
            a( 1 ) := 0.0;
         else
            a( 1 ) := inc.frval / inc.frw;
         end if;
         Log( "Make_Annual_And_Current; frval" & inc.frval'Img & " frw " & inc.frw'Img );
         if( inc.fim01t /= -9.0 and  inc.fim01t /= -8.0 and inc.fim01t /= -3.0 )then
            a( 2 ) := a( 2 ) + inc.fim01t;
         end if;
         if( inc.fim02t /= -9.0 and  inc.fim02t /= -8.0 and inc.fim02t /= -3.0 )then
            a( 2 ) := a( 2 ) + inc.fim02t;
         end if;
         if( inc.fim03t /= -9.0 and  inc.fim03t /= -8.0 and inc.fim03t /= -3.0 )then
            a( 2 ) := a( 2 ) + inc.fim03t;
         end if;
         if( inc.fim04t /= -9.0 and  inc.fim04t /= -8.0 and inc.fim04t /= -3.0 )then
            a( 2 ) := a( 2 ) + inc.fim04t;
         end if;
         if( inc.fim05t /= -9.0 and  inc.fim05t /= -8.0 and inc.fim05t /= -3.0 )then
            a( 2 ) := a( 2 ) + inc.fim05t;
         end if;
         if( inc.fim06t /= -9.0 and  inc.fim06t /= -8.0 and inc.fim06t /= -3.0 )then
            a( 2 ) := a( 2 ) + inc.fim06t;
         end if;
         if( inc.fim07t /= -9.0 and  inc.fim07t /= -8.0 and inc.fim07t /= -3.0 )then
            a( 2 ) := a( 2 ) + inc.fim07t;
         end if;
         if( inc.fim08t /= -9.0 and  inc.fim08t /= -8.0 and inc.fim08t /= -3.0 )then
            a( 2 ) := a( 2 ) + inc.fim08t;
         end if;
         if( inc.fim09t /= -9.0 and  inc.fim09t /= -8.0 and inc.fim09t /= -3.0 )then
            a( 2 ) := a( 2 ) + inc.fim09t;
         end if;
         if( inc.fim10t /= -9.0 and  inc.fim10t /= -8.0 and inc.fim10t /= -3.0 )then
            a( 2 ) := a( 2 ) + inc.fim10t;
         end if;
         if( inc.fim11t /= -9.0 and  inc.fim11t /= -8.0 and inc.fim11t /= -3.0 )then
            a( 2 ) := a( 2 ) + inc.fim11t;
         end if;
         if( inc.fim12t /= -9.0 and  inc.fim12t /= -8.0 and inc.fim12t /= -3.0 )then
            a( 2 ) := a( 2 ) + inc.fim12t;
         end if;
         Log( "Made Annual as " & Format( a( 2 )));
         return a;
      end Make_Annual_And_Current;

      a : A12;
   begin
   -- payg                                     : Amount        := MISS_R;            -- gross_rate_of_pay_per_monthlast_payment : Gross rate of pay per month:last payment 
   --     paygl                                    : Amount        := MISS_R;            -- gross_pay_at_last_payment : Gross pay at last payment 
   --   jspayl                                   : Amount        := MISS_R;            -- s_or_emp_last_gross_earnings : S/emp: last gross earnings 
   -- jspayu                                   : Amount        := MISS_R;            -- s_or_emp_average_earnings_last_12_months : S/emp: Average earnings last 12 months 
   --jspayw                                   : Amount        := MISS_R;            -- s_or_emp_average_earnings_weekly_or_monthly : S/emp: Average earnings weekly/monthly 
  -- paygly                                   : Amount        := MISS_R;            -- usual_monthly_gross_pay_sept_year_ago : Usual monthly gross pay: Sept year ago 
  -- paynli                                   : Fimnbi_Type   := Fimnbi_Type'First;  -- imputation_flag_apaynly_derived : Imputation flag - APAYNLY (derived) 
  --    paynly                                   : Amount        := MISS_R;            -- usual_monthly_net_pay_sept_year_ago : Usual monthly net pay: Sept year ago 
  -- gross_wage,
      -- gross_self_employment,
      -- jspayg -- monthly self emp
      -- paygya
      -- FIXME!!!!!
      if ad.indresp.paygw <= 0.0 or ad.indresp.payg <= 0.0 then
         mad.current_income( WSC_Enums.gross_wage ) := 0.0;
      else
        mad.current_income( WSC_Enums.gross_wage ) :=  ad.indresp.payg / ad.indresp.paygw;
      end if;
      if ad.indresp.paynlw <= 0.0 or ad.indresp.paynl <= 0.0 then
          mad.current_income( WSC_Enums.net_wage ) := mad.current_income( WSC_Enums.gross_wage );
      else
         mad.current_income( WSC_Enums.net_wage ) :=  ad.indresp.payln / ad.indresp.paynlw;
      end if;
         
      --mad.annual_income( WSC_Enums.gross_wage ) := B_Safe_Add( ad.indresp.paygyr ); -- total_gross_earnings_in_last_12_month
      --mad.current_income( WSC_Enums.gross_wage ) := B_Safe_Add( ad.indresp.payg )/4.0; -- Gross rate of pay per month:last payment 
      --if( mad.current_income( WSC_Enums.gross_wage ) > 0.0 )then
      --   Log( "annual wage " & Format( mad.annual_income( WSC_Enums.gross_wage )) & " weekly wage " & Format( mad.current_income( WSC_Enums.gross_wage )));
      --end if;
      --mad.annual_income( WSC_Enums.net_wage ) := B_Safe_Add( ad.indresp.paygyr ); -- total_net_earnings_in_last_12_month
      --mad.current_income( WSC_Enums.net_wage ) := B_Safe_Add( ad.indresp.payn )/4.0; -- net_rate_of_pay_per_month_last_payment
      -- FIXME FIXME FIXME
      mad.current_income( WSC_Enums.gross_self_employment ) := B_Safe_Add( ad.indresp.jspayg, ad.indresp.jsprof )/WEEKS_PER_MONTH; -- Monthly self employed gross pay; Monthly self employed profit
      -- mad.annual_income( WSC_Enums.gross_self_employment ) := B_Safe_Add( Amount( ad.indresp.jsprf ), ad.indresp.jspayu ); -- S/emp: net profit in last yearly account; S/emp: Average earnings last 12 months
      -- jspayw
      -- jsprls --  S/emp accounts: whether profit or loss 
      -- mad.annual_income( WSC_Enums.net_self_employment ) := B_Safe_Add( Amount( ad.indresp.jsprf )); -- S/emp: net profit in last yearly account; 
      
      for i in 1 .. ad.num_incomes loop
         Log( "on income type " & ad.incomes( i ).ficode'Img );
         a := Make_Annual_And_Current( ad.incomes( i )); 
         case ad.incomes( i ).ficode is
            when BHPS_Enums.ni_retir_pension => 
               mad.current_income( WSC_Enums.ni_retir_pension ) := a( 1 );
               mad.annual_income( WSC_Enums.ni_retir_pension ) := a( 2 );
            when BHPS_Enums.sev_disabl_allow => 
               mad.current_income( WSC_Enums.sev_disabl_allow ) := a( 1 );
               mad.annual_income( WSC_Enums.sev_disabl_allow ) := a( 2 );
            when BHPS_Enums.invalidity_pens => 
               mad.current_income( WSC_Enums.invalidity_pens ) := a( 1 );
               mad.annual_income( WSC_Enums.invalidity_pens ) := a( 2 );
            when BHPS_Enums.ind_injury_allow => mad.current_income( WSC_Enums.ind_injury_allow ) := a( 1 );mad.annual_income( WSC_Enums.ind_injury_allow ) := a( 2 );
            when BHPS_Enums.attendance_allow => mad.current_income( WSC_Enums.attendance_allow ) := a( 1 );mad.annual_income( WSC_Enums.attendance_allow ) := a( 2 );
            when BHPS_Enums.pension_prev_emp => mad.current_income( WSC_Enums.pension_prev_emp ) := a( 1 );mad.annual_income( WSC_Enums.pension_prev_emp ) := a( 2 );
            when BHPS_Enums.mobility_allow => mad.current_income( WSC_Enums.mobility_allow ) := a( 1 );mad.annual_income( WSC_Enums.mobility_allow ) := a( 2 );
            when BHPS_Enums.inv_care_allow => mad.current_income( WSC_Enums.inv_care_allow ) := a( 1 );mad.annual_income( WSC_Enums.inv_care_allow ) := a( 2 );
            when BHPS_Enums.war_disabl_pens => mad.current_income( WSC_Enums.war_disabl_pens ) := a( 1 );mad.annual_income( WSC_Enums.war_disabl_pens ) := a( 2 );
            when BHPS_Enums.disab_livng_allwnce => mad.current_income( WSC_Enums.disab_livng_allwnce ) := a( 1 );mad.annual_income( WSC_Enums.disab_livng_allwnce ) := a( 2 );
            when BHPS_Enums.disab_wrkng_allwnce => mad.current_income( WSC_Enums.disab_wrkng_allwnce ) := a( 1 );mad.annual_income( WSC_Enums.disab_wrkng_allwnce ) := a( 2 );
            when BHPS_Enums.incapacity_benefit => mad.current_income( WSC_Enums.incapacity_benefit ) := a( 1 );mad.annual_income( WSC_Enums.incapacity_benefit ) := a( 2 );
            when BHPS_Enums.disab_liv_allowcare => mad.current_income( WSC_Enums.disab_liv_allowcare ) := a( 1 );mad.annual_income( WSC_Enums.disab_liv_allowcare ) := a( 2 );
            when BHPS_Enums.disab_liv_allowmob => mad.current_income( WSC_Enums.disab_liv_allowmob ) := a( 1 );mad.annual_income( WSC_Enums.disab_liv_allowmob ) := a( 2 );
            when BHPS_Enums.disab_liv_allow_dk => mad.current_income( WSC_Enums.disab_liv_allow_dk ) := a( 1 );mad.annual_income( WSC_Enums.disab_liv_allow_dk ) := a( 2 );
            when BHPS_Enums.pens_spse_prev_emp => mad.current_income( WSC_Enums.pens_spse_prev_emp ) := a( 1 );mad.annual_income( WSC_Enums.pens_spse_prev_emp ) := a( 2 );
            when BHPS_Enums.unempl_or_incme_supt => mad.current_income( WSC_Enums.unempl_or_incme_supt ) := a( 1 );mad.annual_income( WSC_Enums.unempl_or_incme_supt ) := a( 2 );
            when BHPS_Enums.income_support => mad.current_income( WSC_Enums.income_support ) := a( 1 );mad.annual_income( WSC_Enums.income_support ) := a( 2 );
            when BHPS_Enums.unempl_benefit => mad.current_income( WSC_Enums.unempl_benefit ) := a( 1 );mad.annual_income( WSC_Enums.unempl_benefit ) := a( 2 );
            when BHPS_Enums.ni_sick_benefit => mad.current_income( WSC_Enums.ni_sick_benefit ) := a( 1 );mad.annual_income( WSC_Enums.ni_sick_benefit ) := a( 2 );
            when BHPS_Enums.child_benefit => mad.current_income( WSC_Enums.child_benefit ) := a( 1 );mad.annual_income( WSC_Enums.child_benefit ) := a( 2 );
            when BHPS_Enums.one_parent_benefit => mad.current_income( WSC_Enums.one_parent_benefit ) := a( 1 );mad.annual_income( WSC_Enums.one_parent_benefit ) := a( 2 );
            when BHPS_Enums.family_credit => mad.current_income( WSC_Enums.family_credit ) := a( 1 );mad.annual_income( WSC_Enums.family_credit ) := a( 2 );
            when BHPS_Enums.maternity_allow => mad.current_income( WSC_Enums.maternity_allow ) := a( 1 );mad.annual_income( WSC_Enums.maternity_allow ) := a( 2 );
            when BHPS_Enums.housing_benefit => mad.current_income( WSC_Enums.housing_benefit ) := a( 1 );mad.annual_income( WSC_Enums.housing_benefit ) := a( 2 );
            when BHPS_Enums.annuity_or_priv_pens => mad.current_income( WSC_Enums.annuity_or_priv_pens ) := a( 1 );mad.annual_income( WSC_Enums.annuity_or_priv_pens ) := a( 2 );
            when BHPS_Enums.comm_charge_bene => mad.current_income( WSC_Enums.comm_charge_bene ) := a( 1 );mad.annual_income( WSC_Enums.comm_charge_bene ) := a( 2 );
            when BHPS_Enums.other_state_bene => mad.current_income( WSC_Enums.other_state_bene ) := a( 1 );mad.annual_income( WSC_Enums.other_state_bene ) := a( 2 );
            when BHPS_Enums.job_seekers_allow => mad.current_income( WSC_Enums.job_seekers_allow ) := a( 1 );mad.annual_income( WSC_Enums.job_seekers_allow ) := a( 2 );
            when BHPS_Enums.child_tax_credit => mad.current_income( WSC_Enums.child_tax_credit ) := a( 1 );mad.annual_income( WSC_Enums.child_tax_credit ) := a( 2 );
            when BHPS_Enums.return_to_work_credit => mad.current_income( WSC_Enums.return_to_work_credit ) := a( 1 );mad.annual_income( WSC_Enums.return_to_work_credit ) := a( 2 );
            when BHPS_Enums.widow_or_war_pens => mad.current_income( WSC_Enums.widow_or_war_pens ) := a( 1 );mad.annual_income( WSC_Enums.widow_or_war_pens ) := a( 2 );
            when BHPS_Enums.educa_grant => mad.current_income( WSC_Enums.educa_grant ) := a( 1 );mad.annual_income( WSC_Enums.educa_grant ) := a( 2 );
            when BHPS_Enums.t_u_or_friendly_soc_payt => mad.current_income( WSC_Enums.t_u_or_friendly_soc_payt ) := a( 1 );mad.annual_income( WSC_Enums.t_u_or_friendly_soc_payt ) := a( 2 );
            when BHPS_Enums.maint_or_alimony => mad.current_income( WSC_Enums.maint_or_alimony ) := a( 1 );mad.annual_income( WSC_Enums.maint_or_alimony ) := a( 2 );
            when BHPS_Enums.payment_abs_relative => mad.current_income( WSC_Enums.payment_abs_relative ) := a( 1 );mad.annual_income( WSC_Enums.payment_abs_relative ) := a( 2 );
            when BHPS_Enums.rent_or_boarders_or_lodgers => mad.current_income( WSC_Enums.rent_or_boarders_or_lodgers ) := a( 1 );mad.annual_income( WSC_Enums.rent_or_boarders_or_lodgers ) := a( 2 );
            when BHPS_Enums.rent_other_prop => mad.current_income( WSC_Enums.rent_other_prop ) := a( 1 );mad.annual_income( WSC_Enums.rent_other_prop ) := a( 2 );
            when BHPS_Enums.foster_allowance => mad.current_income( WSC_Enums.foster_allowance ) := a( 1 );mad.annual_income( WSC_Enums.foster_allowance ) := a( 2 );
            when BHPS_Enums.sick_or_acci_insurance => mad.current_income( WSC_Enums.sick_or_acci_insurance ) := a( 1 );mad.annual_income( WSC_Enums.sick_or_acci_insurance ) := a( 2 );
            when BHPS_Enums.any_other_payment => 
               mad.current_income( WSC_Enums.any_other_payment ) := a( 1 );
               mad.annual_income( WSC_Enums.any_other_payment ) := a( 2 );
            when BHPS_Enums.wid_mothr_allow => 
               mad.current_income( WSC_Enums.wid_mothr_allow ) := a( 1 );
               mad.annual_income( WSC_Enums.wid_mothr_allow ) := a( 2 );
            when BHPS_Enums.pension_credit => 
               mad.current_income( WSC_Enums.pension_credit ) := a( 1 );
               mad.annual_income( WSC_Enums.pension_credit ) := a( 2 );
            
         end case;
      end loop;
   end Create_Incomes;
   
   function Wave_From_Char( c : Character ) return WSC_Enums.Waves is
      s : String( 1 .. 1 );
   begin
      s( 1 ) := c;
      return WSC_Enums.Waves'Value( s );
   end Wave_From_Char;
   
   function Make_Diff_And_Help( aa : Adla_Type; ad : Adlad_Type ) return wscm.Diff_And_Help is
      dh : wscm.Diff_And_Help;
   begin
      case aa is
         when dont_know |
              refused |
              proxy_and_or_phone |
              not_applicable |
              missing |
              by_self => dh.help := WSC_Enums.by_self;
         when with_help_from_someone_else => dh.help := WSC_Enums.with_help_from_someone_else;
         when not_at_all => dh.help := WSC_Enums.not_at_all;
      end case;
      case ad is
         when 
            dont_know |
            refused |
            proxy_and_or_phone |
            not_applicable |
            missing |
            very_easy => dh.diff := WSC_Enums.very_easy;
         when fairly_easy => dh.diff := WSC_Enums.fairly_easy;
         when fairly_difficult => dh.diff := WSC_Enums.fairly_difficult;
         when very_difficult => dh.diff := WSC_Enums.very_difficult;
      end case;
      return dh;
   end Make_Diff_And_Help;
   
   procedure Create_Person_Details( ad : BHPS.Adult; mad : in out wscm.Person; wave : BHPS_Indexes.Waves ) is
   begin
      mad.pid := -9;
      if( ad.indresp.pid > 0 )then
         mad.pid := WSC_Enums.Sernum_Value( ad.indresp.pid );
      elsif( ad.indsamp.pid > 0 )then
         mad.pid := WSC_Enums.Sernum_Value( ad.indsamp.pid );
      end if;
      mad.age := ad.age;
      mad.pno := ad.pno;
      mad.wave := Wave_From_Char( wave );
      mad.has_full_sample := ad.indresp.pno > 0;
      mad.years_in_residential_care := -1;
      mad.regressors := ( others => 0.0 );
      if( ad.indall.hgsex = male )then
         mad.sex := male;
      elsif( ad.indall.hgsex = female )then
         mad.sex := female;
      else
         if( mad.pno mod 2 = 0 )then
            mad.sex := male;
         else
            mad.sex := female;
         end if;
         Put_Line( "no sex recorded for indall " & ad.indall.hid'Img & " pid " & ad.indall.pid'Img & " ad.indall.hgsex " & ad.indall.hgsex'Img & " sex assigned " & mad.sex'Img );
      end if;

      mad.respondent_weights( basic ) := ad.indall.xrwght;
      mad.enumeration_weights( basic ) := ad.indall.xewght;
      
      case wave is
         when 'a' .. 'h' => 
            null;
         when 'i' =>
            mad.enumeration_weights( extended_1 ) := ad.indall.xewtsw1;
            mad.enumeration_weights( extended_2 ) := ad.indall.xewtsw2;
            mad.respondent_weights( extended_1 ) := ad.indall.xewtsw1;
            mad.respondent_weights( extended_2 ) := ad.indall.xewtsw2;
         when 'j' .. 'r' =>
            mad.enumeration_weights( extended_1 ) := ad.indall.xewtuk1;
            mad.enumeration_weights( extended_2 ) := ad.indall.xewtuk2;
            mad.respondent_weights( extended_1 ) := ad.indall.xewtuk1;
            mad.respondent_weights( extended_2 ) := ad.indall.xewtuk2;
         when 's' .. 'z' => null;
      end case;
      -- TODO HOURS WORKED      
      case ad.indresp.jbstat is
         when
            dont_know        |
            refused          |
            proxy_respondent |
            inapplicable     |
            missing_or_wild  => mad.employment_status := na;
         when something_else => mad.employment_status := WSC_Enums.something_else;
         when self_employed => mad.employment_status :=  WSC_Enums.self_employed;
         when in_paid_employ => mad.employment_status := WSC_Enums.in_paid_employ;
         when unemployed => mad.employment_status := WSC_Enums.unemployed;
         when retired => mad.employment_status := WSC_Enums.retired;
         when family_care => mad.employment_status := WSC_Enums.family_care;
         when ft_student => mad.employment_status := WSC_Enums.ft_student;
         when long_term_sick_or_disabled => mad.employment_status := WSC_Enums.long_term_sick_or_disabled;
         when on_matern_leave => mad.employment_status := WSC_Enums.on_matern_leave;
         when govt_trng_scheme => mad.employment_status := WSC_Enums.govt_trng_scheme;
      end case;

      case ad.indresp.qfedhi is      
         when proxy_respondent |
              proxy |
              missing =>mad.highest_qualification := na;
         when no_qf => mad.highest_qualification := no_qf;
         when higher_degree => mad.highest_qualification := higher_degree;
         when first_degree => mad.highest_qualification := first_degree;
         when teaching_qf => mad.highest_qualification := teaching_qf;
         when other_higher_qf => mad.highest_qualification := other_higher_qf;
         when nursing_qf => mad.highest_qualification := nursing_qf;
         when gce_a_levels => mad.highest_qualification := gce_a_levels;
         when gce_o_levels_or_equiv => mad.highest_qualification := gce_o_levels_or_equiv;
         when commercial_qf_no_o_levels => mad.highest_qualification := commercial_qf_no_o_levels;
         when cse_grade_2_5_scot_grade_4_5 => mad.highest_qualification := cse_grade_2_5_scot_grade_4_5;
         when apprenticeship => mad.highest_qualification := apprenticeship;
         when other_qf => mad.highest_qualification := other_qf;
         when still_at_school_no_qf => mad.highest_qualification := still_at_school_no_qf;
      end case;
      
      case ad.indresp.mlstat is
         when BHPS_Enums.married => mad.marital_status := WSC_Enums.married;
         when BHPS_Enums.separated => mad.marital_status := WSC_Enums.separated;
         when BHPS_Enums.divorced => mad.marital_status := WSC_Enums.divorced;
         when BHPS_Enums.widowed => mad.marital_status := WSC_Enums.widowed;
         when BHPS_Enums.never_married => mad.marital_status := WSC_Enums.never_married;
         when BHPS_Enums.in_a_civil_partnership => mad.marital_status := WSC_Enums.in_a_civil_partnership;
         when BHPS_Enums.have_a_dissolved_civil_partnership => mad.marital_status := WSC_Enums.have_a_dissolved_civil_partnership;
         when BHPS_Enums.separated_from_a_civil_partnership => mad.marital_status := WSC_Enums.separated_from_a_civil_partnership;
         when BHPS_Enums.surviving_partner_of_a_civil_partnership => mad.marital_status := WSC_Enums.surviving_partner_of_a_civil_partnership;      
         when others => mad.marital_status := na;
      end case;
      
      case ad.indresp.ivlpar is
         when BHPS_Enums.married_and_with_spouse => mad.partner_status := WSC_Enums.married_and_with_spouse;
         when BHPS_Enums.cohabiting => mad.partner_status := WSC_Enums.cohabiting;
         when BHPS_Enums.in_civil_partnership => mad.partner_status := WSC_Enums.in_civil_partnership;
         when others => mad.partner_status := neither;
      end case;      

      case ad.indresp.hlstat is
         when BHPS_Enums.dont_know |
            BHPS_Enums.refused |
            BHPS_Enums.proxy_and_or_phone |
            BHPS_Enums.inapplicable |
            BHPS_Enums.missing_or_wild => mad.health_status := WSC_Enums.na;
         when BHPS_Enums.excellent => mad.health_status := WSC_Enums.excellent;
         when BHPS_Enums.good => mad.health_status := WSC_Enums.good;
         when BHPS_Enums.fair => mad.health_status := WSC_Enums.fair;
         when BHPS_Enums.poor => mad.health_status := WSC_Enums.poor;
         when BHPS_Enums.very_poor => mad.health_status := WSC_Enums.very_poor;
      end case;
      
      mad.is_disabled := ad.indresp.hldsbl = yes;

      mad.fitness( WSC_Enums.manage_stairs ) := Make_Diff_And_Help( ad.indresp.adla, ad.indresp.adlad );
      mad.fitness( WSC_Enums.get_around_house ) := Make_Diff_And_Help( ad.indresp.adlb, ad.indresp.adlbd );
      mad.fitness( WSC_Enums.get_in_or_out_of_bed ) := Make_Diff_And_Help( ad.indresp.adlc, ad.indresp.adlcd );
      mad.fitness( WSC_Enums.cut_toenails ) := Make_Diff_And_Help( ad.indresp.adld, ad.indresp.adldd );
      mad.fitness( WSC_Enums.bathing_or_showering ) := Make_Diff_And_Help( ad.indresp.adle, ad.indresp.adled );
      mad.fitness( WSC_Enums.walk_down_road  ) := Make_Diff_And_Help( ad.indresp.adlf, ad.indresp.adlfd );  
      mad.hours_of_care_given := wscm.Hours_Count( Map_Aidhrs( ad.indresp.aidhrs ));
      
   end Create_Person_Details;
   
   -- where a month is 1/12 of a year
   function To_Weekly( a : Amount; weeks : Amount ) return Amount is
   begin
      if( a = -9.0 ) or ( a = -8.0 ) or ( a = -3.0 )then
         return 0.0;
      end if;
      if( weeks > 0.0 )then
         return a * 4.0/weeks;
      end if;
      return a;
   end To_Weekly;
   
   procedure Create_Person( ad : BHPS.Adult; mad : in out wscm.Person; wave : BHPS_Indexes.Waves ) is
   begin
      Create_Person_Details( ad, mad, wave );
      Create_Incomes( ad, mad );
      Impute_Benefit_Levels( mad ); 
   end Create_Person;

   procedure Impute_Care_Amounts( 
      hh                   : in out wscm.Household; 
      hh_last_period       : wscm.Household;
      add_random_component : Boolean ) is
   use Model.WSC.Household.Regressions;
   use Model.WSC.Household;
   begin
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 ..  hh.benefit_units( buno ).num_adults loop
            declare
               bu                  : wscm.Benefit_Unit renames hh.benefit_units( buno );
               ad                  : Person renames bu.adults( adno );
               ad_last_period      : Person renames hh_last_period.benefit_units( buno ).adults( adno );
               is_couple           : Boolean := bu.position_of_spouse > 0 and ( bu.position_of_head = ad.pno or bu.position_of_spouse = ad.pno );
               pr                  : Amount;
               rr                  : Regression_Results;
               use_lagged_dep_vars : constant Boolean := ad.age >= 65;
            begin
               if( hh.hdata.region = wales )then
                  if( ad.age >= 65 )then
                     --
                     --
                     pr :=  Informal_Care_From_Householder_Probit( ad, ad_last_period, hh.hdata.region, use_lagged_dep_vars );
                     Log( "prob informal care is " & Format( pr ));
                     ad.receives_informal_care_from_household_member := 
                        Evaluate_Probit( pr, add_random_component );
                     if( ad.receives_informal_care_from_household_member )then
                        rr :=  Hours_Of_Care_Regression( ad, hh.hdata.region );   
                        if( add_random_component )then
                           rr.vp := rr.vp + Random_Normal_Generator.Draw( 
                              mean => 0.0, 
                              standard_deviation => rr.sd );
                        end if;
                        Log( "got predicted hours as " & Format( rr.vp ));
                        if( rr.vp < 0.0 )then
                           ad.hours_of_care_recieved := 0;
                        elsif( rr.vp > Amount( Hours_Count'Last ))then
                           ad.hours_of_care_recieved := wscm.Hours_Count'Last; 
                        else
                           ad.hours_of_care_recieved := wscm.Hours_Count( rr.vp );
                        end if;
                     end if;
                     Recalculate_Regressors( ad, hh.Num_Adults, hh.Num_Children, hh.hdata );
                     pr :=  Informal_Care_From_Non_Householder_Probit( ad, ad_last_period, hh.hdata.wave );
                     ad.receives_informal_care_from_non_householder  := 
                        Evaluate_Probit( pr, add_random_component );
                     Recalculate_Regressors( ad, hh.Num_Adults, hh.Num_Children, hh.hdata );
                  end if;
               end if;
            end;
         end loop;
      end loop;
   end Impute_Care_Amounts;

   
   procedure Infer_Wealth( 
      hh                    : in out wscm.Household;
      add_random_component  : Boolean ) is
   use Model.WSC.Household.Regressions;
   use Model.WSC.Household;
   begin
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 ..  hh.benefit_units( buno ).num_adults loop
            declare
               bu                     : wscm.Benefit_Unit renames hh.benefit_units( buno );
               ad                     : Person renames bu.adults( adno );
               has_significant_wealth : Boolean;
               pr                     : Amount;
               rr                  : Regression_Results;
             begin
               pr := Has_Wealth_Probit( ad );
               has_significant_wealth := Evaluate_Probit( pr, add_random_component );
               Log( "has significant wealth; pr = " & pr'Img & " has_significant_wealth " & has_significant_wealth'Img );
               if has_significant_wealth then
                  rr :=  Log_Wealth_Regression( ad, hh.hdata.region );
                  if( add_random_component )then
                     rr.vp := rr.vp + Random_Normal_Generator.Draw( 
                        mean => 0.0, 
                        standard_deviation => rr.sd );
                  end if;
                  ad.personal_wealth := Exp( rr.vp );
                  Log( "ad.personal_wealth " & ad.personal_wealth'Img );
               end if;
             end;
          end loop;
       end loop;
    end Infer_Wealth;
    
   
   function Make_Interview_Date( hresp : Hhresp_Rec; wave : BHPS_Indexes.Waves ) return Ada.Calendar.Time is
   use Ada.Calendar;
      year : Year_Number;
      m : Month_Number := Month_Number'First;
      d : Day_Number := Day_Number'First;
   begin
      case hresp.hhmoi is
         when proxy_and_or_phone | inapplicable | missing_or_wild =>
            null;
         when  january =>
            m := 1;
         when  october =>
            m := 10;
         when  november =>
            m := 11;
         when  december =>
            m := 12;
         when  february =>
            m := 2;
         when  march =>
            m := 3;
         when  april =>
            m := 4;
         when  may =>
            m := 5;
         when  june =>
            m := 6;
         when  july =>
            m := 7;
         when  august =>
            m := 8;
         when  september =>
            m := 9;
      end case;
      Log( "wave |" & wave & "| Time_Of Y: " & hresp.hhyoi4'Img &
         " m " & m'Img & "hresp.hhdoi " & hresp.hhdoi'Img );
      if( wave > 'a' )then
         year := hresp.hhyoi4;
      else
         year := 1991;
      end if;
      if( hresp.hhdoi  < 0 )then
         d := 1;
      else
         d := hresp.hhdoi;
      end if;
      return Time_Of( year, m, d );
   end Make_Interview_Date;
   
   function Failed_HH_Interview( hresp : Hhresp_Rec; wave : BHPS_Indexes.Waves ) return Boolean is
      miss : Boolean;
   begin
      case hresp.ivfho is  
      when 
         inapplicable |
         missing_or_wild |
         demolished_or_derelict |
         docs_missing_or_unusable |
         business_use_only |
         refusal_to_hq |
         all_refus_to_inter |
         lang_probs_no_interp |
         too_old_or_infirm |
         no_hh_member_contact |
         temp_accomm_only |
         coversheet_only |
         intervw_refusl_to_hq |
         intvw_refusl_to_intr |
         lang_probs_no_enumn |
         age_or_health_no_enumn |
         docs_missing_or_unus |
         empty_at_first_call |
         only_business_prem |
         out_scope_institn |
         movd_back_to_w1_hh_or_add |
         no_psms_resident |
         adamant_refusal |
         building_not_complete |
         address_not_found |
         institut_no_privt_hh |
         refus_to_rsrch_cntre |
         refusal_to_intviewer |
         language_problems |
         no_intv_age_or_health |
         missing_person_ref |
         moved_prev_wave_add |
         out_scope_institutn |
         moved_out_of_scope |
         whole_hh_deceased |
         lt_untraced_withdrawn |
         new_sample_withdrawn |
         hh_composition_only |
         hh_comp_plus_ques_only |
         youth_interview_only | 
         interviews_plus_refusals |
         phone_pxy_intvw_only |
         withdrawn_before_field => miss := True;
      when 
         pxy_w1_or_2_mem_add_sme |
         all_eligible_hh_intervd |
         interviews_plus_proxies |
         pxy_at_original_addr => miss := False;
      end case;
      if( hresp.hid < 0 )then -- no HID
         miss := True;
      end if;
      if( hresp.hhyoi4 < 0 and wave > 'a' )then -- no date
         miss := True;
      end if;
      return miss;
   end  Failed_HH_Interview;
      
   procedure Create_Household( 
      hh                          : BHPS.Household_Access; 
      mhh                         : in out  wscm.Household; 
      num_non_empty_benefit_units : out Natural;
      someone_missing             : out Boolean;      
      wave                        : BHPS_Indexes.Waves ) is
   use BHPS;
   use BHPS_Indexes;
      hresp      : Hhresp_Rec := hh.Hhresp;
   begin
      Log( "On Household " & hh.hhsamp.hid'Img & " hresp " & hh.hhresp.hid'Img );
      Log( "Response code " & hh.hhresp.ivfho'Img );
      mhh.wave := Wave_From_Char( wave );
      mhh.hdata.wave := mhh.wave;
      num_non_empty_benefit_units := 0;
      someone_missing := False;
      if( Failed_HH_Interview( hresp, wave ) or hresp.hid < 0 )then
         -- if( hh.hresp.hhdoi < 0 and hh.hhyoi4 < 0 )then  
         -- FIXME non-responsive hhld: get a better test for this,
         Log( "returning with non-response hh " & hh.hhsamp.hid'Img );
         mhh.hdata.has_full_sample := False;
         return;
      end if;
      mhh.hdata.interview_date := Make_Interview_Date( hresp, wave );
      mhh.hdata.current_simulated_date := mhh.hdata.interview_date;
      mhh.hid := WSC_Enums.Sernum_Value( hresp.hid );
      mhh.hdata.hid := mhh.hid;
      mhh.hdata.origin_hid := mhh.hid;      
      mhh.hdata.has_full_sample := True;
      case hresp.tenure is
         when BHPS_Enums.proxy_and_or_phone | BHPS_Enums.missing | BHPS_Enums.owned_outright => 
            mhh.hdata.tenure := WSC_Enums.owned_outright;
         when BHPS_Enums.owned_with_mortgage => mhh.hdata.tenure := WSC_Enums.owned_with_mortgage;
         when BHPS_Enums.local_authority_rented => mhh.hdata.tenure := WSC_Enums.local_authority_rented;
         when BHPS_Enums.housing_assoc_rented => mhh.hdata.tenure := WSC_Enums.housing_assoc_rented;
         when BHPS_Enums.rented_from_employer => mhh.hdata.tenure := WSC_Enums.rented_from_employer;
         when BHPS_Enums.rented_private_unfurnished => mhh.hdata.tenure := WSC_Enums.rented_private_unfurnished;
         when BHPS_Enums.rented_private_furnished => mhh.hdata.tenure := WSC_Enums.rented_private_furnished;
         when BHPS_Enums.other_rented => mhh.hdata.tenure := WSC_Enums.other_rented;
      end case;
      case hresp.hsctax is
         when  BHPS_Enums.dont_know |
               BHPS_Enums.refused |
               BHPS_Enums.proxy_and_or_phone |
               BHPS_Enums.not_applicable |
               BHPS_Enums.missing |
               BHPS_Enums.band_a_up_to_40_000 => mhh.hdata.ct_band := WSC_Enums.band_a;
         when BHPS_Enums.band_b_40_001_52_000 => mhh.hdata.ct_band := WSC_Enums.band_b;
         when BHPS_Enums.band_c_52_001_68_000 => mhh.hdata.ct_band := WSC_Enums.band_c;
         when BHPS_Enums.band_d_68_001_88_000 => mhh.hdata.ct_band := WSC_Enums.band_d;
         when BHPS_Enums.band_e_88_001_120_000 => mhh.hdata.ct_band := WSC_Enums.band_e;
         when BHPS_Enums.band_f_120_001_160_000 => mhh.hdata.ct_band := WSC_Enums.band_f;
         when BHPS_Enums.band_g_160_001_320_000 => mhh.hdata.ct_band := WSC_Enums.band_g;
         when BHPS_Enums.band_h_320_001_plus => mhh.hdata.ct_band := WSC_Enums.band_h;
         when BHPS_Enums.hhold_accom_not_valued_separately => mhh.hdata.ct_band := WSC_Enums.not_valued_separately;
         when BHPS_Enums.na_northern_ireland => mhh.hdata.ct_band := WSC_Enums.northern_ireland;
      end case;
      mhh.hdata.mortgage_outstanding := B_Safe_Add( hresp.mgtot ); -- :-- total_mortgage_on_all_property : Total mortgage on all property 
      mhh.hdata.gross_rent := To_Weekly( hresp.rentg, hresp.rentgw );
      mhh.hdata.years_outstanding_on_mortgage := Natural'Max( 0, hresp.mglife );
      mhh.hdata.net_rent := To_Weekly( hresp.rent, hresp.rentgw );
      -- FIXME rentgw : Amount := MISS_R; -- weeks_covered_by_gross_rent : Weeks covered by gross rent 
      mhh.hdata.gross_housing_costs := B_Safe_Add( hresp.xphsg )/WEEKS_PER_MONTH;
      mhh.hdata.net_housing_costs   := B_Safe_Add( hresp.xphsn )/WEEKS_PER_MONTH;-- net_monthly_housing_costs : Net monthly housing costs 
      mhh.hdata.mortgage_payment    := B_Safe_Add( hresp.xpmg )/WEEKS_PER_MONTH;
      case hresp.region is
         when 
            BHPS_Enums.inapplicable |
            BHPS_Enums.missing |
            BHPS_Enums.inner_london => mhh.hdata.region := WSC_Enums.inner_london;
         when BHPS_Enums.outer_london => mhh.hdata.region := WSC_Enums.outer_london;
         when BHPS_Enums.r_of_south_east => mhh.hdata.region := WSC_Enums.r_of_south_east;
         when BHPS_Enums.south_west => mhh.hdata.region := WSC_Enums.south_west;
         when BHPS_Enums.east_anglia => mhh.hdata.region := WSC_Enums.east_anglia;
         when BHPS_Enums.east_midlands => mhh.hdata.region := WSC_Enums.east_midlands;
         when BHPS_Enums.west_midlands_conurbation => mhh.hdata.region := WSC_Enums.west_midlands_conurbation;
         when BHPS_Enums.r_of_west_midlands => mhh.hdata.region := WSC_Enums.r_of_west_midlands;
         when BHPS_Enums.greater_manchester => mhh.hdata.region := WSC_Enums.greater_manchester;
         when BHPS_Enums.merseyside => mhh.hdata.region := WSC_Enums.merseyside;
         when BHPS_Enums.r_of_north_west => mhh.hdata.region := WSC_Enums.r_of_north_west;
         when BHPS_Enums.south_yorkshire => mhh.hdata.region := WSC_Enums.south_yorkshire;
         when BHPS_Enums.west_yorkshire => mhh.hdata.region := WSC_Enums.west_yorkshire;
         when BHPS_Enums.r_of_yorks_and_humberside => mhh.hdata.region := WSC_Enums.r_of_yorks_and_humberside;
         when BHPS_Enums.tyne_and_wear => mhh.hdata.region := WSC_Enums.tyne_and_wear;
         when BHPS_Enums.r_of_north => mhh.hdata.region := WSC_Enums.r_of_north;
         when BHPS_Enums.wales => mhh.hdata.region := WSC_Enums.wales;
         when BHPS_Enums.scotland => mhh.hdata.region := WSC_Enums.scotland;
         when BHPS_Enums.northern_ireland => mhh.hdata.region := WSC_Enums.northern_ireland;
      end case;
      mhh.hdata.weights( basic ) := hresp.hhwght;
      case wave is
         when 'a' .. 'h' => 
            null;
         when 'i' =>
            mhh.hdata.weights( extended_1 ) := hresp.xhwtsw1;
            mhh.hdata.weights( extended_2 ) := hresp.xhwtsw2;
         when 'j' .. 'r' =>
            mhh.hdata.weights( extended_1 ) := hresp.xhwtuk1;
            mhh.hdata.weights( extended_2 ) := hresp.xhwtuk2;
         when 's' .. 'z' => null;
      end case;
      
      mhh.hdata.house_value := B_Safe_Add( hresp.hsval );
      mhh.hdata.years_outstanding_on_mortgage := Natural'Max( 0, hresp.mglife );
      
      mhh.hdata.other_property_value := B_Safe_Add( hresp.hs2valo );
      mhh.hdata.total_income := B_Safe_Add( hresp.fihhmn )/WEEKS_PER_MONTH;  -- household_income_month_before_interview : Household income: month before interview
      Log( "mhh.num_benefit_units " & mhh.num_benefit_units'Img );
      raw_data_ben_units:
      for b in 1 .. hh.Num_Benefit_Units loop
         declare
            bu           : BHPS.Benefit_Unit := Get_Benefit_Unit( hh, b );
            ad_head      : BHPS.Adult := bu.Get_Head_Or_Spouse( head );
            ad_spouse    : BHPS.Adult := bu.Get_Head_Or_Spouse( spouse );
            num_people   : Natural := 0;
            num_adults   : Natural := 0;
            num_children : Natural := 0;
         begin
            Log( "got head pno as " &  ad_head.indall.pno'Img );
            adults_in_bu:
            for adno in 1 .. bu.Num_With_Adult_Records loop
               declare
                  ad  : BHPS.Adult := bu.Get_With_Adult_Records( adno );
                  mad : wscm.Person;
               begin
                  if( ad.indresp.pid > 0 )then -- has full response
                     Log( "on bhps adult no " & ad.pno'Img & " pid " & ad.pid'Img );
                     num_adults := num_adults + 1; 
                     if( num_adults = 1 )then -- found someone for this bu
                        num_non_empty_benefit_units := num_non_empty_benefit_units + 1;
                     end if;
                     Log( "num adults " & num_adults'Img & " num_non_empty_benefit_units " & num_non_empty_benefit_units'Img );
                     Create_Person( ad, mad, wave );
                     mad.hid := mhh.hdata.hid; -- so we can lookup containing household
                     mhh.benefit_units( num_non_empty_benefit_units ).adults( num_adults ) := mad;   
                     if( ad.pid = ad_head.pid )then
                        mad.hdsp := head;    
                        mhh.benefit_units( num_non_empty_benefit_units ).position_of_head := num_adults;
                     elsif( ad.pid = ad_spouse.pid )then
                        mad.hdsp := spouse;    
                        mhh.benefit_units( num_non_empty_benefit_units ).position_of_spouse := num_adults;
                     end if;
                  else
                     someone_missing := True;
                     Log( "ad.indresp.pid < 0 for adno " & adno'Img & "ad.pno " & ad.pno'Img );
                  end if;
               end;
            end loop adults_in_bu;
            children_in_bu:
            for chno in 1 .. bu.Num_Children loop
               declare
                  ad :BHPS.Adult := bu.Get_Child( chno );
                  mch : wscm.Person;
               begin
                  if( ad.indsamp.pid > 0 )then
                     num_children := num_children + 1;
                     if(( num_adults + num_children ) = 1 )then -- found someone for this bu
                        num_non_empty_benefit_units := num_non_empty_benefit_units + 1;
                     end if;
                     Create_Person( ad, mch, wave );
                     mhh.benefit_units( num_non_empty_benefit_units ).children( num_children ) := mch;
                  else
                     someone_missing := True;
                  end if;
               end;
            end loop children_in_bu;
            num_people := num_adults + num_children;
            if( num_non_empty_benefit_units > 0 ) and ( num_people > 0 ) then
               mhh.benefit_units( num_non_empty_benefit_units ).num_people := num_people;
               mhh.benefit_units( num_non_empty_benefit_units ).num_adults := num_adults;
               mhh.benefit_units( num_non_empty_benefit_units ).num_children := num_children;
            end if;
         end;
      end loop raw_data_ben_units;
      mhh.num_benefit_units := num_non_empty_benefit_units;

      for buno in 1 .. mhh.num_benefit_units loop
         for adno in 1 .. mhh.benefit_units( buno ).num_adults loop
            wscm.Recalculate_Regressors( 
               mhh.benefit_units( buno ).adults( adno ), 
               mhh.Num_Adults, 
               mhh.Num_Children, 
               mhh.hdata );                  
         end loop;
         for chno in 1 .. mhh.benefit_units( buno ).num_children loop
            wscm.Recalculate_Regressors( 
               mhh.benefit_units( buno ).children( chno ), 
               mhh.Num_Adults, 
               mhh.Num_Children, 
               mhh.hdata );
         end loop;
      end loop;
      Set_Is_Cared_For( hh.all, mhh );
      Infer_Wealth( mhh, False );
      for buno in 1 .. mhh.Num_Benefit_Units loop
         for adno in 1 .. mhh.benefit_units( buno ).num_adults loop
            if( mhh.benefit_units( buno ).adults( adno ).years_in_residential_care > 0 )then
               Log( "Residential Positive" & mhh.To_String );
            end if;
         end loop;
      end loop;
      if( mhh.hid = 18297901 ) or ( mhh.hid = 18300023 ) or ( mhh.hid = 18336788 ) then
         Log( "HH with residential?? " & mhh.To_String );
      end if;
   end Create_Household;

end Model.WSC.BHPS_Data_Creation_Libs;
