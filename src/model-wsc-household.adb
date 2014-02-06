with Ada.Strings.Unbounded;
with Text_Utils;
with Time_Format;
with GNATColl.Traces;
with Ada.Unchecked_Deallocation;

package body Model.WSC.Household is

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.HOUSEHOLD" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   function Make_New_Sernum( wave : Waves; hhno : Households_Per_Wave_Number; n : Natural := 1 ) return Sernum_Value is
   begin
      return Sernum_Value((( Waves'Pos( wave ) + 1 )  * 10_000_000 ) + ( hhno * 10 ) + n );
   end Make_New_Sernum;
   procedure Remove_Empty_Benefit_Units( hh : in out Household ) is
      bus : Benefit_Unit_Array;
      nbus : Benefit_Unit_Count := 0;
   begin
      --
      -- delete benefit units with nobody left
      --
      bus := hh.benefit_units;
      for buno in 1 .. hh.num_benefit_units loop
         if( bus( buno ).num_people > 0 )then
            nbus := nbus + 1;
            hh.benefit_units( nbus ) := bus( buno );
         end if;
      end loop;
      hh.num_benefit_units := nbus;
   end Remove_Empty_Benefit_Units;
   
   function Weight( hh : Household ) return Amount is
   begin
      return hh.hdata.weights( extended_2 ) *  WEIGHT_MULTIPLIER;
   end Weight;

 
   function Age_Of_Oldest_Member( hh : Household ) return Age_Range is
      hage : Age_Range := Age_Range'First;
   begin
      for b in 1 .. hh.num_benefit_units loop
         for a in 1 .. hh.benefit_units( b ).num_adults loop
            if hh.benefit_units( b ).adults( a ).age > hage then
               hage := hh.benefit_units( b ).adults( a ).age;
            end if;               
         end loop;
         for c in 1 .. hh.benefit_units( b ).num_children loop
            if hh.benefit_units( b ).children( c ).age > hage then
               hage := hh.benefit_units( b ).children( c ).age;
            end if;               
         end loop;
      end loop;
      return hage;
   end Age_Of_Oldest_Member;

   

   procedure Delete_Benefit_Unit( hh : in out Household; buno : Benefit_Unit_Number ) is
   begin
      for b in buno .. hh.num_benefit_units - 1 loop
         hh.benefit_units( b ) := hh.benefit_units( b + 1 );
      end loop;
      hh.num_benefit_units := hh.num_benefit_units - 1;
   end Delete_Benefit_Unit;
   
   procedure Delete_Adult( 
      bu     : in out Benefit_Unit;
      pno    : Person_Number;
      reason : Delete_Reason_Type ) is
      change_heads_status : Boolean := False;
      to_delete_from : Person_Count := 0;
   begin
      Log( "Delete_Adult; killing adult " & pno'Img );
      
      if( bu.position_of_head = pno ) or ( bu.position_of_spouse = pno )then
         change_heads_status := true;
         bu.position_of_spouse := 0;
      end if;
      for i in pno .. bu.num_adults - 1 loop
         bu.adults( i ) := bu.adults( i + 1 );
      end loop;
      if( change_heads_status )then
            -- change status of spouse
          Log( "changing marstat for person " &  bu.position_of_head'Img & " reason " & reason'Img );
          case reason is
             when death =>
                if( bu.adults( bu.position_of_head ).marital_status in married .. separated )then
                   bu.adults( bu.position_of_head ).marital_status := widowed;
                elsif( bu.adults( bu.position_of_head ).marital_status = in_a_civil_partnership )then
                   bu.adults( bu.position_of_head ).marital_status := surviving_partner_of_a_civil_partnership;
                end if;
             when separation =>
                if( bu.adults( bu.position_of_head ).marital_status = married )then
                   bu.adults( bu.position_of_head ).marital_status := separated;
                elsif( bu.adults( bu.position_of_head ).marital_status = in_a_civil_partnership )then
                   bu.adults( bu.position_of_head ).marital_status := have_a_dissolved_civil_partnership;
                end if;
          end case;                      
          bu.adults( bu.position_of_head ).partner_status := neither;
      end if;
      bu.num_adults := bu.num_adults - 1;
      bu.num_people := bu.num_adults + bu.num_children;
      Log( "people count now " & bu.num_people'Img );
   end Delete_Adult; 
   
   function Make_Activities_Of_Daily_Living_Score( dh : Diff_And_Help ) return Amount is
      score : Amount := 0.0;
   begin
     case dh.diff is
        when fairly_difficult => score := 1.0;
        when very_difficult =>  score := 3.0;
        when others => null;
     end case;
     if( dh.help = with_help_from_someone_else ) then
        score := 5.0;
     elsif( dh.help = not_at_all ) then 
       score := 10.0;
     end if;
     return score;
   end Make_Activities_Of_Daily_Living_Score;
   
   function Is_Couple( bu : Benefit_Unit ) return Boolean is
   begin
      if( bu.num_adults = 1 )then
         return False;
      end if;
      for adno in 1 .. bu.num_adults loop
         if( bu.adults( adno ).partner_status /= neither )then
            return True;
         end if;
      end loop;
      return False;
   end Is_Couple;
   
   function Which_Incomes_Received( 
      pers          : Person; 
      use_current   : Boolean ) return Income_Package.Set is
      which_incomes : Income_Package.Set; 
   begin
      if( use_current )then
         for i in incomes_Type loop
            if( pers.current_income( i ) /= 0.0 )then
               which_incomes.Insert( i );
            end if;
         end loop;
      else
         for i in incomes_Type loop
            if( pers.current_income( i ) /= 0.0 )then
               which_incomes.Insert( i );
            end if;
         end loop;
      end if;
      return which_incomes;
   end Which_Incomes_Received;
   
   function Which_Incomes_Received( 
      bu            : Benefit_Unit;  
      use_current   : Boolean ) return Income_Package.Set is
      which_incomes : Income_Package.Set; 
   begin
      for adno in 1 .. bu.num_adults loop
         which_incomes.Union( bu.adults( adno ).Which_Incomes_Received( use_current ));
      end loop;
      for chno in 1 .. bu.num_children loop
         which_incomes.Union( bu.children( chno ).Which_Incomes_Received( use_current ));
      end loop;
      return which_incomes;
   end Which_Incomes_Received;
   
   function Which_Incomes_Received( 
      hh            : Household;  
      use_current   : Boolean ) return Income_Package.Set is
      which_incomes : Income_Package.Set; 
   begin
      for buno in 1 .. hh.num_benefit_units loop
         which_incomes.Union( hh.benefit_units( buno ).Which_Incomes_Received( use_current ));
      end loop;
      return which_incomes;
   end Which_Incomes_Received;
   

   --
   -- This creates a complete set of the regressors actually used in the RHS of 
   -- any of the equations. Compared to HR's setupBHPS.do, it omits any forward looking ones (split, retire, etc.)
   -- which appear only on the LHS and can't meaninfully be computed in a backward looking model.
   -- 
   procedure Recalculate_Regressors( 
      pers             : in out Person; 
      num_adults       : Person_Count;
      num_children     : Person_Count;
      hdata            : Household_Data'Class ) is
      r : Regressors_Array renames pers.regressors;
      rage : constant Amount := Amount( pers.age );

   begin
      r := ( others => 0.0 );
      r( const ) := 1.0;
      
      
      declare
         net_income : Amount := Income_Package.Vector_Product( pers.current_income, NET_INCOME_LIST ) * 52.0;
      begin
         if( net_income > 0.0 )then
            r( loginc ) := Log( net_income );
         elsif( net_income < 0.0 )then
            r( neginc ) := 1.0;
         end if;
      end;
      --
      -- housing and wealth
      -- 
      if( num_adults > 0 )then
         declare
            net_housing_per_adult : Amount := ( hdata.house_value - hdata.mortgage_outstanding ) / Amount( num_adults );         
         begin
            if( net_housing_per_adult > 0.0 )then
               r( loghw ) := Log( net_housing_per_adult );
            elsif( net_housing_per_adult < 0.0 )then
               r( neghw ) := 1.0; 
            end if;
         end;
      end if;
      
      if( pers.personal_wealth > 0.0 )then
         r( lognw ) := Log( pers.personal_wealth );
      elsif( pers.personal_wealth < 0.0 )then
         r( negnw ) := 1.0; 
      end if;
      
      r( age ) :=  rage;
      r( age_2 ) := ( rage ** 2 ) / 100.0;
      r( age_3 ) := ( r( age_2 ) * rage ) / 100.0;
      r( age_4 ) := ( r( age_3 ) * rage ) / 100.0;
      r( age_5 ) := ( r( age_4 ) * rage ) / 100.0;
      
      
      
      if( num_children > 0 )then
         r( hhkids ) := 1.0;
      end if;
      
      if( pers.sex = female ) then 
         r( female ) := 1.0; 
      end if;
      if( pers.sex = male ) then 
         r( male ) := 1.0; 
      end if;
      
      if( pers.dies_this_period )then
         r( dead ) := 1.0;
      end if;
      
      log( "Recalculate_Regressors; pid= " & pers.pid'Img & " got health as " &  pers.health_status'Img );
      case pers.health_status is
         when na          => r( hl_m )     := 1.0;
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
      Log( "pid = " & pers.pid'img & " Made r( healthsc  ) as " & Format( r( healthsc )));
      
      case pers.age is
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
      if( pers.age >= 65 )then
         r( age65o ) := 1.0;
      end if;
      if( pers.age = 64 )then
         r( age64 ) := 1.0;
      end if;
      if( pers.age = 59 )then
         r( age59 ) := 1.0;
      elsif( pers.age = 66 )then
         r( age66 ) := 1.0;
      elsif( pers.age = 67 )then
         r( age67 ) := 1.0;
      elsif( pers.age = 68 )then
         r( age68 ) := 1.0;
      elsif( pers.age = 69 )then
         r( age69 ) := 1.0;
      elsif( pers.age = 70 )then
         r( age70 ) := 1.0;
      end if;

      if pers.marital_status = married then
        r( married ) := 1.0; 
      end if;
      if pers.partner_status = married_and_with_spouse or
         pers.partner_status = cohabiting or 
         pers.partner_status = in_civil_partnership then
         r( couple ) := 1.0;
         r( spinhh ) := 1.0;
      -- else
         -- if pers_last_period.partner_status = married_and_with_spouse or
            -- pers_last_period.partner_status = cohabiting or 
            -- pers_last_period.partner_status = in_civil_partnership then
            -- r( seperate ) := 1.0;
         -- end if;
      end if;
 
      
      r( trend )   := Amount( Waves'Pos( pers.wave ) - Waves'Pos( a ));
      r( trend_2 ) := r( trend ) ** 2;
      r( trend_3 ) := r( trend ) ** 3;
      
      if pers.current_income( disab_liv_allowmob ) /= 0.0 then
         r( dlamob ) := 1.0;   
      end if;
      if pers.current_income( disab_liv_allowcare ) /= 0.0 then
         r( dlacare ) := 1.0;   
      end if;
      if pers.current_income( disab_liv_allow_dk ) /= 0.0 then
         r( dladk ) := 1.0;   
      end if;
      if( r( dlamob ) + r( dlacare ) + r( dladk ) > 0.0 )then
         r( dlaany ) := 1.0;
      end if;
      if pers.current_income( attendance_allow ) /= 0.0 then
         r( aa ) := 1.0;
      end if;
      
      case hdata.tenure is
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
      
      case hdata.region is
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
     case hdata.region is
         when inner_london .. r_of_north => r( england ) := 1.0; 
         when wales            => r( wales ) := 1.0;
         when scotland         => r( scotland ) := 1.0;
         when northern_ireland => r( nireland ) := 1.0;
      end case;

      if pers.fitness( manage_stairs ).diff in fairly_difficult .. very_difficult then
         r( adl_a ) := 1.0;
      end if;
      if pers.fitness( get_around_house ).diff in fairly_difficult .. very_difficult then
         r( adl_b ) := 1.0;
      end if;
      if pers.fitness( get_in_or_out_of_bed ).diff in fairly_difficult .. very_difficult then
         r( adl_c ) := 1.0;
      end if;
      if pers.fitness( cut_toenails ).diff in fairly_difficult .. very_difficult then
         r( adl_d ) := 1.0;
      end if;
      if pers.fitness( bathing_or_showering ).diff in fairly_difficult .. very_difficult then
         r( adl_e ) := 1.0;
      end if;
      
      
      r( adlsc_a ) := Make_Activities_Of_Daily_Living_Score( pers.fitness( manage_stairs ));
      r( adlsc_b ) := Make_Activities_Of_Daily_Living_Score( pers.fitness( get_around_house ));
      r( adlsc_c ) := Make_Activities_Of_Daily_Living_Score( pers.fitness( get_in_or_out_of_bed ));
      r( adlsc_d ) := Make_Activities_Of_Daily_Living_Score( pers.fitness( cut_toenails ));
      r( adlsc_e ) := Make_Activities_Of_Daily_Living_Score( pers.fitness( bathing_or_showering ));
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
      case pers.employment_status is
         when in_paid_employ | self_employed => r( working ) := 1.0;
         when retired => r( retired ) := 1.0;
         when others => null;
      end case;
      
      if( pers.is_disabled )then
         r( disabled ) := 1.0;
      end if;
      
      case pers.highest_qualification is 
         when
            higher_degree |
            first_degree |
            teaching_qf |
            other_higher_qf |
            nursing_qf   => r( hq_deg ) := 1.0;
            when gce_a_levels => r( hq_alev ) := 1.0;
            r( q_oth ) := 1.0; -- any qual below degree
         when gce_o_levels_or_equiv => r( hq_gcse ) := 1.0;
            r( q_oth ) := 1.0; -- any qual below degree
         when commercial_qf_no_o_levels |
            cse_grade_2_5_scot_grade_4_5 |
            apprenticeship |
            other_qf => r( hq_oth ) := 1.0;
            r( q_oth ) := 1.0; -- any qual below degree
         when no_qf |
              still_at_school_no_qf => null;
         when others => null;
      end case;
      
      
      if( pers.current_income( income_support ) > 0.0 ) or
        ( pers.current_income( pension_credit ) > 0.0 ) or
        ( pers.current_income( job_seekers_allow ) > 0.0 )then --- NOTE They don't distingiush between contrib and non here.
           r( ispc ) := 1.0;
      end if;
      if( pers.current_income( sev_disabl_allow ) > 0.0 ) or
        ( pers.current_income( invalidity_pens ) > 0.0 ) or
        ( pers.current_income( ind_injury_allow ) > 0.0 ) or 
        ( pers.current_income( disab_livng_allwnce ) > 0.0 ) or
        ( pers.current_income( disab_wrkng_allwnce ) > 0.0 ) or
        ( pers.current_income( incapacity_benefit ) > 0.0 ) or
        ( pers.current_income( disab_liv_allowcare ) > 0.0 ) or
        ( pers.current_income( disab_liv_allowmob ) > 0.0 ) or
        ( pers.current_income( disab_liv_allow_dk ) > 0.0 ) then
           r( disben ) := 1.0;
      end if;
      if( pers.current_income( pension_prev_emp ) > 0.0 ) or
        ( pers.current_income( pens_spse_prev_emp ) > 0.0 ) or
        ( pers.current_income( annuity_or_priv_pens ) > 0.0 ) then
          r( privpen ) := 1.0;
      end if;
      
   end Recalculate_Regressors;
   
   use Ada.Strings.Unbounded;
   use Text_Utils;

   function To_String( f : Fitness_Array ) return String is
      s : Unbounded_String;
   begin
      for t in Task_Type loop
         s := s & "         " & Task_Type'Image( t ) & " : " & "Help Needed: " & Help_Needed_Type'Image( f( t ).help ) & 
             "degree of difficulty: " & Difficulty_Type'Image( f( t ).diff ) & LINE_BREAK;
      end loop;
      return TS( s );
   end To_String;    
   
    function To_String( pers : Person ) return String is
      s : Unbounded_String;
   begin
      s := s & "      PID                                          : " & Sernum_Value'Image( pers.pid ) & LINE_BREAK;
      s := s & "      HID                                          : " & Sernum_Value'Image( pers.hid ) & LINE_BREAK;
      s := s & "      Pno                                          : " & Adult_Count'Image( pers.pno ) & LINE_BREAK;
      s := s & "      Age                                          : " & Age_Range'Image( pers.age ) & LINE_BREAK;
      s := s & "      Age                                          : " & Age_Range'Image( pers.age ) & LINE_BREAK;
      s := s & "      Sex                                          : " &  Prettify_Image( Gender_Type'Image( pers.sex )) & LINE_BREAK;
      s := s & "      Respondent Weights                           : " & Weights_Package.To_String( pers.respondent_weights ) & LINE_BREAK;
      s := s & "      Enumeration Weights                          : " & Weights_Package.To_String( pers.enumeration_weights ) & LINE_BREAK;
      s := s & "      Disability Score                             : " & Format( pers.activities_of_daily_living_score ) & LINE_BREAK;
      s := s & "      Health Score                                 : " & Format( pers.health_score ) & LINE_BREAK;
      s := s & "      Hdsp                                         : " & Prettify_Image( Head_Or_Spouse'Image( pers.hdsp )) & LINE_BREAK;
      s := s & "      Years In Residential Care                    : " & Age_Range'Image( pers.years_in_residential_care ) & LINE_BREAK;
      s := s & "      Has Full Sample                              : " & Boolean'Image( pers.has_full_sample ) & LINE_BREAK;
      s := s & "      Receives Informal Care from household member : " & Boolean'Image( pers.Receives_informal_care_from_household_member ) & LINE_BREAK;
      s := s & "      Receives informal care from non householder  : " & Boolean'Image( pers.Receives_informal_care_from_non_householder ) & LINE_BREAK;
      s := s & "      Hours of Care Received                       : " & Hours_Count'Image( pers.hours_of_care_recieved ) & LINE_BREAK;
      s := s & "      Hours of Care Given                          : " & Hours_Count'Image( pers.hours_of_care_given ) & LINE_BREAK;
      s := s & "      Dies this period                             : " & Boolean'Image( pers.dies_this_period ) & LINE_BREAK;
      s := s & "      Seperates this period                        : " & Boolean'Image( pers.seperates_this_period ) & LINE_BREAK;
      s := s & "      Usual Hours Worked Per Week                  : " & Hours_Count'Image( pers.usual_hours_worked_per_week ) & LINE_BREAK;
      s := s & "      Highest Qualification                        : " &  Qualification_Type'Image( pers.highest_qualification ) & LINE_BREAK;
      s := s & "      Is Disabled                                  : " & Boolean'Image( pers.is_disabled ) & LINE_BREAK;
      s := s & "      Marital Status                               : " & Marital_Status_Type'Image( pers.marital_status ) & LINE_BREAK;
      s := s & "      Partner Status                               : " & Partner_Status_Type'Image( pers.partner_status ) & LINE_BREAK;
      s := s & "      Health Status                                : " & Health_Status_Type'Image( pers.health_status ) & LINE_BREAK;
      
      s := s & "      Disability Living Allowance Mobility level   : " & High_Low_Nil_Type'Image( pers.disability_living_allowance_mobility_level ) & LINE_BREAK;
      s := s & "      Disability Living Allowance Care level       : " & High_Middle_Low_Nil_Type'Image( pers.disability_living_allowance_care_level ) & LINE_BREAK;
      s := s & "      Attendance Allowance Level                   : " & High_Low_Nil_Type'Image( pers.attendance_allowance_level ) & LINE_BREAK;

      s := s & "      Personal Wealth                              : " & Format( pers.personal_wealth ) & LINE_BREAK;
      s := s & "      Annual Incomes: " & LINE_BREAK & Income_Package.To_String( pers.current_income ) & LINE_BREAK;
      s := s & "      Current Income: " & LINE_BREAK & Income_Package.To_String( pers.current_income ) & LINE_BREAK;
      s := s & "      Fitness Measures                             : " & LINE_BREAK;
      s := s & To_String( pers.fitness ) & LINE_BREAK;
      s := s & "      Regressors: " & LINE_BREAK & Regressors_Package.To_String( pers.regressors ) & LINE_BREAK;
      return TS( s );
   end  To_String;

   function To_String( bu : Benefit_Unit ) return String is
      s : Unbounded_String;
   begin
      for adno in 1 .. bu.num_adults loop
         s := s & "  adult # " & Adult_Count'Image( adno ) & LINE_BREAK;
         s := s & To_String( bu.adults( adno )) & LINE_BREAK;            
      end loop;
      for chno in 1 .. bu.num_children loop
         s := s & "  child # " & Child_Count'Image( chno ) & LINE_BREAK;
         s := s & To_String( bu.children( chno )) & LINE_BREAK;            
      end loop;
      return TS( s );
   end To_String;
   
   function To_String( hdata : Household_Data ) return String is
      s : Unbounded_String;
   begin
      s := s & "   Interview date                   : " & Time_Format.Format( hdata.interview_date ) & LINE_BREAK;
      s := s & "   Simulation date                  : " & Time_Format.Format( hdata.current_simulated_date ) & LINE_BREAK;
      s := s & "   Wave                             : " & Waves'Image( hdata.wave ) & LINE_BREAK;
      s := s & "   Hid                              : " & Sernum_Value'Image( hdata.hid ) & LINE_BREAK;
      s := s & "   Origin Hid                       : " & Sernum_Value'Image( hdata.origin_hid ) & LINE_BREAK;
      s := s & "   Tenure                           : " & Prettify_Image( Tenure_Type'Image( hdata.tenure )) & LINE_BREAK;
      s := s & "   Region                           : " & Prettify_Image( Region_Type'Image( hdata.region )) & LINE_BREAK;
      s := s & "   Gross rent                       : " & Format( hdata.gross_rent ) & LINE_BREAK;
      s := s & "   Net rent                         : " & Format( hdata.net_rent ) & LINE_BREAK;
      s := s & "   Mortgage Outstanding             : " & Format( hdata.mortgage_outstanding ) & LINE_BREAK;
      s := s & "   Mortgage Payment                 : " & Format( hdata.mortgage_payment ) & LINE_BREAK;      
      s := s & "   Gross housing costs              : " & Format( hdata.gross_housing_costs ) & LINE_BREAK;
      s := s & "   Net housing costs                : " & Format( hdata.net_housing_costs ) & LINE_BREAK;
      s := s & "   Total income                     : " & Format( hdata.total_income ) & LINE_BREAK;
      s := s & "   House value                      : " & Format( hdata.house_value ) & LINE_BREAK;
      s := s & "   Other property value             : " & Format( hdata.other_property_value ) & LINE_BREAK;
      s := s & "   Years Outstanding on Mortgage    : " & Natural'Image( hdata.years_outstanding_on_mortgage ) & LINE_BREAK;
      s := s & "   Weights                          : " & Weights_Package.To_String( hdata.weights ) & LINE_BREAK;
      s := s & "   CT band                          : " & Prettify_Image( Council_Tax_Band'Image( hdata.ct_band )) & LINE_BREAK;
      s := s & "   Has full sample                  : " & Boolean'Image( hdata.has_full_sample ) & LINE_BREAK;
            
      return TS( s );
   end To_String;
   
   
   function To_String( hh : Household ) return String is
      s : Unbounded_String;
   begin
      s := s & "hh hid             : " & Sernum_Value'Image( hh.hid ) & LINE_BREAK;
      s := s & "num benefit units  :" & Benefit_Unit_Number'Image( hh.num_benefit_units ) & LINE_BREAK;
      s := s & To_String( hh.hdata ) & LINE_BREAK;
      for buno in 1 .. hh.num_benefit_units loop
         s := s & "benefit unit # " & Benefit_Unit_Number'Image( buno ) & LINE_BREAK;
         s := s & To_String( hh.benefit_units( buno ));
      end loop;
      return TS( s );
   end To_String;
   
   procedure Find_Person_By_Pno( 
      hh    : Household; 
      pno   : Person_Count; 
      buno  : out Benefit_Unit_Number; 
      adno  : out Adult_Count; 
      chno  : out Child_Count;
      found : out Boolean ) is
   begin
      buno := Benefit_Unit_Number'First; 
      adno := Adult_Count'First;  
      chno := Child_Count'First;  
      for bun in 1 .. hh.num_benefit_units loop
         for adn in 1 .. hh.benefit_units( bun ).num_adults loop
            if hh.benefit_units( bun ).adults( adn ).pno = pno then
               adno := adn;
               buno := bun;
               found := True;
               return;
            end if;
         end loop;
         for chn in 1 .. hh.benefit_units( bun ).num_children loop
            if hh.benefit_units( bun ).Children( chn ).pno = pno then
               chno := chn;
               buno := bun;
               found := True;
               return;
            end if;
         end loop;
      end loop;
      found := False;      
   end Find_Person_By_Pno;
   
   procedure Find_Person_By_Pid( 
      hh    : Household; 
      pid   : Sernum_Value; 
      buno  : out Benefit_Unit_Number; 
      adno  : out Adult_Count;
      chno  : out Child_Count;
      found : out Boolean ) is
   begin
      buno := Benefit_Unit_Number'First; 
      adno := Adult_Count'First;  
      chno := Child_Count'First;  
      for bun in 1 .. hh.num_benefit_units loop
         for adn in 1 .. hh.benefit_units( bun ).num_adults loop
            if hh.benefit_units( bun ).adults( adn ).pid = pid then
               adno := adn;
               buno := bun;
               found := True;
               return;
            end if;
         end loop;
         for chn in 1 .. hh.benefit_units( bun ).num_children loop
            if hh.benefit_units( bun ).Children( chn ).pid = pid then
               chno := chn;
               buno := bun;
               found := True;
               return;
            end if;
         end loop;
      end loop;
      found := False;      
   end Find_Person_By_Pid;

   
   function Num_People( hh : Household ) return Person_Count is
      n : Person_Count := 0;
   begin
      for buno in 1 .. hh.num_benefit_units loop
         n := n + hh.benefit_units( buno ).num_adults;
         n := n + hh.benefit_units( buno ).num_children;
      end loop;
      return n;
   end Num_People;

   function Num_Adults( hh : Household ) return Person_Count is
      n : Person_Count := 0;
   begin
      for buno in 1 .. hh.num_benefit_units loop
         n := n + hh.benefit_units( buno ).num_adults;
      end loop;
      return n;
   end Num_Adults;

   function Num_Children( hh : Household ) return Person_Count is
      n : Person_Count := 0;
   begin
      for buno in 1 .. hh.num_benefit_units loop
         n := n + hh.benefit_units( buno ).num_children;
      end loop;
      return n;
   end Num_Children;
   
   function Get_Age_Band( age : Age_Range ) return Age_Band_Type is
   begin
      case age is
         when 0 .. 15  => return age_0_15; 
         when 16 .. 18 => return age_16_18; 
         when 19 .. 21 => return age_19_21; 
         when 22 .. 39 => return age_22_39; 
         when 40 .. 44 => return age_40_44;
         when 45 .. 49 => return age_45_49; 
         when 50 .. 54 => return age_50_54; 
         when 55 .. 59 => return age_55_59; 
         when 60 .. 64 => return age_60_64;     
         when 65 .. 69 => return age_65_69;
         when 70 .. 74 => return age_70_74; 
         when 75 .. 79 => return age_75_79; 
         when 80 .. 84 => return age_80_84; 
         when 85 .. 89 => return age_85_89; 
         when 90 .. Age_Range'Last => return age_90_and_over;
      end case;
   end Get_Age_Band;

   --
   -- note this sorts lowest to highest
   --
   function Compare_By_V( p1, p2 : Person_Ordering_Record ) return Boolean is
   begin
      return p1.v < p2.v;
   end Compare_By_V;
   
   package PO_Sorter is new Person_Ordering_Package.Generic_Sorting( "<" => Compare_By_V ); 
   
   procedure Sort_By_V( list : in out Person_Ordering_List ) is
   begin
      PO_Sorter.Sort( list );
   end Sort_By_V;

   function Population_Total( list : Person_Ordering_List ) return Amount is
      popn : Amount := 0.0;
      n : Natural := Natural( list.Length );
   begin
      for i in 1 .. n loop
         popn := popn + list.Element( i ).weight;
      end loop;
      return popn;
   end Population_Total;
      
   function To_String( po : Person_Ordering_Record ) return String is
      s : Unbounded_String;
   begin
      s := s & LINE_BREAK;
      s := s & "hid :   " & Sernum_Value'Image( po.hid ) & LINE_BREAK;
      s := s & "pid :   " & Sernum_Value'Image( po.pid ) & LINE_BREAK;
      s := s & "pno :   " & Person_Number'Image( po.pno ) & LINE_BREAK;
      s := s & "weight :" & Format( po.weight ) & LINE_BREAK;
      s := s & "v :     " & Format( po.v ) & LINE_BREAK;
      return TS( s );
   end To_String;
   
   procedure Free_Household is new Ada.Unchecked_Deallocation( Object => Household, Name => Household_Access );
   
   procedure Free( hh : in out Household_Access ) is
   begin
      Free_Household( hh );
   end Free;
  
end Model.WSC.Household; 
