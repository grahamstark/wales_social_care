with Model.WSC.Regressors;
with Maths_Functions;
with Random_Normal_Draw;

package body Model.WSC.Household.Regressions is
   
   package MMath is new Maths_Functions( Amount );
   package Rand is new Random_Normal_Draw( Rate );
   
   use Regressors_Package;
   
   --
   --  > probit aa $agepoly $health $adlall $tenure $regdums if age>=65 & sex==1;
   --  
   function Receives_AA_Probit( ad : Person; receipt_last_period : Amount ) return Amount is
      coeffs     : Coeffs_Array := ( others => 0.0 );
      regressors    : Regressors_Array := ad.regressors;
      p             : Amount;
   begin
      if( ad.age < 65 )then
         return -999999.99;
      elsif( ad.age = 65 )then  -- 1st year; no lag possible
         if( ad.sex = male  )then
            coeffs := (
               age            =>   -0.8126337,
               age_2          =>    0.9556204,
               age_3          =>   -0.3630231,
               adla_diff      =>     0.689096,
               adla_vdiff     =>    0.7670485,
               adla_help      =>    0.9987806,
               adla_not       =>     1.028581,
               adlb_diff      =>   -0.0146467,
               adlb_vdiff     =>    0.3303281,
               adlb_help      =>    0.0010373,
               adlb_not       =>   -0.2577513,
               adld_diff      =>    0.3666915,
               adld_vdiff     =>    0.4753957,
               adld_help      =>    0.4493173,
               adld_not       =>    0.6937409,
               adle_diff      =>    0.3481024,
               adle_vdiff     =>    0.0885052,
               adle_help      =>    0.4643662,
               adle_not       =>   -0.1060006,
               ten_m          =>    -1.154333,
               owned_o        =>   -0.3557951,
               owned_m        =>     -0.16015,
               oth_rent       =>   -0.2606698,
               reg_il         =>   -0.8242581,
               reg_ol         =>   -0.6959524,
               reg_se         =>   -0.8756663,
               reg_sw         =>   -0.4116809,
               reg_ee         =>   -0.6998345,
               reg_em         =>   -0.3230499,
               reg_wmc        =>   -0.1660942,
               reg_wmo        =>   -0.2028295,
               reg_gm         =>    0.0979658,
               reg_me         =>   -0.8337397,
               reg_nwo        =>    -0.053262,
               reg_sy         =>   -0.6945465,
               reg_wy         =>   -0.5001451,
               reg_yho        =>   -0.6723425,
               reg_tw         =>    -0.336845,
               reg_no         =>   -0.0967981,
               reg_S          =>   -0.2316209,
               reg_NI         =>    0.1704565,
               const          =>      21.4748,
               others         =>    0.0 );
         else
            coeffs := (
               age            =>     -1.04459,
               age_2          =>      1.36354,
               age_3          =>   -0.5780196,
               adla_diff      =>    0.5253179,
               adla_vdiff     =>    0.6657388,
               adla_help      =>    0.7612138,
               adla_not       =>     1.002857,
               adlb_diff      =>    0.2330978,
               adlb_vdiff     =>    0.1908717,
               adlb_help      =>    0.1871536,
               adlb_not       =>   -0.1086601,
               adld_diff      =>    0.1633538,
               adld_vdiff     =>    0.2731919,
               adld_help      =>    0.4726735,
               adld_not       =>    0.5171976,
               adle_diff      =>    0.1643126,
               adle_vdiff     =>    0.1033935,
               adle_help      =>     0.302441,
               adle_not       =>      0.05356,
               ten_m          =>    -1.550898,
               owned_o        =>   -0.3283165,
               owned_m        =>   -0.0293648,
               oth_rent       =>   -0.0580637,
               reg_il         =>    -1.595399,
               reg_ol         =>   -0.3441471,
               reg_se         =>    -0.535734,
               reg_sw         =>   -0.2402821,
               reg_ee         =>    -0.643355,
               reg_em         =>   -0.3575057,
               reg_wmc        =>   -0.3039527,
               reg_wmo        =>   -0.3071973,
               reg_gm         =>   -0.2445109,
               reg_me         =>   -0.1989887,
               reg_nwo        =>   -0.0801137,
               reg_sy         =>   -0.4790189,
               reg_wy         =>   -0.2600432,
               reg_yho        =>   -0.3383091,
               reg_tw         =>   -0.4923426,
               reg_no         =>   -0.2484363,
               reg_S          =>   -0.2506684,
               reg_NI         =>    0.4776361,
               const          =>     24.96321,
               others         =>    0.0 );
         end if;
      else -- over 65 - use lagged receipts
         if( receipt_last_period > 0.0 )then
            regressors( lagged_depvar ) := 1.0;
         end if;
         if( ad.sex = male )then
            coeffs := (
               lagged_depvar  =>     2.049831,
               age            =>   -0.1601717,
               age_2          =>    0.3787802,
               age_3          =>   -0.2113651,
               adla_diff      =>    0.2582013,
               adla_vdiff     =>    0.5304089,
               adla_help      =>     0.220332,
               adla_not       =>    0.3463105,
               adlb_diff      =>    -0.108693,
               adlb_vdiff     =>    0.3271049,
               adlb_help      =>   -0.1872633,
               adlb_not       =>   -0.1022359,
               adld_diff      =>    0.1806083,
               adld_vdiff     =>    0.3330295,
               adld_help      =>    0.2133359,
               adld_not       =>    0.2871926,
               adle_diff      =>    0.2864241,
               adle_vdiff     =>    0.1283485,
               adle_help      =>    0.4192376,
               adle_not       =>    0.3649384,
               ten_m          =>    -0.435002,
               owned_o        =>   -0.1002466,
               owned_m        =>   -0.1462132,
               oth_rent       =>   -0.0729774,
               reg_il         =>   -0.0751865,
               reg_ol         =>    0.0311101,
               reg_se         =>   -0.4095003,
               reg_sw         =>   -0.1364902,
               reg_ee         =>   -0.1868461,
               reg_em         =>   -0.1760362,
               reg_wmc        =>   -0.0539246,
               reg_wmo        =>    0.1190564,
               reg_gm         =>   -0.2120147,
               reg_me         =>   -0.4180002,
               reg_nwo        =>    0.0586548,
               reg_sy         =>   -0.4126091,
               reg_wy         =>   -0.1361276,
               reg_yho        =>   -0.3252777,
               reg_tw         =>     0.026625,
               reg_no         =>    -0.018123,
               reg_S          =>    0.0040535,
               reg_NI         =>     0.314012,
               const          =>    -2.312899,
               others         =>    0.0 );
         end if;
         --
         --
         --
         --  > probit aa L.aa $agepoly $health $adlall $tenure $regdums if age>=65 & sex==2 & L.aa~=.;
         if( ad.sex = female )then
            coeffs := (
               lagged_depvar  =>     1.860857,
               age            =>    0.9797691,
               age_2          =>   -0.9717317,
               age_3          =>    0.3146334,
               adla_diff      =>    0.2020207,
               adla_vdiff     =>    0.4555837,
               adla_help      =>     0.386758,
               adla_not       =>    0.4153551,
               adlb_diff      =>    0.1423146,
               adlb_vdiff     =>   -0.2364868,
               adlb_help      =>    0.1791385,
               adlb_not       =>    -0.123333,
               adld_diff      =>    0.0199871,
               adld_vdiff     =>    0.1956697,
               adld_help      =>    0.2383883,
               adld_not       =>    0.2400389,
               adle_diff      =>    0.0627914,
               adle_vdiff     =>    0.1590974,
               adle_help      =>    0.1765299,
               adle_not       =>   -0.0425626,
               ten_m          =>    -1.072154,
               owned_o        =>   -0.1690398,
               owned_m        =>    0.0031027,
               oth_rent       =>   -0.1235158,
               reg_il         =>    -1.202401,
               reg_ol         =>   -0.0892537,
               reg_se         =>   -0.3060208,
               reg_sw         =>   -0.1133764,
               reg_ee         =>   -0.2074493,
               reg_em         =>   -0.2303625,
               reg_wmc        =>   -0.0117321,
               reg_wmo        =>     0.035524,
               reg_gm         =>    0.0002763,
               reg_me         =>   -0.3006007,
               reg_nwo        =>   -0.0262005,
               reg_sy         =>   -0.3194645,
               reg_wy         =>   -0.0859132,
               reg_yho        =>   -0.3043502,
               reg_tw         =>   -0.2478782,
               reg_no         =>    -0.193109,
               reg_S          =>   -0.0602899,
               reg_NI         =>    0.2511144,
               const          =>    -33.74853,
               others         =>    0.0 );
         end if;
      end if; -- aged over 65 - lagged depvar version
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end Receives_AA_Probit;   

   --
   --  > probit dlaany $agepoly $health $adlall $tenure $regdums if age>=65 & sex==1;
   function Receives_DLA_Probit( ad : Person; receipt_last_period : Amount ) return Amount is
      coeffs     : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
      p          : Amount;
   begin
      if( receipt_last_period > 0.0 )then
         regressors( lagged_depvar ) := 1.0;
      end if;
      --  > probit dlaany L.dlaany $agepoly $health $adlall $tenure $regdums if age>=65 & sex==1 & L.dlaany~=.;
      --
      if( ad.sex = male )then
         coeffs := (
            lagged_depvar  =>     2.600702,
            age            =>    0.5473266,
            age_2          =>   -0.7662817,
            age_3          =>    0.3415098,
            adla_diff      =>    0.4350017,
            adla_vdiff     =>    0.2955922,
            adla_help      =>    0.6023015,
            adla_not       =>    0.6444282,
            adlb_diff      =>    0.1112521,
            adlb_vdiff     =>    0.3126597,
            adlb_help      =>    0.2677464,
            adlb_not       =>   -0.0368413,
            adld_diff      =>    0.2929959,
            adld_vdiff     =>   -0.1499834,
            adld_help      =>    0.1315032,
            adld_not       =>    0.3571298,
            adle_diff      =>    0.1414201,
            adle_vdiff     =>     0.010579,
            adle_help      =>    0.1948805,
            adle_not       =>   -0.2716112,
            ten_m          =>   -0.0332556,
            owned_o        =>   -0.2734351,
            owned_m        =>   -0.1287891,
            oth_rent       =>   -0.1944273,
            reg_il         =>   -0.6453171,
            reg_ol         =>   -0.5265478,
            reg_se         =>    -0.516457,
            reg_sw         =>   -0.2756628,
            reg_ee         =>   -0.5979681,
            reg_em         =>   -0.1390322,
            reg_wmc        =>   -0.2041627,
            reg_wmo        =>   -0.3811261,
            reg_gm         =>   -0.0161932,
            reg_me         =>   -0.4865056,
            reg_nwo        =>   -0.0986781,
            reg_sy         =>   -0.2916623,
            reg_wy         =>   -0.1611138,
            reg_yho        =>   -0.0922209,
            reg_tw         =>   -0.1162371,
            reg_no         =>    0.0175311,
            reg_S          =>   -0.1831518,
            reg_NI         =>   -0.1444231,
            const          =>    -14.21707,
            others         =>    0.0 );
      end if;
      --
      --  > probit dlaany L.dlaany $agepoly $health $adlall $tenure $regdums if age>=65 & sex==2 & L.dlaany~=.;
      --  
      --
      if( ad.sex = female )then
         coeffs := (
            lagged_depvar  =>     2.356905,
            age            =>    0.0216405,
            age_2          =>   -0.1013807,
            age_3          =>    0.0656548,
            adla_diff      =>      0.41151,
            adla_vdiff     =>    0.2266379,
            adla_help      =>    0.2565622,
            adla_not       =>    0.5542898,
            adlb_diff      =>    0.1239693,
            adlb_vdiff     =>    0.3602831,
            adlb_help      =>    0.1215943,
            adlb_not       =>   -0.0892081,
            adld_diff      =>    0.1526693,
            adld_vdiff     =>      0.11188,
            adld_help      =>    0.3496297,
            adld_not       =>    0.3624762,
            adle_diff      =>    0.0081009,
            adle_vdiff     =>   -0.1993929,
            adle_help      =>   -0.0926783,
            adle_not       =>   -0.1677202,
            ten_m          =>   -0.6641647,
            owned_o        =>   -0.2301696,
            owned_m        =>    0.0386417,
            oth_rent       =>    0.0460935,
            reg_ol         =>   -0.2583237,
            reg_se         =>   -0.2464435,
            reg_sw         =>   -0.1735916,
            reg_ee         =>   -0.2801388,
            reg_em         =>    -0.201468,
            reg_wmc        =>   -0.3682079,
            reg_wmo        =>   -0.3890908,
            reg_gm         =>   -0.2378296,
            reg_me         =>    0.0278301,
            reg_nwo        =>    0.0367446,
            reg_sy         =>   -0.0255467,
            reg_wy         =>   -0.2633051,
            reg_yho        =>   -0.1182914,
            reg_tw         =>   -0.0360895,
            reg_no         =>   -0.2237966,
            reg_S          =>   -0.2538247,
            reg_NI         =>    0.0993097,
            const          =>   -0.7037645,
            others         =>    0.0 );
      end if;
      p := Vector_Product( regressors, coeffs );
      return p;   
   end Receives_DLA_Probit;   

   
   function Has_Wealth_Probit( ad : Person ) return Amount is
      coeffs     : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
      p          : Amount;
   begin
      --
      --  . probit wealth  $agepoly $tenure $quals spinhh working ispc disben privpen if sex==1 & age>=60;
      --
      if( ad.age < 60 )then
         return -999999.99;
      end if;
      if( ad.sex = male and ad.age >= 60 )then
         coeffs := (
            age            =>   -0.5336373,
            age_2          =>    0.5931942,
            age_3          =>   -0.2005652,
            owned_o        =>    0.7939764,
            owned_m        =>     0.590803,
            socrent        =>    0.2352344,
            hq_deg         =>    0.8463607,
            q_oth         =>    0.3387782,
            spinhh         =>   -0.3440977,
            working        =>   -0.0864859,
            ispc           =>     -1.20272,
            disben         =>   -0.1093406,
            privpen        =>    0.7633954,
            const          =>     14.57745,
            others         =>    0.0 );
      end if;
      --
      --  . probit wealth $agepoly $tenure $quals spinhh working ispc disben privpen if sex==2 & age>=60;
      --  
      --
      if( ad.sex = female and ad.age >= 60 )then
         coeffs := (
            age            =>    -1.020881,
            age_2          =>      1.44193,
            age_3          =>   -0.6618577,
            owned_o        =>    0.2181092,
            owned_m        =>   -0.2658743,
            socrent        =>   -0.7291014,
            hq_deg         =>    0.8308371,
            q_oth         =>    0.7211276,
            spinhh         =>    0.0450244,
            working        =>    0.1441464,
            ispc           =>   -0.4311769,
            disben         =>   -0.1565342,
            privpen        =>    0.2506206,
            const          =>     23.14196,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end Has_Wealth_Probit;   

   function Log_Wealth_Regression( ad : Person; region : Region_Type ) return Regression_Results is
      coeffs     : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
      r          : Regression_Results;
   begin
      --
      --  . regress logwealth  $agepoly $tenure $quals spinhh working ispc disben privpen if sex==1 & age>=60 & wealth==1 [aw=wave1wgt];
      --  (sum of wgt is   1.7662e+05)
      --
      if( ad.sex = male and ad.age >= 60 )then
         coeffs := (
            age            =>    -1.828641,
            age_2          =>     2.336877,
            age_3          =>   -0.9836738,
            owned_o        =>   -0.1569361,
            owned_m        =>    -0.584389,
            socrent        =>   -0.8263617,
            hq_deg         =>    0.7277306,
            q_oth         =>    0.2575215,
            spinhh         =>   -0.3664481,
            working        =>   -0.1414765,
            ispc           =>   -0.0131117,
            disben         =>   -0.5736748,
            privpen        =>    0.0642335,
            const          =>     57.81431,
            others         =>    0.0 );
            r.sd := 0.4071714;
      end if;
      --
      --  . regress logwealth $agepoly $tenure $quals spinhh working ispc disben privpen if sex==2 & age>=60 & wealth==1 [aw=wave1wgt];
      --  (sum of wgt is   1.9326e+05)
      --
      if( ad.sex = female and ad.age >= 60 )then
         coeffs := (
            age            =>    -1.486739,
            age_2          =>     1.876676,
            age_3          =>   -0.7819455,
            owned_o        =>   -0.4915864,
            owned_m        =>    -1.185368,
            socrent        =>    -1.089082,
            hq_deg         =>    0.6628543,
            hq_oth         =>     0.366651,
            spinhh         =>    0.1778105,
            working        =>   -0.0653802,
            ispc           =>   -0.7548687,
            disben         =>   -0.0740709,
            privpen        =>   -0.2626482,
            const          =>     49.31527,
            others         =>    0.0 );
            r.sd := 0.4592151;
      end if;
      --
      --
      r.vp := Vector_Product( regressors, coeffs );
      return r;
   end Log_Wealth_Regression;

   function  Hours_Of_Care_Regression( ad : Person; region : Region_Type ) return Regression_Results is
      coeffs     : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
      r          : Regression_Results;
   begin
      --
      --  regress hrs_care $agepoly spinhh $healthvars if age>=65 & wales==1 & sex==1 & rec_care==1;
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            age            =>     98.95351,
            age_2          =>    -117.3564,
            age_3          =>     45.82451,
            spinhh         =>     18.41606,
            hl_exc         =>     -17.7624,
            hl_good        =>   -0.2751777,
            hl_poor        =>     29.06532,
            hl_vpoor       =>      19.8316,
            const          =>    -2726.389,
            others         =>    0.0 );
         r.sd := 50.65882;-- FIXME GET FROM HR
      end if;
      --
      -- regress hrs_care $agepoly spinhh $healthvars if age>=65 & wales==1 & sex==2 & rec_care==1;
      --
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            age            =>     57.80654,
            age_2          =>    -73.67423,
            age_3          =>     31.84331,
            spinhh         =>     2.720769,
            hl_exc         =>     2.764437,
            hl_good        =>     14.98159,
            hl_poor        =>     19.29674,
            hl_vpoor       =>     34.87564,
            const          =>    -1495.423,
            others         =>    0.0 );
         r.sd := 52.83546; -- FIXME GET FROM HR
      end if;
      --
      --
      r.vp := Vector_Product( regressors, coeffs );
      return r;
   end Hours_Of_Care_Regression;

   
   function Informal_Care_From_Non_Householder_Probit( ad : Person; ad_last_period : Person; wave : Waves ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      if( ad.age < 65 )then
         return -99999.99;
      end if;
      if ad_last_period.receives_informal_care_from_non_householder then
         regressors( lagged_depvar ) := 1.0;
      else
         regressors( lagged_depvar ) := 0.0;
      end if;
      --
      --  > probit carenhh L.carenhh $agedums trend couple $adl $healthvars if age>=65 & indsex
      --  > ==1 & wave>1;
      --
      if( ad.sex = male and ad.age >= 65 and wave > a )then
        coeffs := (
            lagged_depvar  =>    0.9292838,
            age            =>     -1.79844,
            age_2          =>      2.25418,
            age_3          =>   -0.9164846,
            couple         =>   -0.4929454,
            adl_b          =>    0.2517881,
            adl_c          =>    0.4963694,
            adl_e          =>    0.2297227,
            hl_exc         =>    -0.751033,
            hl_good        =>   -0.3970667,
            hl_poor        =>    0.3221877,
            hl_vpoor       =>    0.4878925,
            hl_m           =>    0.2925096,
            const          =>     45.19135,
            others         =>    0.0 );
      end if;
      --
      --  . probit carenhh L.carenhh $agedums trend couple $adl $healthvars if age>=65 & indsex
      --  > ==2 & wave>1;
      --
      if( ad.sex = female and ad.age >= 65 and wave > a )then
         coeffs := (
            lagged_depvar  =>    0.9472619,
            age            =>    -0.438921,
            age_2          =>     0.617449,
            age_3          =>   -0.2697857,
            couple         =>   -0.5316892,
            adl_b          =>    0.1682442,
            adl_c          =>     0.436074,
            adl_e          =>    0.0739401,
            hl_exc         =>   -0.5695472,
            hl_good        =>   -0.3682404,
            hl_poor        =>    0.2314049,
            hl_vpoor       =>    0.5083906,
            hl_m           =>    0.2006054,
            const          =>     8.421749,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end Informal_Care_From_Non_Householder_Probit;      
   
   --
   -- CHECK THIS 2 VERSIONS! 
   --
   function Informal_Care_From_Householder_Probit( ad : Person; ad_last_period : Person; region : Region_Type; use_lagged_dep_vars : Boolean ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      if( ad.age < 65 )then
         return -99999.99;
      end if;
      if ad_last_period.receives_informal_care_from_household_member then
         regressors( lagged_depvar ) := 1.0;
      else
         regressors( lagged_depvar ) := 0.0;
      end if;
      --  > age>=65 & wales==1 & sex==1;
      -- updated
      if( use_lagged_dep_vars )then
         if( ad.sex = male and ad.age >= 65 and region = wales )then
            coeffs := (
               lagged_depvar  =>     2.109797,
               age            =>     2.615803,
               age_2          =>    -3.233201,
               age_3          =>     1.328267,
               spinhh         =>   -0.2542178,
               adla_diff      =>     0.498788,
               adla_vdiff     =>    0.8106467,
               adla_help      =>     0.894058,
               adla_not       =>    0.4483325,
               adld_diff      =>   -0.0022625,
               adld_vdiff     =>     0.060666,
               adld_help      =>    0.5041557,
               adld_not       =>    0.4904544,
               const          =>    -71.87927,
               others         =>    0.0 );
         end if;
         --
         --  . probit rec_care L.rec_care $agedums trend adlscore spinhh $healthvars $adlvars2 if 
         --  > age>=65 & wales==1 & sex==2;
         -- updated
         if( ad.sex = female and ad.age >= 65 and region = wales )then
            coeffs := (
               lagged_depvar  =>     2.368224,
               age            =>     1.424617,
               age_2          =>    -1.804174,
               age_3          =>    0.7573493,
               spinhh         =>    -0.193854,
               adla_diff      =>    0.4847755,
               adla_vdiff     =>     0.470436,
               adla_help      =>    0.9238459,
               adla_not       =>    0.5196518,
               adld_diff      =>   -0.0806131,
               adld_vdiff     =>    0.7191399,
               adld_help      =>    0.1471455,
               adld_not       =>    0.1985971,
               const          =>    -39.13481,
               others         =>    0.0 );
         end if;
      else
         if( ad.sex = male and ad.age >= 65 )then
            coeffs := (
               age            =>     4.121037,
               age_2          =>    -5.311862,
               age_3          =>     2.273882,
               adlscore       =>   -0.1100525,
               spinhh         =>   -0.3624428,
               hl_exc         =>   -0.8286056,
               hl_good        =>   -0.3575385,
               hl_poor        =>    0.4736279,
               hl_vpoor       =>    0.2135089,
               adla_diff      =>    0.5817324,
               adla_vdiff     =>     1.089785,
               adla_help      =>     1.327492,
               adla_not       =>     1.739473,
               adlb_diff      =>    0.3110216,
               adlb_vdiff     =>     0.530132,
               adlb_help      =>     1.232798,
               adlb_not       =>     1.138988,
               adlc_diff      =>   -0.1951552,
               adlc_vdiff     =>    0.2016143,
               adlc_help      =>    0.8709876,
               adld_diff      =>    0.3168711,
               adld_vdiff     =>    0.6064056,
               adld_help      =>     1.384627,
               adld_not       =>     1.873838,
               adle_diff      =>    0.2745479,
               adle_vdiff     =>    0.1473752,
               adle_help      =>    0.9775732,
               adle_not       =>     1.505242,
               const          =>    -107.2567,
               others         =>    0.0 );
         end if;
         --  . probit rec_care  $agepoly adlscore spinhh $healthvars $adlall if age>=65 & wales==1 & sex==2;
         --
         if( ad.sex = female and ad.age >= 65 )then
            coeffs := (
               age            =>     2.404815,
               age_2          =>    -3.141192,
               age_3          =>      1.35462,
               adlscore       =>   -0.0256708,
               spinhh         =>   -0.3192684,
               hl_exc         =>   -0.6782684,
               hl_good        =>   -0.2391217,
               hl_poor        =>    0.3957619,
               hl_vpoor       =>    0.1462036,
               adla_diff      =>    0.6084987,
               adla_vdiff     =>    0.5275427,
               adla_help      =>     1.177421,
               adla_not       =>    0.7049912,
               adlb_diff      =>    0.2301933,
               adlb_vdiff     =>     0.117202,
               adlb_help      =>    0.5977517,
               adlc_diff      =>    0.1524009,
               adlc_vdiff     =>    0.1681544,
               adlc_help      =>    0.6516381,
               adlc_not       =>    0.6540169,
               adld_diff      =>   -0.0243161,
               adld_vdiff     =>    0.7257041,
               adld_help      =>    0.5108592,
               adld_not       =>    0.6637434,
               adle_diff      =>    0.1480731,
               adle_vdiff     =>   -0.0969582,
               adle_help      =>    0.5083551,
               adle_not       =>      0.42818,
               const          =>    -62.23101,
               others         =>    0.0 );
         end if;
      end if;
      --
      --  
      p := Vector_Product( regressors, coeffs );
      return p;   
   end Informal_Care_From_Householder_Probit;
   
   function Health_Worse_Probit( ad : Person; region : Region_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      if( ad.age < 65 )then
         return -99999.99;
      end if;
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            hl_exc         =>      1.06218,
            hl_good        =>    0.2928482,
            hl_poor        =>   -0.1360769,
            age            =>    -4.375676,
            age_2          =>     5.873243,
            age_3          =>    -2.600492,
            const          =>     106.5816,
            others         =>    0.0 );

      end if;
      --
      -- and healthsc < 5
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            hl_exc         =>    0.8291384,
            hl_good        =>    0.2464003,
            hl_poor        =>   -0.1918449,
            age            =>    -1.812622,
            age_2          =>     2.317528,
            age_3          =>   -0.9706754,
            const          =>     45.60006,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end Health_Worse_Probit;

   function Health_Better_Probit( ad : Person; region : Region_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      if( ad.age < 65 )then
         return -99999.99;
      end if;
      --
      --  > probit hl_better $healthvars $agepoly if sex==1 & age>=65 & wales==1 & hl_worse==0 & healthsc>1;
      -- and healthsc > 1
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            hl_good        =>   -0.6079938,
            hl_poor        =>    0.0658496,
            hl_vpoor       =>    0.7120249,
            age            =>    -1.536055,
            age_2          =>     2.038065,
            age_3          =>     -0.89592,
            const          =>     38.09827,
            others         =>    0.0 );
      end if;
      --
      --
      -- . probit hl_better $healthvars $agepoly if sex==2 & age>=65 & wales==1 & hl_worse==0 & healthsc>1;
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            hl_good        =>   -0.5085135,
            hl_poor        =>    0.3116947,
            hl_vpoor       =>    0.6663046,
            age            =>    -1.210706,
            age_2          =>     1.455243,
            age_3          =>    -0.578403,
            const          =>     32.80009,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end Health_Better_Probit;
   --
   -- NOTE from bi-annual ELSA data
   --
   function To_Care_Probit( ad : Person ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      if( ad.age >= 65 )then
         coeffs := (
            female         =>    0.3686283,
            age            =>    -3.834016,
            age_2          =>     5.077663,
            age_3          =>    -2.185432,
            adl_b          =>    0.0835879,
            adl_c          =>    0.0528569,
            adl_e          =>    0.3030085,
            hl_exc         =>    0.1597538,
            hl_good        =>    0.1871745,
            hl_poor        =>      0.29455,
            hl_vpoor       =>    0.1974546,
            hl_m           =>     1.232349,
            const          =>     90.79723,
            others         =>    0.0 );
      end if;
      p := Vector_Product( regressors, coeffs );
      return p;   
   end To_Care_Probit;

   
   function ADL_Base_Probit( ad : Person; region : Region_Type; which : Task_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      if( ad.age < 65 )then
         return -99999.99;
      end if;
      if( ad.age in 65 .. 70 )then
         case which is
         when manage_stairs =>
            if( ad.sex = male )then
               coeffs := (
                  age66          =>   -0.0279117,
                  age67          =>    0.0492608,
                  age68          =>    0.0717791,
                  age69          =>   -0.0213081,
                  age70          =>   -0.0096139,
                  hl_exc         =>     -1.16863,
                  hl_good        =>   -0.9322928,
                  hl_poor        =>     1.022467,
                  hl_vpoor       =>     1.494855,
                  england        =>   -0.3080215,
                  scotland       =>   -0.1837166,
                  const          =>   -0.5642602,
                  others         =>    0.0 );
            else
               coeffs := (
                  age66          =>    0.0302588,
                  age67          =>    0.1655869,
                  age68          =>    0.0685592,
                  age69          =>    0.1472834,
                  age70          =>    0.1790979,
                  hl_exc         =>    -1.564241,
                  hl_good        =>   -0.8574789,
                  hl_poor        =>    0.8611652,
                  hl_vpoor       =>     1.024475,
                  england        =>   -0.1224194,
                  scotland       =>    0.0082114,
                  const          =>   -0.5363188,
                  others         =>    0.0 );
            end if;
         when get_around_house => 
            --
            --
            --
            --  . probit adl_b age66 age67 age68 age69 age70 $healthvars england scotland  if age>=65 & age<=70 & sex==1;
            --  
            --
            if( ad.sex = male )then
               coeffs := (
                  age66          =>   -0.1557729,
                  age67          =>   -0.3110602,
                  age68          =>   -0.0392555,
                  age69          =>   -0.2990735,
                  age70          =>   -0.1875754,
                  hl_exc         =>   -0.9109162,
                  hl_good        =>   -0.6031889,
                  hl_poor        =>     1.029235,
                  hl_vpoor       =>     1.881896,
                  england        =>   -0.3004636,
                  scotland       =>   -0.3565631,
                  const          =>    -1.353864,
                  others         =>    0.0 );
            else
               coeffs := (
                  age66          =>   -0.0365216,
                  age67          =>    0.2500612,
                  age68          =>    0.1699187,
                  age69          =>    0.1450002,
                  age70          =>    0.1948064,
                  hl_exc         =>    -1.102983,
                  hl_good        =>    -0.508575,
                  hl_poor        =>    0.9525301,
                  hl_vpoor       =>     1.408616,
                  england        =>   -0.2355587,
                  scotland       =>   -0.0619515,
                  const          =>    -1.624317,
                  others         =>    0.0 );
            end if;
         when get_in_or_out_of_bed =>
            --
            --  . probit adl_c age66 age67 age68 age69 age70 $healthvars england scotland  if age>=65 & age<=70 & sex==1;
            if( ad.sex = male )then
               coeffs := (
                  age66          =>   -0.2736266,
                  age67          =>    -0.094708,
                  age68          =>   -0.0326526,
                  age69          =>   -0.1306569,
                  age70          =>    0.0836399,
                  hl_exc         =>    -1.066776,
                  hl_good        =>   -0.7024546,
                  hl_poor        =>    0.7956444,
                  hl_vpoor       =>     1.734596,
                  england        =>   -0.2862416,
                  scotland       =>   -0.3890636,
                  const          =>    -1.266154,
                  others         =>    0.0 );
            else
            --  . probit adl_c age66 age67 age68 age69 age70 $healthvars england scotland  if age>=65 & age<=70 & sex==2;
               coeffs := (
                  age66          =>     0.105711,
                  age67          =>    0.2553652,
                  age68          =>    0.2478701,
                  age69          =>    0.1557936,
                  age70          =>    0.0437864,
                  hl_exc         =>    -1.206664,
                  hl_good        =>   -0.5849399,
                  hl_poor        =>    0.7777059,
                  hl_vpoor       =>     1.090963,
                  england        =>   -0.2393974,
                  scotland       =>    -0.193143,
                  const          =>    -1.385249,
                  others         =>    0.0 );
            end if;
         --
         --  . probit adl_d age66 age67 age68 age69 age70 $healthvars england scotland  if age>=65 & age<=70 & sex==1;
         when cut_toenails =>
            if( ad.sex = male )then
               coeffs := (
                  age66          =>    0.0377923,
                  age67          =>    0.1986758,
                  age68          =>    0.2915699,
                  age69          =>    0.2955732,
                  age70          =>    0.3020346,
                  hl_exc         =>    -1.147963,
                  hl_good        =>   -0.6824805,
                  hl_poor        =>     0.754871,
                  hl_vpoor       =>     1.190416,
                  england        =>   -0.2730147,
                  scotland       =>   -0.1859174,
                  const          =>    -0.449828,
                  others         =>    0.0 );
            else
            --  . probit adl_d age66 age67 age68 age69 age70 $healthvars england scotland  if age>=65 & age<=70 & sex==2;
               coeffs := (
                  age66          =>    0.0773439,
                  age67          =>    0.1757389,
                  age68          =>    0.2063171,
                  age69          =>    0.2380288,
                  age70          =>    0.4054152,
                  hl_exc         =>    -1.250104,
                  hl_good        =>   -0.7242934,
                  hl_poor        =>    0.6232351,
                  hl_vpoor       =>    0.7655654,
                  england        =>   -0.1066316,
                  scotland       =>   -0.1675604,
                  const          =>   -0.1651358,
                  others         =>    0.0 );
            end if;
         when bathing_or_showering =>
            --
            --  . probit adl_e age66 age67 age68 age69 age70 $healthvars england scotland  if age>=65 & age<=70 & sex==1;
            if( ad.sex = male )then
               coeffs := (
                  age66          =>    0.1998172,
                  age67          =>    0.1294159,
                  age68          =>    0.2765349,
                  age69          =>    0.1539155,
                  age70          =>    0.3051063,
                  hl_exc         =>   -0.9373883,
                  hl_good        =>    -0.516058,
                  hl_poor        =>     1.015719,
                  hl_vpoor       =>     1.557516,
                  england        =>   -0.1288975,
                  scotland       =>   -0.1891576,
                  const          =>    -1.490486,
                  others         =>    0.0 );
            --  . probit adl_e age66 age67 age68 age69 age70 $healthvars england scotland  if age>=65 & age<=70 & sex==2;
            else
               coeffs := (
                  age66          =>    0.0927814,
                  age67          =>    0.1272466,
                  age68          =>    0.2352598,
                  age69          =>    0.2555301,
                  age70          =>    0.3356552,
                  hl_exc         =>    -1.268661,
                  hl_good        =>   -0.6100833,
                  hl_poor        =>    0.7550829,
                  hl_vpoor       =>     1.101519,
                  england        =>   -0.1780017,
                  scotland       =>   -0.1424988,
                  const          =>    -1.208949,
                  others         =>    0.0 );
            end if;
         when walk_down_road =>
           null;
         end case;
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end  ADL_Base_Probit;
      
   
   
   function ADL_A_Worse_Probit( ad : Person; region : Region_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      -- robit adl_a_w adla_diff adla_vdiff adla_help $agepoly $healthvars if sex==1 & age>=65 & wales==1 & adla_not==0;
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            adla_diff      =>    0.1507531,
            adla_vdiff     =>    0.2425611,
            adla_help      =>    0.3060451,
            age            =>     1.378233,
            age_2          =>      -1.8742,
            age_3          =>    0.8624921,
            hl_exc         =>   -0.5935192,
            hl_good        =>    -0.293103,
            hl_poor        =>    0.1738994,
            hl_vpoor       =>    0.0479758,
            const          =>    -35.55549,
            others         =>    0.0 );
      end if;
      --
      --
      -- . probit hl_better $healthvars $agepoly if sex==2 & age>=65 & wales==1 & hl_worse==0 & healthsc>1;
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            adla_diff      =>    0.4910673,
            adla_vdiff     =>    0.6325873,
            adla_help      =>    0.0609145,
            age            =>   -0.1117038,
            age_2          =>    0.2104197,
            age_3          =>    -0.101593,
            hl_exc         =>   -0.5160122,
            hl_good        =>   -0.3124538,
            hl_poor        =>     0.005084,
            hl_vpoor       =>    0.2922182,
            const          =>   -0.3507321,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end ADL_A_Worse_Probit;
   
   function ADL_A_Better_Probit( ad : Person; region : Region_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      -- robit adl_a_w adla_diff adla_vdiff adla_help $agepoly $healthvars if sex==1 & age>=65 & wales==1 & adla_not==0;
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            adla_diff      =>    -1.241603,
            adla_vdiff     =>   -0.5627578,
            adla_help      =>   -0.8559649,
            age            =>     1.395911,
            age_2          =>    -1.688009,
            age_3          =>    0.6610759,
            hl_exc         =>    0.5592953,
            hl_good        =>    0.4904278,
            hl_poor        =>   -0.5708838,
            hl_vpoor       =>   -0.7135583,
            const          =>    -36.82397,
            others         =>    0.0 );
      end if;
      --
      --
      -- . probit hl_better $healthvars $agepoly if sex==2 & age>=65 & wales==1 & hl_worse==0 & healthsc>1;
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            adla_diff      =>    -1.136113,
            adla_vdiff     =>   -0.6302466,
            adla_help      =>    -1.277417,
            age            =>     2.051312,
            age_2          =>     -2.58994,
            age_3          =>     1.064002,
            hl_exc         =>    0.2759361,
            hl_good        =>    0.3726847,
            hl_poor        =>   -0.2819548,
            hl_vpoor       =>   -0.5611943,
            const          =>     -52.3873,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end ADL_A_Better_Probit;
   
   
   
   function ADL_B_Worse_Probit( ad : Person; region : Region_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      -- probit adl_b_w adlb_diff adlb_vdiff adlb_help $agepoly $healthvars if sex==1 & age>=65 & wales==1 & adlb_not==0;
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            adlb_diff      =>   -0.0651274,
            adlb_vdiff     =>   -0.2338201,
            adlb_help      =>   -0.4580525,
            age            =>     2.580808,
            age_2          =>    -3.492326,
            age_3          =>     1.580524,
            hl_exc         =>   -0.6061805,
            hl_good        =>   -0.4011105,
            hl_poor        =>     0.409755,
            hl_vpoor       =>    0.6177731,
            const          =>    -65.52244,
            others         =>    0.0 );
      end if;
      --
      -- probit adl_b_w adlb_diff adlb_vdiff adlb_help $agepoly $healthvars if sex==2 & age>=65 & wales==1 & adlb_not==0;
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            adlb_diff      =>    0.1708421,
            adlb_vdiff     =>   -0.5051366,
            adlb_help      =>   -0.0219366,
            age            =>    -1.008146,
            age_2          =>     1.418089,
            age_3          =>   -0.6349363,
            hl_exc         =>   -0.8126801,
            hl_good        =>   -0.5209134,
            hl_poor        =>    0.3044696,
            hl_vpoor       =>     0.318792,
            const          =>     21.14848,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end ADL_B_Worse_Probit;

   function ADL_B_Better_Probit( ad : Person; region : Region_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      -- probit adl_b_w adlb_diff adlb_vdiff adlb_help $agepoly $healthvars if sex==1 & age>=65 & wales==1 & adlb_not==0;
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            adlb_diff      =>     -1.23663,
            adlb_vdiff     =>    -1.213934,
            adlb_help      =>   -0.8823167,
            age            =>   -0.3743259,
            age_2          =>    0.7269112,
            age_3          =>   -0.4291131,
            hl_exc         =>   -0.1014781,
            hl_good        =>    0.2345771,
            hl_poor        =>   -0.4921276,
            hl_vpoor       =>    -0.754753,
            const          =>     6.687198,
            others         =>    0.0 );
      end if;
      --
      -- probit adl_b_w adlb_diff adlb_vdiff adlb_help $agepoly $healthvars if sex==2 & age>=65 & wales==1 & adlb_not==0;
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            adlb_diff      =>    -1.298326,
            adlb_vdiff     =>   -0.7705426,
            adlb_help      =>    -1.040685,
            age            =>     3.525559,
            age_2          =>    -4.487256,
            age_3          =>     1.875422,
            hl_exc         =>   -0.0238311,
            hl_good        =>    0.3369026,
            hl_poor        =>   -0.2341562,
            hl_vpoor       =>   -0.6032454,
            const          =>     -89.9451,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end ADL_B_Better_Probit;
   
   function ADL_C_Worse_Probit( ad : Person; region : Region_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      -- probit adl_b_w adlb_diff adlb_vdiff adlb_help $agepoly $healthvars if sex==1 & age>=65 & wales==1 & adlb_not==0;
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            adlc_diff      =>     0.047574,
            adlc_vdiff     =>   -0.0743515,
            adlc_help      =>   -0.5560735,
            age            =>     2.464851,
            age_2          =>    -3.360076,
            age_3          =>     1.524178,
            hl_exc         =>   -0.6092279,
            hl_good        =>   -0.3926262,
            hl_poor        =>    0.5102472,
            hl_vpoor       =>    0.4484688,
            const          =>    -61.81636,
            others         =>    0.0 );
      end if;
      --
      -- probit adl_b_w adlb_diff adlb_vdiff adlb_help $agepoly $healthvars if sex==2 & age>=65 & wales==1 & adlb_not==0;
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            adlc_diff      =>      0.21119,
            adlc_vdiff     =>   -0.8971672,
            adlc_help      =>    -0.825157,
            age            =>   -0.2890511,
            age_2          =>    0.3772253,
            age_3          =>   -0.1541403,
            hl_exc         =>   -0.8805639,
            hl_good        =>   -0.5726053,
            hl_poor        =>    0.3741695,
            hl_vpoor       =>    0.1286304,
            const          =>     5.511041,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end ADL_C_Worse_Probit;

   function ADL_C_Better_Probit( ad : Person; region : Region_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      --  probit adl_c_b adlc_diff adlc_vdiff adlc_help $agepoly $healthvars if sex==1 & age>=65 & wales==1 & adlc_none==0 & adl_c_w==0;
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            adlc_diff      =>    -1.275036,
            adlc_vdiff     =>   -0.3148584,
            adlc_help      =>    -1.398165,
            age            =>    -5.891644,
            age_2          =>     7.836061,
            age_3          =>    -3.463556,
            hl_exc         =>    0.0033631,
            hl_good        =>    0.4665581,
            hl_poor        =>   -0.2930744,
            hl_vpoor       =>    -0.347159,
            const          =>     148.3742,
            others         =>    0.0 );
      end if;
      --
      -- probit adl_d_w adld_diff adld_vdiff adld_help $agepoly $healthvars if sex==1 & age>=65 & wales==1 & adld_not==0;
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            adlc_diff      =>    -1.263861,
            adlc_vdiff     =>   -0.6031271,
            adlc_help      =>   -0.9315262,
            age            =>     1.841937,
            age_2          =>    -2.326927,
            age_3          =>    0.9589613,
            hl_exc         =>    -0.031551,
            hl_good        =>    0.4746987,
            hl_poor        =>   -0.1083471,
            hl_vpoor       =>   -0.2870241,
            const          =>    -46.68282,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end ADL_C_Better_Probit;

   function ADL_D_Worse_Probit( ad : Person; region : Region_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      -- probit adl_d_w adld_diff adld_vdiff adld_help $agepoly $healthvars if sex==1 & age>=65 & wales==1 & adld_not==0;
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            adld_diff      =>    0.5453275,
            adld_vdiff     =>    -0.039225,
            adld_help      =>    0.2868695,
            age            =>     1.969465,
            age_2          =>    -2.537087,
            age_3          =>     1.094957,
            hl_exc         =>   -0.3701681,
            hl_good        =>   -0.2638427,
            hl_poor        =>     0.073042,
            hl_vpoor       =>    0.0089845,
            const          =>    -52.27996,
            others         =>    0.0 );
      end if;
      --
      -- probit adl_d_w adld_diff adld_vdiff adld_help $agepoly $healthvars if sex==2 & age>=65 & wales==1 & adld_not==0;
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            adld_diff      =>    0.5230302,
            adld_vdiff     =>    0.6732227,
            adld_help      =>    0.4174251,
            age            =>    -1.956351,
            age_2          =>     2.593292,
            age_3          =>    -1.133506,
            hl_exc         =>   -0.3919565,
            hl_good        =>   -0.2732457,
            hl_poor        =>    0.0093173,
            hl_vpoor       =>    0.0074805,
            const          =>     47.61008,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end ADL_D_Worse_Probit;

   function ADL_D_Better_Probit( ad : Person; region : Region_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      -- probit adl_d_b adld_diff adld_vdiff adld_help $agepoly $healthvars if sex==1 & age>=65 & wales==1 & adld_none==0 & adl_d_w==0;
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            adld_diff      =>   -0.7214668,
            adld_vdiff     =>   -0.0697984,
            adld_help      =>    -1.039588,
            age            =>    -3.330334,
            age_2          =>     4.389988,
            age_3          =>    -1.940659,
            hl_exc         =>    0.4088496,
            hl_good        =>    0.3612301,
            hl_poor        =>   -0.2866144,
            hl_vpoor       =>   -0.7222784,
            const          =>     84.87789,
            others         =>    0.0 );
      end if;
      --
      -- probit adl_e_w adle_diff adle_vdiff adle_help $agepoly $healthvars if sex==1 & age>=65 & wales==1 & adle_not==0;
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
          coeffs := (
            adld_diff      =>    -0.671897,
            adld_vdiff     =>   -0.3103444,
            adld_help      =>   -0.9698539,
            age            =>    0.5704462,
            age_2          =>   -0.7597814,
            age_3          =>    0.3143867,
            hl_exc         =>    0.3496369,
            hl_good        =>    0.3627668,
            hl_poor        =>   -0.1659142,
            hl_vpoor       =>   -0.5488742,
            const          =>    -13.34299,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end ADL_D_Better_Probit;


   function ADL_E_Worse_Probit( ad : Person; region : Region_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      -- probit adl_e_w adle_diff adle_vdiff adle_help $agepoly $healthvars if sex==1 & age>=65 & wales==1 & adle_not==0;
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            adle_diff      =>    0.0615758,
            adle_vdiff     =>   -0.0995681,
            adle_help      =>    -0.527721,
            age            =>     1.903688,
            age_2          =>    -2.359457,
            age_3          =>    0.9822898,
            hl_exc         =>   -0.6186399,
            hl_good        =>   -0.4454671,
            hl_poor        =>    0.4034456,
            hl_vpoor       =>    0.4526177,
            const          =>    -52.87758,
            others         =>    0.0 );
      end if;
      --
      -- probit adl_e_w adle_diff adle_vdiff adle_help $agepoly $healthvars if sex==2 & age>=65 & wales==1 & adle_not==0;
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            adle_diff      =>    0.1268597,
            adle_vdiff     =>   -0.1404242,
            adle_help      =>   -0.8950266,
            age            =>    -2.229969,
            age_2          =>     2.818777,
            age_3          =>    -1.159849,
            hl_exc         =>    -1.086616,
            hl_good        =>    -0.716067,
            hl_poor        =>    0.2067264,
            hl_vpoor       =>    0.1052902,
            const          =>     56.44168,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end ADL_E_Worse_Probit;


   function ADL_E_Better_Probit( ad : Person; region : Region_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      -- probit adl_e_b adle_diff adle_vdiff adle_help $agepoly $healthvars if sex==1 & age>=65 & wales==1 & adle_none==0 & adl_e_w==0;
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            adle_diff      =>   -0.9716658,
            adle_vdiff     =>   -0.8424179,
            adle_help      =>    -1.325088,
            age            =>     1.635112,
            age_2          =>    -1.822622,
            age_3          =>    0.6352476,
            hl_exc         =>    0.1221096,
            hl_good        =>      0.27708,
            hl_poor        =>   -0.4126191,
            hl_vpoor       =>   -0.9489926,
            const          =>     -45.6086,
            others         =>    0.0 );
      end if;
      --
      --  probit adl_e_b adle_diff adle_vdiff adle_help $agepoly $healthvars if sex==2 & age>=65 & wales==1 & adle_none==0 & adl_e_w==0;
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
          coeffs := (
            adle_diff      =>    -1.163807,
            adle_vdiff     =>   -0.7570822,
            adle_help      =>    -1.475993,
            age            =>     2.162794,
            age_2          =>     -2.71739,
            age_3          =>     1.114612,
            hl_exc         =>    0.2065566,
            hl_good        =>    0.4271005,
            hl_poor        =>   -0.2609373,
            hl_vpoor       =>   -0.3416168,
            const          =>    -55.31896,
            others         =>    0.0 );
     end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end ADL_E_Better_Probit;
   
   function Get_ADL_Change_Probit( ad : Person; which : Task_Type; direction : Change_Direction_Type; region : Region_Type ) return Amount is
      p : Amount := 0.0;
    begin
      if( ad.age < 65 )then
         return -99999.99;
      end if;
      case which is
      when manage_stairs =>
         if( direction = worsening )then
            p := ADL_A_Worse_Probit( ad,region );
         else
            p := ADL_A_Better_Probit( ad,region );
         end if;
      when get_around_house =>
         if( direction = worsening )then
            p := ADL_B_Worse_Probit( ad,region );
         else
            p := ADL_B_Better_Probit( ad,region );
         end if;
      when get_in_or_out_of_bed =>
         if( direction = worsening )then
            p := ADL_C_Worse_Probit( ad,region );
         else
            p := ADL_C_Better_Probit( ad,region );
         end if;
      when cut_toenails =>
         if( direction = worsening )then
            p := ADL_D_Worse_Probit( ad,region );
         else
            p := ADL_D_Better_Probit( ad,region );
         end if;
      when bathing_or_showering =>
         if( direction = worsening )then
            p := ADL_E_Worse_Probit( ad,region );
         else
            p := ADL_E_Better_Probit( ad,region );
         end if;
      when walk_down_road =>
         null;
      end case;
      return p;
   end Get_Adl_Change_Probit;
   
   function Dies_This_Period_Probit( ad : Person; region : Region_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array  := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      --  > probit dead $agedums $survars2 if age>=65 & wales==1 & sex==1;
      --  updated
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            age            =>    -2.492184,
            age_2          =>      3.20898,
            age_3          =>    -1.344959,
            hl_exc         =>   -0.2429036,
            hl_good        =>     -0.50586,
            hl_poor        =>     0.152715,
            hl_vpoor       =>    0.7425185,
            const          =>     61.27476,
            others         =>    0.0 );
      end if;
      --
      --
      --
      --  > probit dead  $agedums $survars2 if age>=65 & wales==1 & sex==2;
      --  updated
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            age            =>    -4.079929,
            age_2          =>     5.285728,
            age_3          =>    -2.240773,
            hl_exc         =>   -0.2081626,
            hl_good        =>   -0.2236565,
            hl_poor        =>    0.3703909,
            hl_vpoor       =>    0.6619636,
            const          =>     100.9834,
            others         =>    0.0 );
      end if;
      --
      --      
      p := Vector_Product( regressors, coeffs );
      return p;   
   end Dies_This_Period_Probit;
   
   
   function HH_Split_Probit( ad : Person; region : Region_Type; wave : Waves ) return Amount is
      p : Amount;
      coeffs : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      --  > probit separate female age trend $tendums married if L.pardead==0 & spinhh==2 & age
      --  > >=65 & wales==1 & year<=2007;
      -- FIXME AND PARTNER NOT DEAD
      if( region = wales and ad.age >= 65 )then
         coeffs := (
            female         =>   -0.4124872,
            age            =>    0.0164925,
            owned          =>    0.4200651,
            socrent        =>   -0.4434248,
            married        =>     2.712241,
            const          =>    -4.243692,
            others         =>    0.0 );
      end if;
      --
      --
      p := Vector_Product( regressors, coeffs );
      return p;   
   end HH_Split_Probit;
   

   function Working_Probit( ad : Person; region : Region_Type; wave : Waves ) return Amount is
      p : Amount;
      coeffs : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      --  > probit separate female age trend $tendums married if L.pardead==0 & spinhh==2 & age
      --  > >=65 & wales==1 & year<=2007;
      -- FIXME AND PARTNER NOT DEAD
      if( region = wales and ad.age >= 40 and ad.age < 85 and ad.sex = male )then
         coeffs := (
            age4044        =>    0.8492586,
            age4549        =>    0.8517021,
            age5054        =>     0.683094,
            age6064        =>   -0.5846137,
            age6569        =>    -1.853757,
            age7074        =>    -1.933281,
            age7579        =>     -2.08306,
            age8084        =>    -2.558109,
            hl_exc         =>    0.6224952,
            hl_good        =>    0.5480313,
            hl_poor        =>   -0.8380233,
            hl_vpoor       =>    -1.640117,
            spinhh         =>   -0.4411138,
            owned          =>    0.6001516,
            hq_deg         =>      0.26542,
            hq_alev        =>    0.0562359,
            hq_gcse        =>    0.3809759,
            hq_oth         =>    0.2932929,
            const          =>   -0.1061111,
            others         =>    0.0 );

      end if;
      if( region = wales and ad.age >= 40 and ad.age < 85 and ad.sex = female )then
         coeffs := (
            age4044        =>    0.7676859,
            age4549        =>    0.7242306,
            age5054        =>    0.4734135,
            age6064        =>   -0.6310589,
            age6569        =>    -1.598737,
            age7074        =>    -2.124655,
            age7579        =>    -2.420393,
            age8084        =>    -2.602289,
            hl_exc         =>    0.5258602,
            hl_good        =>    0.3548843,
            hl_poor        =>   -0.7074901,
            hl_vpoor       =>    -1.299928,
            spinhh         =>   -0.0644706,
            owned          =>     0.390255,
            hq_deg         =>     0.598068,
            hq_alev        =>     0.427773,
            hq_gcse        =>    0.2440916,
            hq_oth         =>    0.3833893,
            const          =>    -0.807535,
            others         =>    0.0 );
      end if;
      --
      --
      p := Vector_Product( regressors, coeffs );
      return p;   
   end Working_Probit;
   
   
   function Rent_To_Own_Probit( ad : Person; region : Region_Type; wave : Waves ) return Amount is
      p : Amount;
      coeffs : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
         coeffs := (
            female         =>   -0.0765562,
            age            =>   -0.0095098,
            hhkids         =>    0.0728611,
            spinhh         =>   -0.1803964,
            const          =>   -0.8891921,
            others         =>    0.0 );
      --
      --
      p := Vector_Product( regressors, coeffs );
      return p;   
   end Rent_To_Own_Probit;
   
   function Own_To_Rent_Probit( ad : Person; region : Region_Type; wave : Waves ) return Amount is
      p : Amount;
      coeffs : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      --
      --
         coeffs := (
            female         =>   -0.0018696,
            age            =>   -0.0082941,
            hhkids         =>   -0.0059132,
            spinhh         =>    0.2398212,
            const          =>    -2.246895,
            others         =>    0.0 );
      p := Vector_Product( regressors, coeffs );
      return p;   
   end Own_To_Rent_Probit;
   
   
   function Retire_Probit( ad : Person; region : Region_Type; wave : Waves ) return Amount is
      p : Amount;
      coeffs : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      -- obit retire owned $agepoly age59 $healthvars trend spinhh $education if wales==1 & sex==2 & age>=40 
         -- FIXME AND PARTNER NOT DEAD
       --   and wave < r
       -- and working = 1
      if( region = wales and ad.age >= 40 and ad.age < 65 and ad.sex = male )then
         coeffs := (
            owned          =>    -0.141975,
            age            =>    -0.790951,
            age_2          =>     1.446766,
            age_3          =>   -0.8482181,
            age64          =>     1.140505,
            hl_exc         =>    0.1074586,
            hl_good        =>   -0.0230127,
            hl_poor        =>    0.5403226,
            hl_vpoor       =>     1.198591,
            spinhh         =>    0.2506615,
            hq_deg         =>   -0.1312051,
            hq_alev        =>   -0.0035627,
            hq_gcse        =>    0.0207223,
            hq_oth         =>    0.0667818,
            const          =>     12.33842,
            others         =>    0.0 );
      end if;
      if( region = wales and ad.age >= 65 and ad.sex = male )then
         coeffs := (
            owned          =>    0.5201504,
            age            =>     59.30154,
            age_2          =>     -83.0767,
            age_3          =>     38.67865,
            hl_exc         =>    -1.230953,
            hl_good        =>    -1.248023,
            hl_poor        =>    0.0665117,
            spinhh         =>    0.6433052,
            hq_deg         =>    0.3447561,
            hq_alev        =>     0.682287,
            hq_gcse        =>     1.471594,
            hq_oth         =>    0.4641729,
            const          =>    -1408.087,
            others         =>    0.0 );
      end if;
      -- and wave < r 
       -- and working = 1
      if( region = wales and ad.age >= 40 and ad.age < 65 and ad.sex = female )then
         coeffs := (
            owned          =>   -0.0463216,
            age            =>    0.2969357,
            age_2          =>   -0.8590443,
            age_3          =>    0.7709355,
            age59          =>    0.2341594,
            hl_exc         =>   -0.1622239,
            hl_good        =>   -0.1520454,
            hl_poor        =>    0.1413534,
            hl_vpoor       =>    0.6699312,
            trend          =>    -0.021694,
            spinhh         =>    0.0074254,
            hq_deg         =>   -0.1021852,
            hq_alev        =>    0.0046985,
            hq_gcse        =>   -0.1297435,
            hq_oth         =>    0.0729346,
            const          =>    -4.032189,
            others         =>    0.0 );
      end if;
      if( region = wales and ad.age >= 65 and ad.sex = female )then
         coeffs := (
            owned          =>    0.4331502,
            age            =>     28.66106,
            age_2          =>    -39.33429,
            age_3          =>     18.03173,
            hl_exc         =>   -0.4472235,
            hl_good        =>    -1.821393,
            spinhh         =>    0.1379992,
            hq_deg         =>    -0.722979,
            hq_alev        =>   -0.4984142,
            hq_oth         =>    -1.827921,
            const          =>    -695.6117,
            others         =>    0.0 );
      end if;
      --
      --
      p := Vector_Product( regressors, coeffs );
      return p;   
   end Retire_Probit;
   
   function Reenter_Work_Probit( ad : Person; region : Region_Type; wave : Waves ) return Amount is
      p : Amount;
      coeffs : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      -- obit retire owned $agepoly age59 $healthvars trend spinhh $education if wales==1 & sex==2 & age>=40 
         -- FIXME AND PARTNER NOT DEAD
       --   and working = 0 
      if( region = wales and ad.age >= 40 and ad.age < 65 and ad.sex = male )then
         coeffs := (
            owned          =>    0.5680574,
            age            =>    0.2309786,
            age_2          =>   -0.0473416,
            age_3          =>   -0.3012084,
            hl_exc         =>   -0.0187036,
            hl_good        =>    0.1473335,
            hl_poor        =>   -0.5238021,
            hl_vpoor       =>   -0.7165665,
            spinhh         =>    0.0112477,
            hq_deg         =>    0.3880229,
            hq_alev        =>   -0.1037714,
            hq_gcse        =>    0.6170832,
            hq_oth         =>    0.5777199,
            const          =>     -8.26991,
            others         =>    0.0 );
      end if;
      --  > probit reenter owned $agepoly $healthvars spinhh $education if wales==1 & sex==2 & age>=40 
      --  > & age<65 & working==0 & year>1999 & year<2008;
      -- and wave < r 
      if( region = wales and ad.age >= 40 and ad.age < 65 and ad.sex = female )then
         coeffs := (
            owned          =>    0.1477839,
            age            =>    -0.324075,
            age_2          =>     0.754688,
            age_3          =>   -0.6121185,
            hl_exc         =>    0.2227109,
            hl_good        =>    0.3155928,
            hl_poor        =>    -0.517561,
            hl_vpoor       =>    -0.210889,
            spinhh         =>    0.0575561,
            hq_deg         =>    0.4961927,
            hq_alev        =>    0.4020617,
            hq_gcse        =>    0.2731368,
            hq_oth         =>    0.3242441,
            const          =>     3.111353,
            others         =>    0.0 );
      end if;
      --
      --
      p := Vector_Product( regressors, coeffs );
      return p;   
   end Reenter_Work_Probit;


   --
   -- NOT USED
   --
   function Health_Regression( ad : Person; region : Region_Type ) return Regression_Results is
      coeffs     : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
      r          : Regression_Results;
   begin
      --
      --  > regress healthch healthsc adlscore $agedums if sex==1 & age>=65 & wales==1;
      --  
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            healthsc       =>    -0.438247,
            adlscore       =>    0.0271551,
            age7074        =>    0.0006823,
            age7579        =>   -0.0007128,
            age8084        =>   -0.0149752,
            age8589        =>    0.0359743,
            age90          =>    0.0021677,
            trend          =>   -0.0072449,
            const          =>    0.9891843,
            others         =>    0.0 );
         r.sd := 0.693737;
      end if;
      --
      --  . regress healthch healthsc adlscore $agedums if sex==2 & age>=65 & wales==1;
      --  
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            healthsc       =>    -0.420551,
            adlscore       =>    0.0231533,
            age7074        =>   -0.0231008,
            age7579        =>   -0.0054609,
            age8084        =>   -0.1005908,
            age8589        =>   -0.0868427,
            age90          =>   -0.0836518,
            trend          =>   -0.0084437,
            const          =>     1.000898,
            others         =>    0.0 );
         r.sd := 0.7097944;
      end if;
      --
      --
      r.vp := Vector_Product( regressors, coeffs );
      return r;   
   end Health_Regression;
   
   
   
   
   --
   -- NOT USED
   --
   function Change_In_ADL_Regression( ad : Person; region : Region_Type ) return Regression_Results is
      coeffs     : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
      r          : Regression_Results;
   begin
      --
      --  > regress chadlsc adlscore $agedums $healthvars trend if sex==1 & age>=65 & wales==1;
      --  
      --
      if( ad.sex = male and ad.age >= 65 and region = wales )then
         coeffs := (
            adlscore       =>   -0.2792694,
            age7074        =>   -0.0818354,
            age7579        =>      0.37014,
            age8084        =>   -0.1990031,
            age8589        =>     0.182757,
            age90          =>     2.291449,
            hl_exc         =>   -0.2041206,
            hl_good        =>   -0.2411988,
            hl_poor        =>     1.943719,
            hl_vpoor       =>     2.051984,
            trend          =>   -0.0415517,
            const          =>    0.7953608,
            others         =>    0.0 );
         r.sd := 3.868221;
      end if;
      --
      --
      --
      --  . regress chadlsc adlscore $agedums $healthvars trend if sex==2 & age>=65 & wales==1;
      --  
      --
      if( ad.sex = female and ad.age >= 65 and region = wales )then
         coeffs := (
            adlscore       =>   -0.3039989,
            age7074        =>    0.0366933,
            age7579        =>     0.582343,
            age8084        =>     1.119166,
            age8589        =>     1.487792,
            age90          =>     1.606643,
            hl_exc         =>    -1.067354,
            hl_good        =>    -1.200362,
            hl_poor        =>   -0.0694572,
            hl_vpoor       =>    -0.130375,
            trend          =>   -0.0196212,
            const          =>     1.565978,
            others         =>    0.0 );
         r.sd := 5.071933;
      end if;
      --
      --
      r.vp := Vector_Product( regressors, coeffs );
      return r;   -- sd
   end Change_In_ADL_Regression;
   
   
   function Recieving_Any_Care_Probit( 
      ad                    : Person; 
      receiving_last_period : Boolean;
      use_lags              : Boolean ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      if receiving_last_period then
         regressors( lagged_depvar ) := 1.0;
      else
         regressors( lagged_depvar ) := 0.0;
      end if;

      if( use_lags )then
         if( ad.sex = male and ad.age >= 65 )then
            coeffs := (
               lagged_depvar  =>     1.113299,
               age            =>   -0.0024927,
               age_2          =>   -0.0444034,
               age_3          =>    0.0587361,
               adla_diff      =>    0.1031856,
               adla_vdiff     =>    0.0765329,
               adla_help      =>    0.2228683,
               adla_not       =>    0.1321765,
               adlb_diff      =>    0.0352899,
               adlb_vdiff     =>    0.1918376,
               adlb_help      =>    0.3319053,
               adlb_not       =>      0.37345,
               adld_diff      =>    0.0638833,
               adld_vdiff     =>   -0.0376499,
               adld_help      =>    0.2376222,
               adld_not       =>    0.2797535,
               adle_diff      =>     0.146274,
               adle_vdiff     =>    0.1364714,
               adle_help      =>    0.2107118,
               adle_not       =>    0.2229452,
               hl_exc         =>   -0.3432539,
               hl_good        =>   -0.1654704,
               hl_poor        =>    0.4436546,
               hl_vpoor       =>    0.7166037,
               reg_il         =>     0.146361,
               reg_ol         =>   -0.2196307,
               reg_se         =>   -0.1370266,
               reg_sw         =>    0.0517552,
               reg_ee         =>   -0.1574931,
               reg_em         =>    0.0367747,
               reg_wmc        =>   -0.1981327,
               reg_wmo        =>    0.0505628,
               reg_gm         =>   -0.0860488,
               reg_me         =>   -0.0193508,
               reg_nwo        =>   -0.1210572,
               reg_sy         =>     0.287307,
               reg_wy         =>   -0.1861734,
               reg_yho        =>   -0.1820206,
               reg_tw         =>   -0.0616995,
               reg_no         =>    0.0734834,
               reg_S          =>      0.17709,
               reg_NI         =>    0.0266204,
               working        =>   -0.1731759,
               spinhh         =>    0.1209785,
               const          =>    -1.458458,
               others         =>    0.0 );
         end if;
         --
         --
         --
         --  . probit anycare L.anycare $agedums $adlall $healthvars $regdums working spinhh if age>=65 & sex==2;
   
         --  
   
         --
         if( ad.sex = female and ad.age >= 65 )then
            coeffs := (
               lagged_depvar  =>      1.11883,
               age            =>    -1.033323,
               age_2          =>     1.367144,
               age_3          =>   -0.5791777,
               adla_diff      =>    0.1101685,
               adla_vdiff     =>    0.0394376,
               adla_help      =>    0.2336695,
               adla_not       =>    0.1996065,
               adlb_diff      =>   -0.0453825,
               adlb_vdiff     =>    0.2574404,
               adlb_help      =>   -0.2892933,
               adlb_not       =>   -0.2561102,
               adld_diff      =>    0.0795955,
               adld_vdiff     =>   -0.1041503,
               adld_help      =>    0.1975468,
               adld_not       =>    0.2393239,
               adle_diff      =>    0.0624132,
               adle_vdiff     =>    0.4536753,
               adle_help      =>    0.3796752,
               adle_not       =>     0.408778,
               hl_exc         =>   -0.5566003,
               hl_good        =>    -0.252545,
               hl_poor        =>    0.3415891,
               hl_vpoor       =>    0.6071852,
               reg_il         =>   -0.0493304,
               reg_ol         =>   -0.0909931,
               reg_se         =>   -0.1140054,
               reg_sw         =>   -0.0019768,
               reg_ee         =>   -0.2156496,
               reg_em         =>    0.1479866,
               reg_wmc        =>   -0.2443022,
               reg_wmo        =>   -0.0289064,
               reg_gm         =>   -0.0858303,
               reg_me         =>    0.0682282,
               reg_nwo        =>   -0.1505266,
               reg_sy         =>   -0.3239975,
               reg_wy         =>   -0.1877987,
               reg_yho        =>   -0.0966364,
               reg_tw         =>    0.0783228,
               reg_no         =>   -0.1293337,
               reg_S          =>     0.188654,
               reg_NI         =>   -0.0158645,
               working        =>   -0.0781167,
               spinhh         =>     0.156489,
               const          =>     23.44276,
               others         =>    0.0 );
         end if;
      else
         
         if( ad.sex = male and ad.age >= 65 )then
            coeffs := (
               age            =>   -0.4083138,
               age_2          =>    0.4621776,
               age_3          =>   -0.1443058,
               adla_diff      =>    0.1367391,
               adla_vdiff     =>    0.1289351,
               adla_help      =>    0.3411937,
               adla_not       =>    0.3260469,
               adlb_diff      =>    0.0136536,
               adlb_vdiff     =>    0.0912779,
               adlb_help      =>    0.1934891,
               adlb_not       =>    0.2379997,
               adld_diff      =>    0.0994489,
               adld_vdiff     =>    0.1450013,
               adld_help      =>    0.3395945,
               adld_not       =>    0.3914033,
               adle_diff      =>    0.0989757,
               adle_vdiff     =>     0.054198,
               adle_help      =>    0.3407987,
               adle_not       =>    0.2956014,
               hl_exc         =>   -0.4005597,
               hl_good        =>   -0.2098101,
               hl_poor        =>    0.4252782,
               hl_vpoor       =>      0.68877,
               reg_il         =>    0.0422853,
               reg_ol         =>   -0.2625283,
               reg_se         =>   -0.1826559,
               reg_sw         =>      0.04703,
               reg_ee         =>   -0.3221175,
               reg_em         =>    0.0186503,
               reg_wmc        =>   -0.1592578,
               reg_wmo        =>   -0.0167243,
               reg_gm         =>   -0.1188675,
               reg_me         =>   -0.2657258,
               reg_nwo        =>    -0.137325,
               reg_sy         =>    0.2332087,
               reg_wy         =>   -0.3043914,
               reg_yho        =>   -0.3294494,
               reg_tw         =>    0.0019607,
               reg_no         =>    0.0334917,
               reg_S          =>    0.1565813,
               reg_NI         =>     0.062238,
               working        =>   -0.1792243,
               spinhh         =>    0.1691753,
               const          =>     9.165955,
               others         =>    0.0 );
         end if;
         --
         --
         --
         --  . probit anycare $agedums $adlall $healthvars $regdums working spinhh if age>=65 & sex==2;
   
         --  
   
         --
         if( ad.sex = female and ad.age >= 65 )then
            coeffs := (
               age            =>   -0.9069279,
               age_2          =>     1.183109,
               age_3          =>   -0.4850877,
               adla_diff      =>    0.1044108,
               adla_vdiff     =>    0.0637544,
               adla_help      =>    0.3081578,
               adla_not       =>    0.3493789,
               adlb_diff      =>    0.0686009,
               adlb_vdiff     =>    0.3144646,
               adlb_help      =>    -0.190963,
               adlb_not       =>   -0.4221459,
               adld_diff      =>    0.1338332,
               adld_vdiff     =>   -0.0563494,
               adld_help      =>    0.2841863,
               adld_not       =>    0.3351077,
               adle_diff      =>     0.100272,
               adle_vdiff     =>    0.3654431,
               adle_help      =>    0.4586386,
               adle_not       =>    0.5292477,
               hl_exc         =>   -0.6650956,
               hl_good        =>   -0.3185917,
               hl_poor        =>    0.3588939,
               hl_vpoor       =>    0.6399779,
               reg_il         =>   -0.1914805,
               reg_ol         =>   -0.0777092,
               reg_se         =>   -0.1332385,
               reg_sw         =>   -0.0118495,
               reg_ee         =>   -0.3292817,
               reg_em         =>    0.1921235,
               reg_wmc        =>   -0.2855126,
               reg_wmo        =>   -0.1029237,
               reg_gm         =>   -0.0781346,
               reg_me         =>   -0.0422126,
               reg_nwo        =>   -0.1463265,
               reg_sy         =>   -0.2802055,
               reg_wy         =>   -0.1799814,
               reg_yho        =>   -0.1650388,
               reg_tw         =>    0.1353722,
               reg_no         =>   -0.1301584,
               reg_S          =>     0.256796,
               reg_NI         =>    0.1223966,
               working        =>   -0.1333214,
               spinhh         =>    0.1911544,
               const          =>      20.4828,
               others         =>    0.0 );
         end if;
      end if;
      --
      --
      p := Vector_Product( regressors, coeffs );
      return p;   -- sd
   end Recieving_Any_Care_Probit;
   
   
   function Recieving_Care_Probit( 
      ad  : Person; 
      ct  : Care_Type ) return Amount is
      p          : Amount;
      coeffs     : Coeffs_Array := ( others => 0.0 );
      regressors : Regressors_Array := ad.regressors;
   begin
      if( ad.age < 65 )then
         return -99999.99;
      end if;
      --
      --  > probit hlsva $agedums $adlall $healthvars $regdums if age>=65 & sex==1 & hlsva>=0;
      --  
      --
      case ct is
         when health_visitor =>
            
            if( ad.sex = male and ad.age >= 65 )then
               coeffs := (
                  age            =>    0.1574299,
                  age_2          =>   -0.1841313,
                  age_3          =>    0.0881311,
                  adla_diff      =>    0.0617596,
                  adla_vdiff     =>    0.1497347,
                  adla_help      =>    0.2805762,
                  adla_not       =>    0.2203055,
                  adlb_diff      =>     0.104962,
                  adlb_vdiff     =>    -0.064326,
                  adlb_help      =>    0.3214192,
                  adlb_not       =>    0.5491068,
                  adlc_diff      =>   -0.1194842,
                  adlc_vdiff     =>   -0.2000554,
                  adlc_help      =>   -0.1669624,
                  adlc_not       =>   -0.6519071,
                  adld_diff      =>    0.0083226,
                  adld_vdiff     =>    0.2641673,
                  adld_help      =>    0.2573032,
                  adld_not       =>    0.3202983,
                  adle_diff      =>    0.0843041,
                  adle_vdiff     =>    0.0898978,
                  adle_help      =>    0.4073026,
                  adle_not       =>    0.5173173,
                  hl_exc         =>   -0.3669851,
                  hl_good        =>   -0.2320973,
                  hl_poor        =>    0.4381016,
                  hl_vpoor       =>    0.6325601,
                  reg_il         =>    0.0715166,
                  reg_ol         =>   -0.3090641,
                  reg_se         =>   -0.0668112,
                  reg_sw         =>     0.190726,
                  reg_ee         =>   -0.2863383,
                  reg_em         =>    0.0615167,
                  reg_wmc        =>   -0.1770401,
                  reg_wmo        =>    0.1434761,
                  reg_gm         =>    0.0148879,
                  reg_me         =>     0.003866,
                  reg_nwo        =>   -0.0669826,
                  reg_sy         =>    0.4553311,
                  reg_wy         =>   -0.2041155,
                  reg_yho        =>   -0.2524465,
                  reg_tw         =>    0.2382199,
                  reg_no         =>    0.1815029,
                  reg_S          =>    0.2727351,
                  reg_NI         =>   -0.0230549,
                  const          =>    -6.680095,
                  others         =>    0.0 );
            end if;
            --
            --  . probit hlsva $agedums $adlall $healthvars $regdums if age>=65 & sex==2 & hlsva>=0;
            --
            if( ad.sex = female and ad.age >= 65 )then
               coeffs := (
                  age            =>    -0.732472,
                  age_2          =>     1.026132,
                  age_3          =>    -0.453807,
                  adla_diff      =>    0.0473263,
                  adla_vdiff     =>    0.0408905,
                  adla_help      =>    0.2194046,
                  adla_not       =>    0.2433379,
                  adlb_diff      =>    0.1488446,
                  adlb_vdiff     =>    0.3301034,
                  adlb_help      =>   -0.0623041,
                  adlb_not       =>   -0.2888066,
                  adlc_diff      =>   -0.0147347,
                  adlc_vdiff     =>    0.0784092,
                  adlc_help      =>    0.1657699,
                  adlc_not       =>    0.1294446,
                  adld_diff      =>    0.0444928,
                  adld_vdiff     =>   -0.0803758,
                  adld_help      =>    0.1397389,
                  adld_not       =>      0.25181,
                  adle_diff      =>    0.0449892,
                  adle_vdiff     =>     0.324938,
                  adle_help      =>    0.3801779,
                  adle_not       =>    0.4833318,
                  hl_exc         =>   -0.5917948,
                  hl_good        =>   -0.2917983,
                  hl_poor        =>    0.3527981,
                  hl_vpoor       =>    0.6601866,
                  reg_il         =>   -0.0502395,
                  reg_ol         =>   -0.1857816,
                  reg_se         =>   -0.1936217,
                  reg_sw         =>    0.0898773,
                  reg_ee         =>   -0.2657989,
                  reg_em         =>    0.2060726,
                  reg_wmc        =>   -0.2093028,
                  reg_wmo        =>   -0.0830525,
                  reg_gm         =>   -0.3829876,
                  reg_me         =>   -0.2210881,
                  reg_nwo        =>   -0.2278174,
                  reg_sy         =>   -0.1602847,
                  reg_wy         =>   -0.2131548,
                  reg_yho        =>   -0.1443976,
                  reg_tw         =>     0.180104,
                  reg_no         =>   -0.0462135,
                  reg_S          =>    0.2268254,
                  reg_NI         =>   -0.0497985,
                  const          =>     15.06607,
                  others         =>    0.0 );
            end if;
         when home_help =>
            --
            --  > probit hlsvb $agedums $adlall $healthvars $regdums if age>=65  & sex==1 & hlsvb>=0;
            --
            if( ad.sex = male and ad.age >= 65 )then
               coeffs := (
                  age            =>    -2.246381,
                  age_2          =>     2.797387,
                  age_3          =>    -1.117517,
                  adla_diff      =>    0.3269616,
                  adla_vdiff     =>    0.3908227,
                  adla_help      =>    0.5198492,
                  adla_not       =>    0.6198497,
                  adlb_diff      =>    0.1122985,
                  adlb_vdiff     =>    0.1904522,
                  adlb_help      =>    0.2766279,
                  adlb_not       =>    0.1485627,
                  adlc_diff      =>    0.0435042,
                  adlc_vdiff     =>    0.3618971,
                  adlc_help      =>    -0.369545,
                  adlc_not       =>    0.0397522,
                  adld_diff      =>    0.3136777,
                  adld_vdiff     =>     0.158525,
                  adld_help      =>     0.280081,
                  adld_not       =>    0.4982389,
                  adle_diff      =>   -0.0092684,
                  adle_vdiff     =>    -0.267527,
                  adle_help      =>    0.0008381,
                  adle_not       =>   -0.1350807,
                  hl_exc         =>   -0.2493467,
                  hl_good        =>   -0.0834878,
                  hl_poor        =>    0.1966567,
                  hl_vpoor       =>     0.502989,
                  reg_il         =>    -0.067902,
                  reg_ol         =>   -0.1531042,
                  reg_se         =>   -0.4008438,
                  reg_sw         =>   -0.1787374,
                  reg_ee         =>   -0.3457663,
                  reg_em         =>    0.2175526,
                  reg_wmc        =>    0.2197801,
                  reg_wmo        =>   -0.0953035,
                  reg_gm         =>    0.2523599,
                  reg_me         =>   -0.1896119,
                  reg_nwo        =>   -0.2649243,
                  reg_sy         =>   -0.1646324,
                  reg_wy         =>   -0.5281362,
                  reg_yho        =>   -0.3694871,
                  reg_tw         =>   -0.1689645,
                  reg_no         =>     0.135036,
                  reg_S          =>    0.2992228,
                  reg_NI         =>    0.4941697,
                  const          =>     55.76636,
                  others         =>    0.0 );
            end if;
            --
            --  . probit hlsvb $agedums $adlall $healthvars $regdums if age>=65 & sex==2 & hlsvb>=0;
            --
            if( ad.sex = female and ad.age >= 65 )then
               coeffs := (
                  age            =>   -0.8517953,
                  age_2          =>     1.153534,
                  age_3          =>   -0.4808649,
                  adla_diff      =>    0.1583838,
                  adla_vdiff     =>    0.2175392,
                  adla_help      =>    0.3312262,
                  adla_not       =>      0.42539,
                  adlb_diff      =>    0.0748561,
                  adlb_vdiff     =>    0.4914414,
                  adlb_help      =>   -0.1453304,
                  adlb_not       =>   -0.3697493,
                  adlc_diff      =>    0.0158137,
                  adlc_vdiff     =>   -0.1255656,
                  adlc_help      =>   -0.0940145,
                  adlc_not       =>    0.1195396,
                  adld_diff      =>    0.1228052,
                  adld_vdiff     =>   -0.0764259,
                  adld_help      =>    0.2523528,
                  adld_not       =>    0.2220084,
                  adle_diff      =>    0.1180825,
                  adle_vdiff     =>    0.2431295,
                  adle_help      =>    0.3270428,
                  adle_not       =>    0.3841383,
                  hl_exc         =>    -0.531345,
                  hl_good        =>   -0.2904125,
                  hl_poor        =>    0.1804161,
                  hl_vpoor       =>    0.3025029,
                  reg_il         =>   -0.3478436,
                  reg_ol         =>    0.1960827,
                  reg_se         =>     0.160571,
                  reg_sw         =>    0.0120738,
                  reg_ee         =>   -0.1073871,
                  reg_em         =>    0.2121873,
                  reg_wmc        =>   -0.3136044,
                  reg_wmo        =>   -0.0176115,
                  reg_gm         =>     0.312965,
                  reg_me         =>    0.4543022,
                  reg_nwo        =>   -0.0191856,
                  reg_sy         =>   -0.3430537,
                  reg_wy         =>    -0.060776,
                  reg_yho        =>    -0.103306,
                  reg_tw         =>    0.3917285,
                  reg_no         =>   -0.0189798,
                  reg_S          =>    0.5025835,
                  reg_NI         =>    0.6420703,
                  const          =>     17.25244,
                  others         =>    0.0 );
            end if;
         when meals_on_wheels =>
            --
            --  > probit hlsvc $agedums $adlall $healthvars $regdums if age>=65 & sex==1 & hlsvc>=0;
            --
            if( ad.sex = male and ad.age >= 65 )then
               coeffs := (
                  age            =>    -2.534143,
                  age_2          =>     3.225953,
                  age_3          =>    -1.326403,
                  adla_diff      =>    0.1273763,
                  adla_vdiff     =>    0.2215546,
                  adla_help      =>    0.1138269,
                  adla_not       =>    0.2265034,
                  adlb_diff      =>    0.0384285,
                  adlb_vdiff     =>   -0.0084859,
                  adlb_help      =>    0.3077666,
                  adlb_not       =>    0.0207992,
                  adlc_diff      =>   -0.1634742,
                  adlc_vdiff     =>    0.2273309,
                  adlc_help      =>   -0.3411386,
                  adlc_not       =>   -0.1488003,
                  adld_diff      =>    0.0778018,
                  adld_vdiff     =>    0.2410545,
                  adld_help      =>    0.2122584,
                  adld_not       =>    0.0143602,
                  adle_diff      =>    0.3116121,
                  adle_vdiff     =>    0.0453735,
                  adle_help      =>    0.1121996,
                  adle_not       =>     0.193578,
                  hl_exc         =>      -0.3625,
                  hl_good        =>   -0.0983311,
                  hl_poor        =>    0.1202028,
                  hl_vpoor       =>    0.4370087,
                  reg_il         =>    0.2815991,
                  reg_ol         =>    -0.964822,
                  reg_se         =>   -0.1621156,
                  reg_sw         =>   -0.3813375,
                  reg_ee         =>    -0.222355,
                  reg_em         =>   -0.1941083,
                  reg_wmc        =>   -0.0120978,
                  reg_wmo        =>   -0.2333616,
                  reg_gm         =>   -0.5153459,
                  reg_nwo        =>    0.1522653,
                  reg_sy         =>   -0.3644941,
                  reg_yho        =>   -0.3968024,
                  reg_no         =>    0.0551798,
                  reg_S          =>   -0.3369542,
                  reg_NI         =>    0.2548651,
                  const          =>     62.24198,
                  others         =>    0.0 );
            end if;
            --
            --  . probit hlsvc $agedums $adlall $healthvars $regdums if age>=65 & sex==2 & hlsvc>=0;
            --
            if( ad.sex = female and ad.age >= 65 )then
               coeffs := (
                  age            =>     2.046147,
                  age_2          =>    -2.549221,
                  age_3          =>     1.075735,
                  adla_diff      =>   -0.0185267,
                  adla_vdiff     =>   -0.0027414,
                  adla_help      =>   -0.0060041,
                  adla_not       =>    0.0246074,
                  adlb_diff      =>    0.0475065,
                  adlb_vdiff     =>     0.054804,
                  adlb_help      =>   -0.0331827,
                  adlb_not       =>   -0.5619159,
                  adlc_diff      =>    0.0664817,
                  adlc_vdiff     =>    -0.025831,
                  adlc_help      =>    0.0197225,
                  adlc_not       =>   -0.3510476,
                  adld_diff      =>     0.189274,
                  adld_vdiff     =>   -0.0786783,
                  adld_help      =>    0.1293511,
                  adld_not       =>    0.1166603,
                  adle_diff      =>    0.0818385,
                  adle_vdiff     =>    0.0815689,
                  adle_help      =>    0.3091879,
                  adle_not       =>    0.4172092,
                  hl_exc         =>   -0.6360827,
                  hl_good        =>   -0.2321472,
                  hl_poor        =>    0.1612871,
                  hl_vpoor       =>     0.436197,
                  reg_ol         =>    -0.012249,
                  reg_se         =>   -0.0967361,
                  reg_sw         =>   -0.3856154,
                  reg_ee         =>    -0.146727,
                  reg_em         =>    0.0588896,
                  reg_wmc        =>   -0.8713597,
                  reg_wmo        =>   -0.5977019,
                  reg_gm         =>    0.5495504,
                  reg_me         =>   -0.4763913,
                  reg_nwo        =>   -0.2671517,
                  reg_sy         =>   -0.2127042,
                  reg_wy         =>    -0.632866,
                  reg_yho        =>   -0.1671091,
                  reg_tw         =>   -0.7454805,
                  reg_no         =>   -0.0184146,
                  reg_S          =>   -0.0656671,
                  reg_NI         =>    0.1287643,
                  const          =>    -57.58979,
                  others         =>    0.0 );
            end if;
         when social_worker =>
            --
            --  > probit hlsvd $agedums $adlall $healthvars $regdums if age>=65 & sex==1 & hlsvd>=0;
            --
            if( ad.sex = male and ad.age >= 65 )then
               coeffs := (
                  age            =>     1.094193,
                  age_2          =>    -1.499418,
                  age_3          =>     0.687961,
                  adla_diff      =>    0.0589718,
                  adla_vdiff     =>    0.2256625,
                  adla_help      =>    0.3726156,
                  adla_not       =>    0.4392281,
                  adlb_diff      =>    0.0097574,
                  adlb_vdiff     =>    0.2612892,
                  adlb_help      =>    0.1822578,
                  adlb_not       =>   -0.1339545,
                  adlc_diff      =>   -0.0834826,
                  adlc_vdiff     =>    0.0009039,
                  adlc_help      =>   -0.1331837,
                  adlc_not       =>     0.216175,
                  adld_diff      =>    0.3711354,
                  adld_vdiff     =>    0.1318836,
                  adld_help      =>    0.3914928,
                  adld_not       =>    0.4613056,
                  adle_diff      =>    0.2030768,
                  adle_vdiff     =>      0.02079,
                  adle_help      =>    0.4661657,
                  adle_not       =>    0.0029114,
                  hl_exc         =>    -0.260729,
                  hl_good        =>   -0.1491564,
                  hl_poor        =>     0.276516,
                  hl_vpoor       =>    0.3878182,
                  reg_il         =>   -0.1368961,
                  reg_ol         =>   -0.0254861,
                  reg_se         =>   -0.3188386,
                  reg_sw         =>   -0.1479113,
                  reg_ee         =>    0.0287741,
                  reg_em         =>   -0.0640381,
                  reg_wmc        =>   -0.1253051,
                  reg_wmo        =>   -0.0323806,
                  reg_gm         =>   -0.1878271,
                  reg_nwo        =>   -0.1420359,
                  reg_sy         =>   -0.0856399,
                  reg_wy         =>   -0.0635017,
                  reg_yho        =>   -0.7948547,
                  reg_tw         =>    0.1421748,
                  reg_no         =>   -0.2493498,
                  reg_S          =>   -0.1757136,
                  reg_NI         =>   -0.0186197,
                  const          =>    -28.95037,
                  others         =>    0.0 );
            end if;
            --
            --  . probit hlsvd $agedums $adlall $healthvars $regdums if age>=65 & sex==2 & hlsvd>=0;
            --
            if( ad.sex = female and ad.age >= 65 )then
               coeffs := (
                  age            =>   -0.0452175,
                  age_2          =>    0.0918012,
                  age_3          =>   -0.0419244,
                  adla_diff      =>    0.1510932,
                  adla_vdiff     =>    0.2028086,
                  adla_help      =>    0.2738317,
                  adla_not       =>     0.318333,
                  adlb_diff      =>    0.0656887,
                  adlb_vdiff     =>    0.0148103,
                  adlb_help      =>    0.0468288,
                  adlb_not       =>   -0.0559428,
                  adlc_diff      =>   -0.0604262,
                  adlc_vdiff     =>    0.1573873,
                  adlc_help      =>     0.013211,
                  adlc_not       =>   -0.3953622,
                  adld_diff      =>    0.1613881,
                  adld_vdiff     =>   -0.0933245,
                  adld_help      =>     0.229408,
                  adld_not       =>    0.2219393,
                  adle_diff      =>    0.1047038,
                  adle_vdiff     =>    0.2845552,
                  adle_help      =>    0.4846194,
                  adle_not       =>    0.5598053,
                  hl_exc         =>   -0.4835048,
                  hl_good        =>   -0.2525199,
                  hl_poor        =>    0.3081938,
                  hl_vpoor       =>    0.5374876,
                  reg_il         =>   -0.5328757,
                  reg_ol         =>   -0.3002631,
                  reg_se         =>   -0.0038624,
                  reg_sw         =>   -0.1769698,
                  reg_ee         =>   -0.4971582,
                  reg_em         =>   -0.1192224,
                  reg_wmc        =>   -0.1213405,
                  reg_wmo        =>   -0.0191518,
                  reg_gm         =>    0.0994103,
                  reg_me         =>    -0.007872,
                  reg_nwo        =>   -0.0729229,
                  reg_sy         =>   -0.3361044,
                  reg_wy         =>   -0.8414103,
                  reg_yho        =>    0.0252969,
                  reg_tw         =>   -0.2035568,
                  reg_no         =>    0.0017562,
                  reg_S          =>     0.000171,
                  reg_NI         =>    0.0201308,
                  const          =>    -2.009155,
                  others         =>    0.0 );
            end if;
         when physiotherapist =>
            --
            --  > probit hlsvi $agedums $adlall $healthvars $regdums if age>=65 & sex==1 & hlsvi>=0;
            --
            if( ad.sex = male and ad.age >= 65 )then
               coeffs := (
                  age            =>   -0.2251553,
                  age_2          =>    0.2394517,
                  age_3          =>   -0.0849875,
                  adla_diff      =>    0.1469212,
                  adla_vdiff     =>    0.0129587,
                  adla_help      =>     0.141611,
                  adla_not       =>    0.2016108,
                  adlb_diff      =>   -0.0047073,
                  adlb_vdiff     =>    0.0954124,
                  adlb_help      =>   -0.0125655,
                  adlb_not       =>    0.3466201,
                  adlc_diff      =>   -0.0164076,
                  adlc_vdiff     =>    0.4181599,
                  adlc_help      =>   -0.0137818,
                  adlc_not       =>   -0.0576194,
                  adld_diff      =>    0.0227776,
                  adld_vdiff     =>    0.0569072,
                  adld_help      =>   -0.0372654,
                  adld_not       =>    0.2593029,
                  adle_diff      =>    0.0504488,
                  adle_vdiff     =>   -0.0672776,
                  adle_help      =>    0.3150429,
                  adle_not       =>   -0.3023555,
                  hl_exc         =>   -0.2213363,
                  hl_good        =>   -0.0682639,
                  hl_poor        =>    0.3171936,
                  hl_vpoor       =>    0.1867436,
                  reg_il         =>   -0.0762102,
                  reg_ol         =>   -0.1317225,
                  reg_se         =>    0.2059631,
                  reg_sw         =>   -0.1767631,
                  reg_ee         =>    -0.056991,
                  reg_em         =>    0.1712811,
                  reg_wmc        =>    0.0879205,
                  reg_wmo        =>    0.1338586,
                  reg_gm         =>   -0.1562466,
                  reg_me         =>   -0.0315163,
                  reg_nwo        =>    0.0455528,
                  reg_sy         =>    0.2228839,
                  reg_wy         =>   -0.4381442,
                  reg_yho        =>    0.0817224,
                  reg_tw         =>   -0.0116949,
                  reg_no         =>     0.242413,
                  reg_S          =>     0.158197,
                  reg_NI         =>   -0.0026918,
                  const          =>     5.275398,
                  others         =>    0.0 );
            end if;
            --
            --  . probit hlsvi $agedums $adlall $healthvars $regdums if age>=65 & sex==2 & hlsvi>=0;
            --
            if( ad.sex = female and ad.age >= 65 )then
               coeffs := (
                  age            =>    0.1849489,
                  age_2          =>   -0.2032915,
                  age_3          =>    0.0627467,
                  adla_diff      =>    0.1780286,
                  adla_vdiff     =>    0.1800243,
                  adla_help      =>    0.0200144,
                  adla_not       =>    0.0704469,
                  adlb_diff      =>   -0.1435354,
                  adlb_vdiff     =>    0.0107983,
                  adlb_help      =>   -0.0088941,
                  adlb_not       =>   -0.3090835,
                  adlc_diff      =>     0.109638,
                  adlc_vdiff     =>    0.0919868,
                  adlc_help      =>    0.2888924,
                  adlc_not       =>     0.205826,
                  adld_diff      =>    0.1260811,
                  adld_vdiff     =>    0.1543907,
                  adld_help      =>     0.155393,
                  adld_not       =>    0.1443343,
                  adle_diff      =>    0.1031255,
                  adle_vdiff     =>     0.059394,
                  adle_help      =>    0.2251383,
                  adle_not       =>    0.2545003,
                  hl_exc         =>   -0.3509381,
                  hl_good        =>    -0.132557,
                  hl_poor        =>    0.0167723,
                  hl_vpoor       =>    0.2931217,
                  reg_il         =>   -0.1899517,
                  reg_ol         =>   -0.0071153,
                  reg_se         =>    0.0639046,
                  reg_sw         =>    0.1254275,
                  reg_ee         =>   -0.0087119,
                  reg_em         =>   -0.1065968,
                  reg_wmc        =>     0.032879,
                  reg_wmo        =>    0.0787545,
                  reg_gm         =>   -0.2031376,
                  reg_me         =>   -0.2001052,
                  reg_nwo        =>   -0.1517789,
                  reg_sy         =>   -0.1471054,
                  reg_wy         =>   -0.1718272,
                  reg_yho        =>    0.0037786,
                  reg_tw         =>    0.1078934,
                  reg_no         =>   -0.0036925,
                  reg_S          =>   -0.0178459,
                  reg_NI         =>   -0.1400186,
                  const          =>    -6.461316,
                  others         =>    0.0 );
            end if;
      end case;
      --
      p := Vector_Product( regressors, coeffs );
      return p; 
   end Recieving_Care_Probit;

   --
   function Private_Care_Demand_Probit( ad : Person ) return Amount is
      coeffs     : Coeffs_Array := ( others => 0.0 );
      regressors    : Regressors_Array := ad.regressors;
      p             : Amount;
   begin
      if( ad.age < 65 )then
         return -999999.99;
      end if;
      coeffs := (
         age            =>   -0.7498903,
         age_2          =>      1.09343,
         age_3          =>   -0.4895075,
         female         =>    0.5106418,
         trend          =>   -0.0154981,
         hl_exc         =>   -0.5870405,
         hl_good        =>    -0.313609,
         hl_poor        =>    0.2807172,
         hl_vpoor       =>    0.2587717,
         hl_m           =>   -0.0912031,
         adl_b          =>    0.3638741,
         adl_c          =>    0.3529165,
         adl_e          =>    0.0873187,
         loginc         =>    0.0662865,
         neginc         =>    0.3752415,
         lognw          =>    0.0536625,
         negnw          =>    0.2298066,
         loghw          =>     0.048604,
         neghw          =>      0.46352,
         const          =>     11.83264,
         others         =>    0.0 );
      p := Vector_Product( regressors, coeffs );
      return p; 
   end Private_Care_Demand_Probit;
   
   
end Model.WSC.Household.Regressions;
