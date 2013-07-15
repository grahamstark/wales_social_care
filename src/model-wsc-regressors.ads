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

with T_Utils;
with Base_Model_Types;
--
-- Just an enumerated type holding the variables in Howard's BHPS regressions, and some code to declare
-- arrays of same using the T_Utils generic.
--
package Model.WSC.Regressors is
   
   use Base_Model_Types;
   
   type Regressors_Type is (
      -- tenures
      male,
      female,
      age,
      spinhh,
      ten_m,
      owned_o,
      owned_m,
      la_rent,
      ha_rent,
      oth_rent,
      owned,
      socrent,
      dead,
      adl_a,
      adl_b,
      adl_c,
      adl_d,
      adl_e,
      adlscore,
      adlsc_a,
      adla_none,
      adla_diff,
      adla_vdiff,
      adla_help,
      adla_not,
      adla_d2,
      adl_a_w,
      adl_a_b,
      
      adlsc_b,
      adlb_none,
      adlb_diff,
      adlb_vdiff,
      adlb_help,
      adlb_not,
      adlb_d2,
      adl_b_w,
      adl_b_b,
      
      adlsc_c,
      adlc_none,
      adlc_diff,
      adlc_vdiff,
      adlc_help,
      adlc_not,
      adlc_d2,
      adl_c_w,
      adl_c_b,
      
      adlsc_d,
      adld_none,
      adld_diff,
      adld_vdiff,
      adld_help,
      adld_not,
      adld_d2,
      adl_d_w,
      adl_d_b,
      
      adlsc_e,
      adle_none,
      adle_diff,
      adle_vdiff,
      adle_help,
      adle_not,
      adle_d2,
      adl_e_w,
      adl_e_b,
      
      england,
      scotland,
      wales,
      nireland,
      
      married,
      couple,
      carepers,
      
      care1,
      care2,
      care3,
      care4,
      care5,
      care6,
      care7,
      care8,
      care9,
      care10,
      care11,
      care12,
      care13,
      care14,
      carepno1,
      carepno2,
      carepno3,
      carepno4,
      carepno5,
      carepno6,
      carepno7,
      carepno8,
      carepno9,
      carepno10,
      carepno11,
      carepno12,
      carepno13,
      carepno14,
      rec_care,
      
      dead1,
      dead2,
      dead3,
      dead4,
      dead5,
      dead6,
      dead7,
      dead8,
      d1,
      d2,
      d3,
      d4,
      d5,
      d6,
      d7,
      d8,
      pardead,
      seperate,
      combine,
      dlamob,
      dlacare,
      dladk,
      dlaany,
      aa,
      rp,
      trend, 
      trend_2,
      trend_3,
      disabled,
      disab_m,
      age_2,
      age_3,
      age_4,
      age_5,
      age4044, 
      age4549, 
      age5054, 
      age5559, 
      age6064,     
      age6569,
      age7074, 
      age7579, 
      age8084, 
      age8589, 
      age90,
      
      age65o,
      age64, 
      age66,
      age67,
      age68,
      age69,
      age70,
      age59, 
      
      reg_il,--
      reg_ol,--
      reg_se,--
      reg_sw,--
      reg_ee,--
      reg_em,--
      reg_wmc,---
      reg_wmo,--
      reg_gm,--
      reg_me,--
      reg_nwo,--
      reg_sy,--
      reg_wy,--
      reg_yho,--
      reg_tw,--
      reg_no,--
      reg_w,--
      reg_s,--
      reg_ni, --
      
      hhkids, --
      hl_worse, -- 
      hl_better, --
      rent2own, --
      own2rent, --
      renter, --
      owner, --
      working, --
      retired, --
      retire,  --
      reenter, --
      
      hq_deg, --
      hq_alev, --
      hq_gcse, --
      hq_oth, --
      
      q_oth, -- any qual above gcse
      
      hl_exc, 
      hl_good, 
      hl_fair, 
      hl_poor, 
      hl_vpoor,
      hl_m,    
      healthsc,
      lagged_depvar,
      lead_depvar,
      
      ispc,
      disben,
      privpen,
    
      loginc, -- log real income
      neginc, -- real income is zero or negative
      lognw, -- log real (non-housing) net wealth
      negnw, -- negative net wealth
      loghw, -- log real (housing) net wealth
      neghw, -- real non-housing net wealth is zero or negative
     
      const ); 

  package Regressors_Package is new T_Utils( 
   T=>Regressors_Type, 
   Amount_Type => Amount, 
   Rate_Type=>Rate, 
   Counter_Type=>Counter_Type );
  
  subtype Regressors_Array is Regressors_Package.Amount_Array;
  subtype Coeffs_Array is Regressors_Package.Rate_Array;
  
end Model.WSC.Regressors;
