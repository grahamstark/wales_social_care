package body Model.WSC.Parameters.Historic is
   
   
   type Historic_Parameters_Array is array( Historic_Years ) of Parameters_Rec;
   
   params : Historic_Parameters_Array;

   function Get_Parameters( wave : Waves ) return Parameters_Rec is
      year : Historic_Years := Year_From_Wave( wave ) - 1;
   begin
      return params( year );
   end Get_Parameters;
   
   function Get_Parameters( year : Historic_Years ) return Parameters_Rec is
   begin
      return params( year );
   end Get_Parameters;
   
begin   
      -- 
      -- 
      -- WAVE  1992
      -- 
      params( 1992 ).benefits.attendance_allowance.low_age := 65;
      params( 1992 ).benefits.attendance_allowance.high_age := 150;
      params( 1992 ).benefits.attendance_allowance.benefit_rate( high ) :=   43.35 ;
      params( 1992 ).benefits.attendance_allowance.benefit_rate( low ) :=  28.95 ;
      params( 1992 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 1992 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 1992 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 1992 ).benefits.dla.mobility.low_age := 0;
      params( 1992 ).benefits.dla.mobility.high_age := 64;
      params( 1992 ).benefits.dla.mobility.benefit_rate( high ) := 30.30 ;
      params( 1992 ).benefits.dla.mobility.benefit_rate( low ) := 11.55 ;
      params( 1992 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 1992 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 1992 ).benefits.dla.care.low_age := 3;
      params( 1992 ).benefits.dla.care.high_age := 64;
      params( 1992 ).benefits.dla.care.benefit_rate( high ) := 43.35 ;
      params( 1992 ).benefits.dla.care.benefit_rate( middle ) := 28.95 ;
      params( 1992 ).benefits.dla.care.benefit_rate( low ) := 11.55 ;
      params( 1992 ).benefits.dla.care.test_generosity := 1.0;
      params( 1992 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  1993
      -- 
      params( 1993 ).benefits.attendance_allowance.low_age := 65;
      params( 1993 ).benefits.attendance_allowance.high_age := 150;
      params( 1993 ).benefits.attendance_allowance.benefit_rate( high ) :=   44.90 ;
      params( 1993 ).benefits.attendance_allowance.benefit_rate( low ) :=  30.00 ;
      params( 1993 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 1993 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 1993 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 1993 ).benefits.dla.mobility.low_age := 0;
      params( 1993 ).benefits.dla.mobility.high_age := 64;
      params( 1993 ).benefits.dla.mobility.benefit_rate( high ) := 31.40 ;
      params( 1993 ).benefits.dla.mobility.benefit_rate( low ) := 11.95 ;
      params( 1993 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 1993 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 1993 ).benefits.dla.care.low_age := 3;
      params( 1993 ).benefits.dla.care.high_age := 64;
      params( 1993 ).benefits.dla.care.benefit_rate( high ) := 44.90 ;
      params( 1993 ).benefits.dla.care.benefit_rate( middle ) := 30.00 ;
      params( 1993 ).benefits.dla.care.benefit_rate( low ) := 11.95 ;
      params( 1993 ).benefits.dla.care.test_generosity := 1.0;
      params( 1993 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  1994
      -- 
      params( 1994 ).benefits.attendance_allowance.low_age := 65;
      params( 1994 ).benefits.attendance_allowance.high_age := 150;
      params( 1994 ).benefits.attendance_allowance.benefit_rate( high ) :=   45.70 ;
      params( 1994 ).benefits.attendance_allowance.benefit_rate( low ) :=  30.55 ;
      params( 1994 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 1994 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 1994 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 1994 ).benefits.dla.mobility.low_age := 0;
      params( 1994 ).benefits.dla.mobility.high_age := 64;
      params( 1994 ).benefits.dla.mobility.benefit_rate( high ) := 31.95 ;
      params( 1994 ).benefits.dla.mobility.benefit_rate( low ) := 12.15 ;
      params( 1994 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 1994 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 1994 ).benefits.dla.care.low_age := 3;
      params( 1994 ).benefits.dla.care.high_age := 64;
      params( 1994 ).benefits.dla.care.benefit_rate( high ) := 45.70 ;
      params( 1994 ).benefits.dla.care.benefit_rate( middle ) := 30.55 ;
      params( 1994 ).benefits.dla.care.benefit_rate( low ) := 12.15 ;
      params( 1994 ).benefits.dla.care.test_generosity := 1.0;
      params( 1994 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  1995
      -- 
      params( 1995 ).benefits.attendance_allowance.low_age := 65;
      params( 1995 ).benefits.attendance_allowance.high_age := 150;
      params( 1995 ).benefits.attendance_allowance.benefit_rate( high ) :=   46.70 ;
      params( 1995 ).benefits.attendance_allowance.benefit_rate( low ) :=  31.20 ;
      params( 1995 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 1995 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 1995 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 1995 ).benefits.dla.mobility.low_age := 0;
      params( 1995 ).benefits.dla.mobility.high_age := 64;
      params( 1995 ).benefits.dla.mobility.benefit_rate( high ) := 32.65 ;
      params( 1995 ).benefits.dla.mobility.benefit_rate( low ) := 12.40 ;
      params( 1995 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 1995 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 1995 ).benefits.dla.care.low_age := 3;
      params( 1995 ).benefits.dla.care.high_age := 64;
      params( 1995 ).benefits.dla.care.benefit_rate( high ) := 46.70 ;
      params( 1995 ).benefits.dla.care.benefit_rate( middle ) := 31.20 ;
      params( 1995 ).benefits.dla.care.benefit_rate( low ) := 12.40 ;
      params( 1995 ).benefits.dla.care.test_generosity := 1.0;
      params( 1995 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  1996
      -- 
      params( 1996 ).benefits.attendance_allowance.low_age := 65;
      params( 1996 ).benefits.attendance_allowance.high_age := 150;
      params( 1996 ).benefits.attendance_allowance.benefit_rate( high ) :=   48.50 ;
      params( 1996 ).benefits.attendance_allowance.benefit_rate( low ) :=  32.40 ;
      params( 1996 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 1996 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 1996 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 1996 ).benefits.dla.mobility.low_age := 0;
      params( 1996 ).benefits.dla.mobility.high_age := 64;
      params( 1996 ).benefits.dla.mobility.benefit_rate( high ) := 33.90 ;
      params( 1996 ).benefits.dla.mobility.benefit_rate( low ) := 12.90 ;
      params( 1996 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 1996 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 1996 ).benefits.dla.care.low_age := 3;
      params( 1996 ).benefits.dla.care.high_age := 64;
      params( 1996 ).benefits.dla.care.benefit_rate( high ) := 48.50 ;
      params( 1996 ).benefits.dla.care.benefit_rate( middle ) := 32.40 ;
      params( 1996 ).benefits.dla.care.benefit_rate( low ) := 12.90 ;
      params( 1996 ).benefits.dla.care.test_generosity := 1.0;
      params( 1996 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  1997
      -- 
      params( 1997 ).benefits.attendance_allowance.low_age := 65;
      params( 1997 ).benefits.attendance_allowance.high_age := 150;
      params( 1997 ).benefits.attendance_allowance.benefit_rate( high ) :=   49.50 ;
      params( 1997 ).benefits.attendance_allowance.benefit_rate( low ) :=  33.10 ;
      params( 1997 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 1997 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 1997 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 1997 ).benefits.dla.mobility.low_age := 0;
      params( 1997 ).benefits.dla.mobility.high_age := 64;
      params( 1997 ).benefits.dla.mobility.benefit_rate( high ) := 34.60 ;
      params( 1997 ).benefits.dla.mobility.benefit_rate( low ) := 13.15 ;
      params( 1997 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 1997 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 1997 ).benefits.dla.care.low_age := 3;
      params( 1997 ).benefits.dla.care.high_age := 64;
      params( 1997 ).benefits.dla.care.benefit_rate( high ) := 49.50 ;
      params( 1997 ).benefits.dla.care.benefit_rate( middle ) := 33.10 ;
      params( 1997 ).benefits.dla.care.benefit_rate( low ) := 13.50 ;
      params( 1997 ).benefits.dla.care.test_generosity := 1.0;
      params( 1997 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  1998
      -- 
      params( 1998 ).benefits.attendance_allowance.low_age := 65;
      params( 1998 ).benefits.attendance_allowance.high_age := 150;
      params( 1998 ).benefits.attendance_allowance.benefit_rate( high ) :=   51.30 ;
      params( 1998 ).benefits.attendance_allowance.benefit_rate( low ) :=  34.30 ;
      params( 1998 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 1998 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 1998 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 1998 ).benefits.dla.mobility.low_age := 0;
      params( 1998 ).benefits.dla.mobility.high_age := 64;
      params( 1998 ).benefits.dla.mobility.benefit_rate( high ) := 35.85 ;
      params( 1998 ).benefits.dla.mobility.benefit_rate( low ) := 13.60 ;
      params( 1998 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 1998 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 1998 ).benefits.dla.care.low_age := 3;
      params( 1998 ).benefits.dla.care.high_age := 64;
      params( 1998 ).benefits.dla.care.benefit_rate( high ) := 51.30 ;
      params( 1998 ).benefits.dla.care.benefit_rate( middle ) := 34.30 ;
      params( 1998 ).benefits.dla.care.benefit_rate( low ) := 13.60 ;
      params( 1998 ).benefits.dla.care.test_generosity := 1.0;
      params( 1998 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  1999
      -- 
      params( 1999 ).benefits.attendance_allowance.low_age := 65;
      params( 1999 ).benefits.attendance_allowance.high_age := 150;
      params( 1999 ).benefits.attendance_allowance.benefit_rate( high ) :=   52.95 ;
      params( 1999 ).benefits.attendance_allowance.benefit_rate( low ) :=  35.40 ;
      params( 1999 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 1999 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 1999 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 1999 ).benefits.dla.mobility.low_age := 0;
      params( 1999 ).benefits.dla.mobility.high_age := 64;
      params( 1999 ).benefits.dla.mobility.benefit_rate( high ) := 37.00 ;
      params( 1999 ).benefits.dla.mobility.benefit_rate( low ) := 14.05 ;
      params( 1999 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 1999 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 1999 ).benefits.dla.care.low_age := 3;
      params( 1999 ).benefits.dla.care.high_age := 64;
      params( 1999 ).benefits.dla.care.benefit_rate( high ) := 52.95 ;
      params( 1999 ).benefits.dla.care.benefit_rate( middle ) := 35.40 ;
      params( 1999 ).benefits.dla.care.benefit_rate( low ) := 14.05 ;
      params( 1999 ).benefits.dla.care.test_generosity := 1.0;
      params( 1999 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  2000
      -- 
      params( 2000 ).benefits.attendance_allowance.low_age := 65;
      params( 2000 ).benefits.attendance_allowance.high_age := 150;
      params( 2000 ).benefits.attendance_allowance.benefit_rate( high ) :=   53.55 ;
      params( 2000 ).benefits.attendance_allowance.benefit_rate( low ) :=  35.80 ;
      params( 2000 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 2000 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 2000 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 2000 ).benefits.dla.mobility.low_age := 0;
      params( 2000 ).benefits.dla.mobility.high_age := 64;
      params( 2000 ).benefits.dla.mobility.benefit_rate( high ) := 37.40 ;
      params( 2000 ).benefits.dla.mobility.benefit_rate( low ) := 14.20 ;
      params( 2000 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 2000 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 2000 ).benefits.dla.care.low_age := 3;
      params( 2000 ).benefits.dla.care.high_age := 64;
      params( 2000 ).benefits.dla.care.benefit_rate( high ) := 53.55 ;
      params( 2000 ).benefits.dla.care.benefit_rate( middle ) := 35.80 ;
      params( 2000 ).benefits.dla.care.benefit_rate( low ) := 14.20 ;
      params( 2000 ).benefits.dla.care.test_generosity := 1.0;
      params( 2000 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  2001
      -- 
      params( 2001 ).benefits.attendance_allowance.low_age := 65;
      params( 2001 ).benefits.attendance_allowance.high_age := 150;
      params( 2001 ).benefits.attendance_allowance.benefit_rate( high ) :=   55.30 ;
      params( 2001 ).benefits.attendance_allowance.benefit_rate( low ) :=  37.00 ;
      params( 2001 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 2001 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 2001 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 2001 ).benefits.dla.mobility.low_age := 0;
      params( 2001 ).benefits.dla.mobility.high_age := 64;
      params( 2001 ).benefits.dla.mobility.benefit_rate( high ) := 38.65 ;
      params( 2001 ).benefits.dla.mobility.benefit_rate( low ) := 14.65 ;
      params( 2001 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 2001 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 2001 ).benefits.dla.care.low_age := 3;
      params( 2001 ).benefits.dla.care.high_age := 64;
      params( 2001 ).benefits.dla.care.benefit_rate( high ) := 55.30 ;
      params( 2001 ).benefits.dla.care.benefit_rate( middle ) := 37.00 ;
      params( 2001 ).benefits.dla.care.benefit_rate( low ) := 14.65 ;
      params( 2001 ).benefits.dla.care.test_generosity := 1.0;
      params( 2001 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  2002
      -- 
      params( 2002 ).benefits.attendance_allowance.low_age := 65;
      params( 2002 ).benefits.attendance_allowance.high_age := 150;
      params( 2002 ).benefits.attendance_allowance.benefit_rate( high ) :=   56.25 ;
      params( 2002 ).benefits.attendance_allowance.benefit_rate( low ) :=  37.65 ;
      params( 2002 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 2002 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 2002 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 2002 ).benefits.dla.mobility.low_age := 0;
      params( 2002 ).benefits.dla.mobility.high_age := 64;
      params( 2002 ).benefits.dla.mobility.benefit_rate( high ) := 39.30 ;
      params( 2002 ).benefits.dla.mobility.benefit_rate( low ) := 14.90 ;
      params( 2002 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 2002 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 2002 ).benefits.dla.care.low_age := 3;
      params( 2002 ).benefits.dla.care.high_age := 64;
      params( 2002 ).benefits.dla.care.benefit_rate( high ) := 56.25 ;
      params( 2002 ).benefits.dla.care.benefit_rate( middle ) := 37.65 ;
      params( 2002 ).benefits.dla.care.benefit_rate( low ) := 14.90 ;
      params( 2002 ).benefits.dla.care.test_generosity := 1.0;
      params( 2002 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  2003
      -- 
      params( 2003 ).benefits.attendance_allowance.low_age := 65;
      params( 2003 ).benefits.attendance_allowance.high_age := 150;
      params( 2003 ).benefits.attendance_allowance.benefit_rate( high ) :=   57.20 ;
      params( 2003 ).benefits.attendance_allowance.benefit_rate( low ) :=  38.30 ;
      params( 2003 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 2003 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 2003 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 2003 ).benefits.dla.mobility.low_age := 0;
      params( 2003 ).benefits.dla.mobility.high_age := 64;
      params( 2003 ).benefits.dla.mobility.benefit_rate( high ) := 39.95 ;
      params( 2003 ).benefits.dla.mobility.benefit_rate( low ) := 15.15 ;
      params( 2003 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 2003 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 2003 ).benefits.dla.care.low_age := 3;
      params( 2003 ).benefits.dla.care.high_age := 64;
      params( 2003 ).benefits.dla.care.benefit_rate( high ) := 57.20 ;
      params( 2003 ).benefits.dla.care.benefit_rate( middle ) := 38.30 ;
      params( 2003 ).benefits.dla.care.benefit_rate( low ) := 15.15 ;
      params( 2003 ).benefits.dla.care.test_generosity := 1.0;
      params( 2003 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  2004
      -- 
      params( 2004 ).benefits.attendance_allowance.low_age := 65;
      params( 2004 ).benefits.attendance_allowance.high_age := 150;
      params( 2004 ).benefits.attendance_allowance.benefit_rate( high ) :=   58.80 ;
      params( 2004 ).benefits.attendance_allowance.benefit_rate( low ) :=  39.35 ;
      params( 2004 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 2004 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 2004 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 2004 ).benefits.dla.mobility.low_age := 0;
      params( 2004 ).benefits.dla.mobility.high_age := 64;
      params( 2004 ).benefits.dla.mobility.benefit_rate( high ) := 41.05 ;
      params( 2004 ).benefits.dla.mobility.benefit_rate( low ) := 15.55 ;
      params( 2004 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 2004 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 2004 ).benefits.dla.care.low_age := 3;
      params( 2004 ).benefits.dla.care.high_age := 64;
      params( 2004 ).benefits.dla.care.benefit_rate( high ) := 58.80 ;
      params( 2004 ).benefits.dla.care.benefit_rate( middle ) := 39.35 ;
      params( 2004 ).benefits.dla.care.benefit_rate( low ) := 15.55 ;
      params( 2004 ).benefits.dla.care.test_generosity := 1.0;
      params( 2004 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  2005
      -- 
      params( 2005 ).benefits.attendance_allowance.low_age := 65;
      params( 2005 ).benefits.attendance_allowance.high_age := 150;
      params( 2005 ).benefits.attendance_allowance.benefit_rate( high ) :=   60.60 ;
      params( 2005 ).benefits.attendance_allowance.benefit_rate( low ) :=  40.55 ;
      params( 2005 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 2005 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 2005 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 2005 ).benefits.dla.mobility.low_age := 0;
      params( 2005 ).benefits.dla.mobility.high_age := 64;
      params( 2005 ).benefits.dla.mobility.benefit_rate( high ) := 42.30 ;
      params( 2005 ).benefits.dla.mobility.benefit_rate( low ) := 16.05 ;
      params( 2005 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 2005 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 2005 ).benefits.dla.care.low_age := 3;
      params( 2005 ).benefits.dla.care.high_age := 64;
      params( 2005 ).benefits.dla.care.benefit_rate( high ) := 60.60 ;
      params( 2005 ).benefits.dla.care.benefit_rate( middle ) := 40.55 ;
      params( 2005 ).benefits.dla.care.benefit_rate( low ) := 16.05 ;
      params( 2005 ).benefits.dla.care.test_generosity := 1.0;
      params( 2005 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  2006
      -- 
      params( 2006 ).benefits.attendance_allowance.low_age := 65;
      params( 2006 ).benefits.attendance_allowance.high_age := 150;
      params( 2006 ).benefits.attendance_allowance.benefit_rate( high ) :=   62.25 ;
      params( 2006 ).benefits.attendance_allowance.benefit_rate( low ) :=  41.65 ;
      params( 2006 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 2006 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 2006 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 2006 ).benefits.dla.mobility.low_age := 0;
      params( 2006 ).benefits.dla.mobility.high_age := 64;
      params( 2006 ).benefits.dla.mobility.benefit_rate( high ) := 43.45 ;
      params( 2006 ).benefits.dla.mobility.benefit_rate( low ) := 16.50 ;
      params( 2006 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 2006 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 2006 ).benefits.dla.care.low_age := 3;
      params( 2006 ).benefits.dla.care.high_age := 64;
      params( 2006 ).benefits.dla.care.benefit_rate( high ) := 62.25 ;
      params( 2006 ).benefits.dla.care.benefit_rate( middle ) := 41.65 ;
      params( 2006 ).benefits.dla.care.benefit_rate( low ) := 16.50 ;
      params( 2006 ).benefits.dla.care.test_generosity := 1.0;
      params( 2006 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  2007
      -- 
      params( 2007 ).benefits.attendance_allowance.low_age := 65;
      params( 2007 ).benefits.attendance_allowance.high_age := 150;
      params( 2007 ).benefits.attendance_allowance.benefit_rate( high ) :=   64.50 ;
      params( 2007 ).benefits.attendance_allowance.benefit_rate( low ) :=  43.15 ;
      params( 2007 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 2007 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 2007 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 2007 ).benefits.dla.mobility.low_age := 0;
      params( 2007 ).benefits.dla.mobility.high_age := 64;
      params( 2007 ).benefits.dla.mobility.benefit_rate( high ) := 45.00 ;
      params( 2007 ).benefits.dla.mobility.benefit_rate( low ) := 17.10 ;
      params( 2007 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 2007 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 2007 ).benefits.dla.care.low_age := 3;
      params( 2007 ).benefits.dla.care.high_age := 64;
      params( 2007 ).benefits.dla.care.benefit_rate( high ) := 64.50 ;
      params( 2007 ).benefits.dla.care.benefit_rate( middle ) := 43.15 ;
      params( 2007 ).benefits.dla.care.benefit_rate( low ) := 17.10 ;
      params( 2007 ).benefits.dla.care.test_generosity := 1.0;
      params( 2007 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  2008
      -- 
      params( 2008 ).benefits.attendance_allowance.low_age := 65;
      params( 2008 ).benefits.attendance_allowance.high_age := 150;
      params( 2008 ).benefits.attendance_allowance.benefit_rate( high ) :=   67.00 ;
      params( 2008 ).benefits.attendance_allowance.benefit_rate( low ) :=  44.85 ;
      params( 2008 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 2008 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 2008 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 2008 ).benefits.dla.mobility.low_age := 0;
      params( 2008 ).benefits.dla.mobility.high_age := 64;
      params( 2008 ).benefits.dla.mobility.benefit_rate( high ) := 46.75 ;
      params( 2008 ).benefits.dla.mobility.benefit_rate( low ) := 17.75 ;
      params( 2008 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 2008 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 2008 ).benefits.dla.care.low_age := 3;
      params( 2008 ).benefits.dla.care.high_age := 64;
      params( 2008 ).benefits.dla.care.benefit_rate( high ) := 67.00 ;
      params( 2008 ).benefits.dla.care.benefit_rate( middle ) := 44.85 ;
      params( 2008 ).benefits.dla.care.benefit_rate( low ) := 17.75 ;
      params( 2008 ).benefits.dla.care.test_generosity := 1.0;
      params( 2008 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  2009
      -- 
      params( 2009 ).benefits.attendance_allowance.low_age := 65;
      params( 2009 ).benefits.attendance_allowance.high_age := 150;
      params( 2009 ).benefits.attendance_allowance.benefit_rate( high ) :=   70.35 ;
      params( 2009 ).benefits.attendance_allowance.benefit_rate( low ) :=  47.10 ;
      params( 2009 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 2009 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 2009 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 2009 ).benefits.dla.mobility.low_age := 0;
      params( 2009 ).benefits.dla.mobility.high_age := 64;
      params( 2009 ).benefits.dla.mobility.benefit_rate( high ) := 49.10;
      params( 2009 ).benefits.dla.mobility.benefit_rate( low ) := 18.65;
      params( 2009 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 2009 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 2009 ).benefits.dla.care.low_age := 3;
      params( 2009 ).benefits.dla.care.high_age := 64;
      params( 2009 ).benefits.dla.care.benefit_rate( high ) := 70.35;
      params( 2009 ).benefits.dla.care.benefit_rate( middle ) := 47.10;
      params( 2009 ).benefits.dla.care.benefit_rate( low ) := 18.65;
      params( 2009 ).benefits.dla.care.test_generosity := 1.0;
      params( 2009 ).benefits.dla.care.preserve_for_existing_claimants := False;
      -- 
      -- WAVE  2010
      -- 
      params( 2010 ).benefits.attendance_allowance.low_age := 65;
      params( 2010 ).benefits.attendance_allowance.high_age := 150;
      params( 2010 ).benefits.attendance_allowance.benefit_rate( high ) :=   71.40 ;
      params( 2010 ).benefits.attendance_allowance.benefit_rate( low ) :=  47.80 ;
      params( 2010 ).benefits.attendance_allowance.test_generosity := 1.0;
      params( 2010 ).benefits.attendance_allowance.preserve_for_existing_claimants := False;
      params( 2010 ).benefits.dla.dont_pay_for_residential_claimants := False;
      params( 2010 ).benefits.dla.mobility.low_age := 0;
      params( 2010 ).benefits.dla.mobility.high_age := 64;
      params( 2010 ).benefits.dla.mobility.benefit_rate( high ) := 49.85;
      params( 2010 ).benefits.dla.mobility.benefit_rate( low ) := 18.95;
      params( 2010 ).benefits.dla.mobility.test_generosity := 1.0;
      params( 2010 ).benefits.dla.mobility.preserve_for_existing_claimants := False;
      params( 2010 ).benefits.dla.care.low_age := 3;
      params( 2010 ).benefits.dla.care.high_age := 64;
      params( 2010 ).benefits.dla.care.benefit_rate( high ) := 71.40;
      params( 2010 ).benefits.dla.care.benefit_rate( middle ) := 47.80;
      params( 2010 ).benefits.dla.care.benefit_rate( low ) := 18.95;
      params( 2010 ).benefits.dla.care.test_generosity := 1.0;
      params( 2010 ).benefits.dla.care.preserve_for_existing_claimants := False;
      
end  Model.WSC.Parameters.Historic;
