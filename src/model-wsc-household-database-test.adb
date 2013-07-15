with Ada.Assertions;

package body Model.WSC.Household.Database is

   use Ada.Assertions;
   
   function Compare_Id_And_Wave_LT( l, r : Id_And_Wave ) return Boolean is
   begin
      if( l.wave < r.wave )then
         return True;
      elsif( l.wave > r.wave )then
         return False;
      end if;
      return l.id < r.id;
   end Compare_Id_And_Wave_LT;
   
   function Compare_Id_And_Wave_LE( l, r : Id_And_Wave ) return Boolean is
   begin
      if( l.wave < r.wave )then
         return True;
      elsif( l.wave > r.wave )then
         return False;
      end if;
      return l.id <= r.id;
   end Compare_Id_And_Wave_LE;
   
   function Compare_Id_And_Wave_EQ( l, r : Id_And_Wave ) return Boolean is
   begin
      return l.wave = r.wave and l.id = r.id;
   end Compare_Id_And_Wave_EQ;
   
   function Get_Key( pers  : Person ) return Id_And_Wave is
      iw : Id_And_Wave;
   begin
      iw.wave := pers.wave;
      iw.id := pers.pid;
      return iw;      
   end Get_Key;
   
   function Get_Key( skel  : Household_Skeleton ) return Id_And_Wave is
      iw : Id_And_Wave;
   begin
      iw.wave := skel.wave;
      iw.id := skel.hid;
      return iw;      
   end Get_Key;
      
   function Get_Key( hdata : Household_Data ) return Id_And_Wave is
      iw : Id_And_Wave;
   begin
      iw.wave := hdata.wave;
      iw.id := hdata.hid;
      return iw;      
   end Get_Key;

   
   procedure Open( 
      db         : out DB_Type; 
      filename   : String; 
      start_wave : Waves := Waves'First;
      end_wave   : Waves := Waves'Last ) is
   begin
      Pers_Seq_IO.Open( db.pers_f ,filename & "/pers.db" );
      Skel_Seq_IO.Open( db.skel_f ,filename & "/skel.db" );
      HData_Seq_IO.Open( db.hdata_f, filename & "/hdata.db" );
   end Open;
      
   procedure Close( 
      db : in out DB_Type ) is
   begin
      Pers_Seq_IO.Close( db.pers_f );
      Skel_Seq_IO.Close( db.skel_f  );
      HData_Seq_IO.Close( db.hdata_f );
   end Close;
   
   procedure Create( 
      db         : out DB_Type; 
      filename   : String ) is
   begin
      Pers_Seq_IO.Create( db.pers_f ,filename & "/pers.db" );
      Skel_Seq_IO.Create( db.skel_f ,filename & "/skel.db" );
      HData_Seq_IO.Create( db.hdata_f, filename & "/hdata.db" );
   end Create;
   
   
   function Get_Household( db : in DB_Type; wave : Waves; hid : Sernum_Value ) return Household is
      hh : Household;
      idw : Id_And_Wave := ( wave=>wave, id => hid );
      skel : Household_Skeleton;
      pers : Person;
   begin
      HData_Seq_IO.Read( db.hdata_f, idw, hh.hdata );
      Skel_Seq_IO.Read( db.skel_f, idw, skel );
      hh.hid := hh.hdata.hid;
      hh.wave := hh.hdata.wave;
      hh.num_benefit_units := skel.num_benefit_units;
      
      for pno in 1 .. skel.num_people loop
         idw.id := skel.people( pno ).pid;
         Pers_Seq_IO.Read( db.pers_f, idw, pers );
         declare
            buno : Benefit_Unit_Number := skel.people( pno ).buno;
            adno : Person_Number := skel.people( pno ).adno;
            chno : Person_Number := skel.people( pno ).chno;
         begin
            if( adno > 0 )then
               hh.benefit_units( buno ).adults( adno ) := pers;
            elsif( chno > 0 )then
               hh.benefit_units( buno ).children( chno ) := pers;
            else
               Assert( False, " out of sequence error in hhld " & hid'Img & " pno " & pno'Img );
            end if;
         end;
      end loop;
      return hh;
   end Get_Household;
   
   function Get_Person( db : in DB_Type ; wave : Waves; pid : Sernum_Value ) return Person is
      idw : Id_And_Wave := ( wave=>wave, id => pid );
      pers : Person;
   begin
      Pers_Seq_IO.Read( db.pers_f, idw, pers );
      return pers;
   end Get_Person;
   
   function Next_In_Wave( db : in DB_Type; hid : Sernum_Value; wave : Waves ) return Sernum_Value is
      new_curr : Sernum_Value;
   begin
      return new_curr;
   end Next_In_Wave;
   
   procedure Write_Household( db : in out DB_Type; hh : Household ) is
      skel : Household_Skeleton;
      pno  : Person_Count := Person_Count'First;
      pptr : Person_Pointer;
   begin
      HData_Seq_IO.Write( db.hdata_f, hh.hdata );
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 .. hh.benefit_units( buno ).num_adults loop
            pno := pno + 1;
            pptr.pid := hh.benefit_units( buno ).adults( adno ).pid;
            pptr.pno := hh.benefit_units( buno ).adults( adno ).pno;
            pptr.buno := buno;
            pptr.adno := adno;
            pptr.chno := 0;
            pptr.hdsp := hh.benefit_units( buno ).adults( adno ).hdsp;
            skel.people( pno ) := pptr;            
            Pers_Seq_IO.Write( db.pers_f, hh.benefit_units( buno ).adults( adno ));
         end loop;
         for chno in 1 .. hh.benefit_units( buno ).num_children loop
            pno := pno + 1;
            pptr.pid := hh.benefit_units( buno ).children( chno ).pid;
            pptr.pno := hh.benefit_units( buno ).children( chno ).pno;
            pptr.buno := buno;
            pptr.chno := chno;
            pptr.adno := 0;
            pptr.hdsp := neither;
            skel.people( pno ) := pptr;            
            Pers_Seq_IO.Write( db.pers_f, hh.benefit_units( buno ).children( chno ));
         end loop;
      end loop;
      skel.wave := hh.wave;
      skel.hid := hh.hid;
      skel.num_people := pno;
      skel.num_benefit_units := hh.num_benefit_units;
      Skel_Seq_IO.Write( db.skel_f, skel );
   end Write_Household;
      
   procedure Delete_Household( db : in out DB_Type; wave : Waves; hid : Sernum_Value  )is
      idw : Id_And_Wave;
      skel : Household_Skeleton;
   begin
      idw.id := hid;
      idw.wave := wave;
      Skel_Seq_IO.Read( db.skel_f, idw, skel );
      Skel_Seq_IO.Remove( db.skel_f, idw );
      HData_Seq_IO.Remove( db.hdata_f, idw );
      for pno in 1 .. skel.num_people loop
         idw.id := skel.people( pno ).pid;
         Pers_Seq_IO.Remove( db.pers_f, idw );
      end loop;
   end Delete_Household;
   
   procedure Get_Household_Sernums( 
      db : in out DB_Type; 
      wave : Waves; 
      num_hhs : out Households_Per_Wave_Count; 
      sernums : out Sernum_Array ) is
      skel : Household_Skeleton;
   begin
      num_hhs := 1;
      Skel_Seq_IO.Reset( db.skel_f );
      loop
         Skel_Seq_IO.Read( db.skel_f, skel );
         exit when Skel_Seq_IO.End_Of_File( db.skel_f ) or skel.wave > wave;
         sernums( num_hhs ) := skel.hid;
         num_hhs := num_hhs + 1;
      end loop;
      Skel_Seq_IO.Reset( db.skel_f );
   end Get_Household_Sernums;
      
end Model.WSC.Household.Database;
