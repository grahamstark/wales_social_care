with Ada.Assertions;
with Ada.Directories;
with Text_Utils;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with GNATColl.Traces;

package body Model.WSC.Household.Database is
   use Text_Utils;
   use Ada.Assertions;
   use Ada.Text_IO;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.HOUSEHOLD.DATABASE" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   WARN_ON_DUPS : constant Boolean := False;
   
   procedure Compact( db : in out DB_Type ) is
   begin
      null;
   end Compact;
   
   function Which_Data_Origin( wave : Waves ) return Data_Origin is
      which : Data_Origin;
   begin
      if( wave <= Waves_With_Data'Last )then
         which := actual;
      else
         which := estimated;
      end if;
      return which;
   end Which_Data_Origin;
   
   function Compare_Id_And_Wave_LT( l, r : Id_And_Wave ) return Boolean is
   begin
      if( l.wave < r.wave )then
         return True;
      elsif( l.wave > r.wave )then
         return False;
      end if;
      return l.id < r.id;
   end Compare_Id_And_Wave_LT;
   
   function Compare_Id_And_Wave_EQ( l, r : Positive ) return Boolean is
   begin
      return l = r;
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
   
   function Contains_Person( db : DB_Type; wave : Waves; id : Sernum_Value ) return Boolean is
      iw : Id_And_Wave := ( id => id, wave => wave );
   begin
      return db.pers_ptrs.Contains( iw );
   end Contains_Person;

   function Contains_HH( db : DB_Type; wave : Waves; id : Sernum_Value ) return Boolean is
      iw : Id_And_Wave := ( id => id, wave => wave );
   begin
      return db.hh_ptrs.Contains( iw );
   end Contains_HH;
   
   function To_Set( a : Sernum_List ) return Sernum_Set is
      s : Sernum_Set;
      p : Natural := Natural( a.Length );       
   begin
      for i in 1 .. p loop
         s.Include( a.Element( i ));
      end loop;
      return s;
   end To_Set;

   function To_List( s : Sernum_Set ) return Sernum_List is
      use Sernum_Set_Package;
      a : Sernum_List;
      c : Cursor := s.First;
   begin
      a.Append( Element( c ));
      loop
         Next( c );         
         a.Append( Element( c ));
         exit when c = s.Last;
      end loop;
      return a;
   end To_List;
   
   function Difference( left, right : Sernum_List ) return Sernum_List is
      use Sernum_Set_Package;
      s1 : Sernum_Set := To_Set( left );
      s2 : Sernum_Set := To_Set( right );      
   begin
      return To_List( s1 - s2 );
   end Difference;

   function To_String( l : Sernum_List ) return String is
      use Sernum_List_Package;
      use Ada.Strings.Unbounded;
      s : Unbounded_String;
      c : Cursor := l.First;
   begin
      s := s & Element( c )'Img & ",";
      loop
         Next( c );         
         s := s & Element( c )'Img;
         exit when c = l.Last;
         s := s & ",";
      end loop;      
      return TS( s );
   end To_String;
   
   procedure Rebuild_Lists( db : in out DB_Type ) is
      
      use Id_And_Wave_Map_Package;
      
      procedure Write_One( pos : Cursor ) is
         iw    : Id_And_Wave := Key( pos );
      begin
         db.hhlds_in_wave( iw.wave ).Append( iw.id );
      end Write_One;
      
   begin
      for wave in Waves loop
         db.hhlds_in_wave( wave ).Clear;
      end loop;
      Iterate( db.hh_ptrs, Write_One'Access );
      db.is_dirty := ( others => False );
   end Rebuild_Lists;

   procedure Put_Index( imap : in out Id_And_Wave_Index_Map; idw : Id_Wave_And_Pos  ) is
      iw : Id_And_Wave := ( id=>idw.id, wave => idw.wave ); 
   begin
      if( imap.Contains( iw ))then
         imap.Replace( iw, idw.pos );
      else      
         imap.Insert( iw, idw.pos );
      end if;
   end Put_Index;

   procedure Restore_Complete_Index( directory : String; imap : in out Id_And_Wave_Index_Map ) is
      use Ada.Directories;
      use Id_Wave_And_Pos_IO;
      infile : Id_Wave_And_Pos_IO.File_Type;
      iwp    : Id_Wave_And_Pos;
   begin
      if( Exists( directory ) ) then
         Id_Wave_And_Pos_IO.Open( infile, Id_Wave_And_Pos_IO.In_File, directory  );
         loop 
            Id_Wave_And_Pos_IO.Read( infile, iwp );
            Put_Index( imap, iwp );
            exit when( Id_Wave_And_Pos_IO.End_Of_File ( infile ) );
         end loop;      
         Id_Wave_And_Pos_IO.Close( infile );
      end if;
   end Restore_Complete_Index;
   
   procedure Dump_Index( imap : Id_And_Wave_Index_Map ) is
      use Id_And_Wave_Map_Package;
      
      procedure Print( c : Cursor ) is
         idw : Id_And_Wave := Key( c );
         pos : Positive := Element( c );
      begin
         Log( "idw.id " & idw.id'Img & " idw.wave " & idw.wave'Img & " pos " & pos'Img );
      end Print;
   begin    
      imap.Iterate( Print'Access );
   end Dump_Index;

   function Make_Dir_Name( 
      data_directory : String;
      which          : Data_Origin;
      iteration      : Iteration_Number ) return String is
      iteration_string : constant String := Iteration_Number'Image( iteration );
   begin
      if( which = actual )then
         return data_directory & Text_Utils.DIR_SEPARATOR; 
      else
         declare
            iteration_string : constant String := Iteration_Number'Image( iteration )( 2 .. Iteration_Number'Image( iteration )'Last );
         begin
            return data_directory & Text_Utils.DIR_SEPARATOR & iteration_string & Text_Utils.DIR_SEPARATOR;
         end;
      end if;
   end Make_Dir_Name;
   
   -- TODO rebuild lists finish
   
   procedure Open( 
      db             : in out DB_Type; 
      data_directory : String;
      which          : Data_Origin;
      iteration      : Iteration_Number  ) is
      dir_name : constant String := Make_Dir_Name( data_directory, which, iteration );
   begin
      Log( "opening files from directory " & dir_name );
      db.data_directory( which ) := TuS( dir_name );
      db.is_active( which ) := True;
      --
      -- error recovery hack. Sometimes if there's a failure these files are left open. So harmlessly close them
      --
      begin
         Person_IO.Close( db.pers_f( which ));
         HH_Skel_IO.Close( db.skel_f( which ));
         HH_Data_IO.Close( db.hdata_f( which ));
      exception 
         when others=>Log( "close failed" );
      end;
      --
      -- see: http://docs.adacore.com/gnat-unw-docs/html/gnat_rm_10.html for the shared=no parameter
      --
      Person_IO.Open( db.pers_f( which ) , Person_IO.In_File, dir_name & "pers.bin", "shared=no" );
      HH_Skel_IO.Open( db.skel_f( which ) ,HH_Skel_IO.In_File, dir_name & "skel.bin", "shared=no" );
      HH_Data_IO.Open( db.hdata_f( which ), HH_Data_IO.In_File, dir_name & "hdata.bin", "shared=no" );
      if( which = actual )then
         db.pers_ptrs.Clear; 
         db.hh_ptrs.Clear;
      end if;
      Restore_Complete_Index( dir_name & "hh_index.bin", db.hh_ptrs );
      Restore_Complete_Index( dir_name & "pers_index.bin", db.pers_ptrs );
      db.Rebuild_Lists;
   end Open;
   
   procedure Store_Complete_Index( filename : String; imap : Id_And_Wave_Index_Map; which : Data_Origin ) is
      use Id_And_Wave_Map_Package;
      
      outfile : Id_Wave_And_Pos_IO.File_Type;
      
      procedure Write_One( pos : Cursor ) is
         index : Positive := Element( pos );
         iw    : Id_And_Wave := Key( pos );
         iwp   : Id_Wave_And_Pos := ( wave => iw.wave, pos => index, id => iw.id );
         this_origin : Data_Origin := Which_Data_Origin( iw.wave );         
      begin
         if( this_origin = which )then
            Id_Wave_And_Pos_IO.Write( outfile, iwp );
         end if;
      end Write_One;
      
   begin
      Id_Wave_And_Pos_IO.Create( outfile, Id_Wave_And_Pos_IO.Inout_File, filename );
      Iterate( imap, Write_One'Access );
      Id_Wave_And_Pos_IO.Close( outfile );
   end Store_Complete_Index;
      
   procedure Close( db : in out DB_Type ) is
   begin
      for origin in Data_Origin loop
         if( db.is_active( origin ))then
            Log( "Closing Files for " & origin'Img );
            Person_IO.Close( db.pers_f( origin ));
            HH_Skel_IO.Close( db.skel_f( origin ));
            HH_Data_IO.Close( db.hdata_f( origin ));
            if( db.is_dirty( origin ))then
               db.Rebuild_Lists;
               Store_Complete_Index( TS( db.data_directory( origin )) & "hh_index.bin", db.hh_ptrs, origin );
               Store_Complete_Index( TS( db.data_directory( origin )) & "pers_index.bin", db.pers_ptrs, origin );
            end if;
            for wave in Waves loop
               db.hhlds_in_wave( wave ).Clear;
            end loop;
         end if;
      end loop;
      db.pers_ptrs.Clear;
      db.hh_ptrs.Clear;
   end Close;
   
   procedure Create( 
      db             : out DB_Type; 
      data_directory : String;
      which          : Data_Origin;
      iteration      : Iteration_Number  ) is
      iteration_string : constant String := Iteration_Number'Image( iteration );
      target_dir : constant String := Make_Dir_Name( data_directory, which, iteration );
   begin
      Ada.Directories.Create_Path( target_dir );
      db.is_active( which ) := True;
      db.data_directory( which ) := TuS( target_dir );
      Person_IO.Create( db.pers_f( which ), Person_IO.Inout_File, target_dir  & "pers.bin" );
      HH_Skel_IO.Create( db.skel_f( which ), HH_Skel_IO.Inout_File, target_dir & "skel.bin" );
      HH_Data_IO.Create( db.hdata_f( which ), HH_Data_IO.Inout_File, target_dir  & "hdata.bin" );
   end Create;
   
   function Household_Exists( db : in DB_Type; wave : Waves; hid : Sernum_Value ) return Boolean is
      hh       : Household;
      idw      : Id_And_Wave := ( wave=>wave, id => hid );
      skel     : Household_Skeleton;
      pers     : Person;
      location : Positive;
      which    : Data_Origin := Which_Data_Origin( wave );
   begin
      return db.hh_ptrs.Contains( idw );
   end Household_Exists;   
      
   
   function Get_Household( db : in DB_Type; wave : Waves; hid : Sernum_Value ) return Household is
      hh       : Household;
      idw      : Id_And_Wave := ( wave=>wave, id => hid );
      skel     : Household_Skeleton;
      pers     : Person;
      location : Positive;
      which    : Data_Origin := Which_Data_Origin( wave );
   begin
      if( not db.hh_ptrs.Contains( idw ))then
         Log( "Get_Household; looking for hid " & idw.id'Img & " wave " & idw.wave'Img & " NOT FOUND IN " );
         Dump_Index( db.hh_ptrs );
      end if;
      Log( "looking for " );
      location := db.hh_ptrs.Element( idw );
      HH_Data_IO.Read( db.hdata_f( which ), hh.hdata, HH_Data_IO.Count( location ));
      HH_Skel_IO.Read( db.skel_f( which ), skel, HH_Skel_IO.Count( location ) );
      hh.hid := hh.hdata.hid;
      hh.wave := hh.hdata.wave;
      hh.num_benefit_units := skel.num_benefit_units;
      Log( "skel.num_people " & skel.num_people'Img & 
                 " skel.num_benefit_units " & skel.num_benefit_units'Img );
      for pno in 1 .. skel.num_people loop
         idw.id := skel.people( pno ).pid;
         declare
            buno : Benefit_Unit_Number := skel.people( pno ).buno;
            adno : Person_Count := skel.people( pno ).adno;
            chno : Person_Count := skel.people( pno ).chno;
            pos  : Person_IO.Count := Person_IO.Count( skel.people( pno ).file_location );
         begin
            
            -- Ada.Text_IO.Log( "which " & which'Img & 
            --    " pos " & pos'Img );
            Person_IO.Read( db.pers_f( which ), pers, pos );
            hh.benefit_units( buno ).num_people :=  hh.benefit_units( buno ).num_people + 1;
            if( adno > 0 )then
               hh.benefit_units( buno ).adults( adno ) := pers;
               hh.benefit_units( buno ).num_adults := hh.benefit_units( buno ).num_adults + 1;
            elsif( chno > 0 )then
               hh.benefit_units( buno ).children( chno ) := pers;
               hh.benefit_units( buno ).num_children := hh.benefit_units( buno ).num_children + 1;
            else
               Assert( False, " out of sequence error in hhld " & hid'Img & " pno " & pno'Img );
            end if;
         end;
      end loop;
      return hh;
   end Get_Household;
   
   function Get_Person( db : in DB_Type ; wave : Waves; pid : Sernum_Value ) return Person is
      idw   : Id_And_Wave := ( wave=>wave, id => pid );
      pers  : Person;
      pos   : Person_IO.Count;
      which : Data_Origin := Which_Data_Origin( wave );
   begin
      if( db.pers_ptrs.Contains( idw ))then
         pos := Person_IO.Count( db.pers_ptrs.Element( idw ));
         Person_IO.Read( db.pers_f( which ), pers, pos );
      else
         pers.pid := 0;
      end if;      
      return pers;
   end Get_Person;
   
   function To_String( idw : Id_And_Wave ) return String is
      s : String := " idw : wave=" & idw.wave'Img & "; id=" & idw.id'Img & "; ";
   begin
      return s;
   end To_String;
   
   procedure Write_Household( db : in out DB_Type; hh : Household ) is
      skel        : Household_Skeleton;
      pno         : Person_Count := Person_Count'First;
      pptr        : Person_Pointer;
      idw         : Id_And_Wave;
      which       : constant Data_Origin := Which_Data_Origin( hh.wave );
      file_pos    : Positive;
      hh_file_pos : Positive;
      hsize       : constant HH_Data_IO.Positive_Count := HH_Data_IO.Positive_Count'Succ( HH_Data_IO.Size( db.hdata_f( which )));
      psize       : Person_IO.Positive_Count;
   begin
      -- Ada.Text_IO.Log( "which : " & which'Img );
      idw.wave := hh.wave;
      
      -- this code ensures we always write to the end of the file even
      -- if there's been a seek and read to some othe point
      HH_Data_IO.Write( db.hdata_f( which ), hh.hdata, hsize );
      hh_file_pos := Positive( HH_Data_IO.Index( db.hdata_f( which )))-1;
      for buno in 1 .. hh.num_benefit_units loop
         for adno in 1 .. hh.benefit_units( buno ).num_adults loop
            pno := pno + 1;
            pptr.pid := hh.benefit_units( buno ).adults( adno ).pid;
            pptr.pno := hh.benefit_units( buno ).adults( adno ).pno;
            pptr.buno := buno;
            pptr.adno := adno;
            pptr.chno := 0;
            pptr.hdsp := hh.benefit_units( buno ).adults( adno ).hdsp;
            
            idw.id := pptr.pid;
            
            -- index to always write to eof
            psize := Person_IO.Positive_Count'Succ( Person_IO.Size( db.pers_f( which )));            
            Person_IO.Write( db.pers_f( which ), hh.benefit_units( buno ).adults( adno ), psize );
            file_pos := Positive( Person_IO.Index( db.pers_f( which )))-1;
            
            if( db.pers_ptrs.Contains( idw ))then
               if( WARN_ON_DUPS )then
                  Log( "pers-ptrs; warning: attempting to add duplicate idw " & To_String( idw ));
                  Log( "db.pers_ptrs is " );
                  Dump_Index( db.pers_ptrs );
               end if;
               db.pers_ptrs.Replace( idw, file_pos );
            else
               db.pers_ptrs.Insert( idw, file_pos );
            end if;

            pptr.file_location := file_pos;
            skel.people( pno ) := pptr;  
            
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
            idw.id := pptr.pid;
            
            -- always write to end
            psize := Person_IO.Positive_Count'Succ( Person_IO.Size( db.pers_f( which )));
            Person_IO.Write( db.pers_f( which ), hh.benefit_units( buno ).children( chno ), psize );
            file_pos := Positive( Person_IO.Index( db.pers_f( which )))-1;
            
            if( db.pers_ptrs.Contains( idw ))then
               if( WARN_ON_DUPS )then
                  Log( "pers-ptrs; warning: attempting to add duplicate idw " & To_String( idw ));
                  Log( "db.pers_ptrs is " );
                  Dump_Index( db.pers_ptrs );
               end if;
               db.pers_ptrs.Replace( idw, file_pos );
            else
               db.pers_ptrs.Insert( idw, file_pos );
            end if;

            pptr.file_location := file_pos;
            skel.people( pno ) := pptr;  
         end loop;
      end loop;
      skel.wave := hh.wave;
      skel.hid := hh.hid;
      skel.num_people := pno;
      skel.num_benefit_units := hh.num_benefit_units;
      HH_Skel_IO.Write( db.skel_f( which ), skel, HH_Skel_IO.Positive_Count( hsize ));
      idw.id := hh.hdata.hid;
      if( db.hh_ptrs.Contains( idw ))then
         if( WARN_ON_DUPS )then
            Log( "warning: attempting to add duplicate idw " & To_String( idw ));
            Log( "db.hh_ptrs is " );
            Dump_Index( db.hh_ptrs );
         end if;
         db.hh_ptrs.Replace( idw, hh_file_pos );
      else
         Log( "Write_Household; adding " & To_String( idw ));
         db.hh_ptrs.Insert( idw, hh_file_pos );
      end if;
      db.is_dirty( which ) := True;
   end Write_Household;
      
   procedure Delete_Household( db : in out DB_Type; wave : Waves; hid : Sernum_Value  )is
      idw      : Id_And_Wave := ( id=>hid, wave => wave );
      skel     : Household_Skeleton;
      idx      : Sernum_List_Package.Extended_Index := db.hhlds_in_wave( wave ).Find_Index( hid );
      location : Positive := db.hh_ptrs.Element( idw );
      which    : Data_Origin := Which_Data_Origin( wave );
   begin
      HH_Skel_IO.Read( db.skel_f( which ), skel, HH_Skel_IO.Count( location ) );
      db.hh_ptrs.Delete( idw );      
      db.hhlds_in_wave( wave ).Delete( idx );
      for pno in 1 .. skel.num_people loop
         idw.id := skel.people( pno ).pid;
         db.pers_ptrs.Delete( idw );
      end loop;
      db.is_dirty( which ) := True;
   end Delete_Household;
   
   procedure Get_Household_Sernums( 
      db      : in out DB_Type; 
      wave    : Waves;
      sernums : out Sernum_List ) is
      which    : Data_Origin := Which_Data_Origin( wave );
   begin
      if( db.is_dirty( which ))then
         db.Rebuild_Lists;
      end if;
      sernums := db.hhlds_in_wave( wave );
   end Get_Household_Sernums;
   
   procedure Merge( 
      out_db : in out DB_Type; 
      db1    : in out DB_Type; 
      db2    : in out DB_Type;
      origin : Data_Origin := estimated ) is
      sernums1       : Sernum_List;
      sernums2       : Sernum_List;
      num_households1 : Natural;
      num_households2 : Natural;
      hid1, hid2 : Sernum_Value;
      hh : Household;
  begin
     if( origin = estimated )then
         for wave in Estimated_Data_Waves loop
            db1.Get_Household_Sernums( wave, sernums1 );
            db2.Get_Household_Sernums( wave, sernums2 );
            num_households1 := Natural( sernums1.Length );
            num_households2 := Natural( sernums2.Length );
            -- elements in db1 and possibly in db2 (use db2 if so)
            for hhref in  1 .. num_households1 loop
               hid1 := sernums1.Element( hhref );
               if( sernums2.Contains( hid1 ))then
                  hh := db2.Get_Household( wave, hid1 );
               else
                  hh := db1.Get_Household( wave, hid1 );
               end if;
               out_db.Write_Household( hh );               
            end loop;
            --
            -- db2 elements not in db1
            for hhref in 1 .. num_households2 loop
               hid2 := sernums2.Element( hhref );
               if( not sernums1.Contains( hid2 ))then
                  hh := db2.Get_Household( wave, hid2 );
                  out_db.Write_Household( hh );
               end if;
            end loop;            
         end loop;
     end if;     
  end Merge;

      
end Model.WSC.Household.Database;
