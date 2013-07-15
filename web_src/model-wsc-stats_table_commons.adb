with Templates_Parser;
with GNATColl.Traces;

with Model.WSC.Global_Settings;
with Web_Utils;
With Text_Utils;

package body Model.WSC.Stats_Table_Commons is
   
   use Text_Utils;
   
   package euws renames Model.WSC.Global_Settings; 
    
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.STATS_TABLE_COMMONS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   --
   -- fixme dup of Model.WSC.Output.Web_IO
   --
   function Stats_To_Table( 
      stats      : WSC_DB_Data.Table_Stats; 
      title      : String;
      id         : String;
      divisor    : Amount;
      class      : String;
      as_real    : Boolean;
      which      : WSC_DB_Data.Stats_Selection := 1 ) return Unbounded_String is
   use Templates_Parser;
      
      translations : Translate_Set;
      
      function Local_Format( x : Amount ) return String is
      begin
         if not as_real then
            return Format_With_Commas( Counter_Type( x ), False );
         else
            return Format_With_Commas( x );
         end if;
      end Local_Format;
      
      rmean : Real := 0.0;
      rmin  : Real := 0.0;
      rmax  : Real := 0.0;
      rmed  : Real := 0.0;
      sddev : Real := 0.0;
      dec1  : Real := 0.0;
      dec10 : Real := 0.0;

   begin
      case which is    
         when 1 =>
            rmean := stats.rmean_1;
            rmin := stats.rmin_1;
            rmax := stats.rmax_1;
            rmed := stats.rmed_1;
            sddev := stats.sddev_1;
            dec1 := stats.dec1_1;
            dec10 := stats.dec10_1;
         when 2 =>
            rmean := stats.rmean_2;
            rmin := stats.rmin_2;
            rmax := stats.rmax_2;
            rmed := stats.rmed_2;
            sddev := stats.sddev_2;
            dec1 := stats.dec1_2;
            dec10 := stats.dec10_2;
         when 3 =>
            rmean := stats.rmean_3;
            rmin := stats.rmin_3;
            rmax := stats.rmax_3;
            rmed := stats.rmed_3;
            sddev := stats.sddev_3;
            dec1 := stats.dec1_3;
            dec10 := stats.dec10_3;
         when 4 =>
            rmean := stats.rmean_4;
            rmin := stats.rmin_4;
            rmax := stats.rmax_4;
            rmed := stats.rmed_4;
            sddev := stats.sddev_4;
            dec1 := stats.dec1_4;
            dec10 := stats.dec10_4;
         when 5 =>
            rmean := stats.rmean_5;
            rmin := stats.rmin_5;
            rmax := stats.rmax_5;
            rmed := stats.rmed_5;
            sddev := stats.sddev_5;
            dec1 := stats.dec1_5;
            dec10 := stats.dec10_5;
         when 6 =>
            rmean := stats.rmean_6;
            rmin := stats.rmin_6;
            rmax := stats.rmax_6;
            rmed := stats.rmed_6;
            sddev := stats.sddev_6;
            dec1 := stats.dec1_6;
            dec10 := stats.dec10_6;
      end case;
      
      Insert( translations, Assoc( "TITLE", title ));
      Insert( translations, Assoc( "ID", id ));
      Insert( translations, Assoc( "CLASS", class ));
      Insert( translations, Assoc( "SMALL-MEAN", Local_Format( rmean/divisor )));
      Insert( translations, Assoc( "MEAN", Local_Format( rmean )));
      Insert( translations, Assoc( "MIN", Local_Format( rmin )));
      Insert( translations, Assoc( "MAX", Local_Format( rmax )));
      Insert( translations, Assoc( "MED", Local_Format( rmed )));
      Insert( translations, Assoc( "SDEV", Local_Format( sddev )));
      Insert( translations, Assoc( "DEC1", Local_Format( dec1 )));
      Insert( translations, Assoc( "DEC10", Local_Format( dec10 )));
      return Web_Utils.Parse_Template( TuS( euws.template_components_path ) & 
         euws.Dir_Separator & "single_stat_table", translations );      
   end Stats_To_Table;

end Model.WSC.Stats_Table_Commons;
