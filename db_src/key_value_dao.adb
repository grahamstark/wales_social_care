with Key_Value_Parameter_IO;
with Text_Utils;
with Format_Utils;
with Ada.Calendar.Formatting;
with Run_IO;
with GNATColl.Traces;

package body Key_Value_DAO is

   use Text_Utils;
   package Local_Format_Utils is new Format_Utils( Float_Type=>Real, Counter_Type=>Decimal );
   use Local_Format_Utils;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "KEY_VALUE_DAO" );

   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   function Load_To_Text_Buffer( r : Run ) return Keyed_Text_Buffer.Text_Buffer is
      b   : Keyed_Text_Buffer.Text_Buffer;
      kvl : Wsc_Db_Data.Key_Value_Parameter_List.Vector := Run_IO.Retrieve_Associated_Key_Value_Parameters( r );
      n   : Natural := Natural( kvl.Length );
      kv  : Key_Value_Parameter;
   begin
      for i in 1 .. n loop
         kv := kvl.Element( i );
         b.Insert( kv.key, kv.val );
      end loop;
      return b;
   end Load_To_Text_Buffer;
   
   procedure Store_From_Text_Buffer( r : Run; buff : Keyed_Text_Buffer.Text_Buffer ) is
      use  String_Maps_Package;
      
      procedure Store( c : Cursor ) is
         k : Unbounded_String := Key( c );
         v : Unbounded_String := Element( c );
      begin
         Write( r, k, v );
      end Store;
      
   begin
       buff.Iterate( Store'Access );
   end Store_From_Text_Buffer;

   
   function Read( r : Run; key : Unbounded_String ) return Unbounded_String is
      kv : Key_Value_Parameter;
   begin
      Log( "Key_Value_DAO.Read( UBS ) Entered key is |" & TS( key ) & "| ");
      kv :=  Key_Value_Parameter_IO.Retrieve_By_PK( r.username, r.run_id, key );
      Log( "Key_Value_DAO.Read( UBS ) exiting value is |" & TS( kv.val ) & "|" );
      return kv.val;
   end Read;
   
   function Read( r : Run; key : Unbounded_String ) return Real is
      v : Unbounded_String := Read( r, key );
   begin
      return Lenient_Convert( TS( v ));
   end Read;
   
   function Read( r : Run; key : Unbounded_String ) return Integer is
      v : Unbounded_String := Read( r, key );
      s : String := TS( v );
   begin
      Log( "Key_Value_DAO.Read( Integer ) Entered; got value as |" & s & "| " );
      return Integer'Value( s );
   end Read;
   
   function Read( r : Run; key : Unbounded_String ) return String is
      v : Unbounded_String := Read( r, key );
   begin
      return TS( v );
   end Read;
   
   function Read( r : Run; key : Unbounded_String ) return Boolean is
      v : Unbounded_String := Read( r, key );
   begin
      return Boolean'Value( TS( v ));
   end Read;
   
   
   function Read( r : Run; key : Unbounded_String ) return Time is
       v : Unbounded_String := Read( r, key );
   begin
      return Ada.Calendar.Formatting.Value( TS( v ));
   end Read;

   procedure Write( r : Run; key : Unbounded_String; value : Unbounded_String ) is
      kv : Key_Value_Parameter;
   begin
      kv.username := r.username;
      kv.run_id   := r.run_id;
      kv.key      := key;
      kv.val      := value;
      Key_Value_Parameter_IO.Save( kv );
   end Write;
   
   procedure Write( r : Run; key : Unbounded_String; value : Real ) is
   begin
      Write( r, key, TuS( Real'Image( value )));
   end Write;
   
   procedure Write( r : Run; key : Unbounded_String; value : Integer ) is
   begin
      Write( r, key, TuS( Integer'Image( value )));
   end Write;
   
   procedure Write( r : Run; key : Unbounded_String; value : String ) is
   begin
      Write( r, key, TuS( value ));
   end Write;
      
   procedure Write( r : Run; key : Unbounded_String; value : Boolean ) is
   begin
      Write( r, key, TuS( Boolean'Image( value )));
   end Write;
      
   procedure Write( r : Run; key : Unbounded_String; value : Time )is
   begin
      Write( r, key, TuS( Ada.Calendar.Formatting.Image( value )));
   end Write;
   
end Key_Value_DAO;
