with Ada.Strings.Unbounded;
with Ada.Calendar;
with WSC_DB_Data;
with Model.WSC.Run_Declarations;
with Base_Types;
with Keyed_Text_Buffer;

package Key_Value_DAO is
   
   use WSC_DB_Data;
   use Model.WSC.Run_Declarations;
   use Ada.Strings.Unbounded;
   use Ada.Calendar;
   use Base_Types;
      
   function Load_To_Text_Buffer( r : Run ) return Keyed_Text_Buffer.Text_Buffer; 
   procedure Store_From_Text_Buffer( r : Run; buff : Keyed_Text_Buffer.Text_Buffer );
   
   function Read( r : Run; key : Unbounded_String ) return Real;
   function Read( r : Run; key : Unbounded_String ) return Integer;
   function Read( r : Run; key : Unbounded_String ) return Unbounded_String;
   function Read( r : Run; key : Unbounded_String ) return String;
   function Read( r : Run; key : Unbounded_String ) return Boolean;
   function Read( r : Run; key : Unbounded_String ) return Time;

   procedure Write( r : Run; key : Unbounded_String; value : Real );
   procedure Write( r : Run; key : Unbounded_String; value : Integer );
   procedure Write( r : Run; key : Unbounded_String; value : Unbounded_String );
   procedure Write( r : Run; key : Unbounded_String; value : String );
   procedure Write( r : Run; key : Unbounded_String; value : Boolean );
   procedure Write( r : Run; key : Unbounded_String; value : Time );
   
end Key_Value_DAO;
