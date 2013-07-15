with Translate_Utils;
with Utils;
with Ada.Strings.Unbounded;
with WSC_Enums;
--
-- This contains the base code for our Dutch/English language handling. It contains some basic
-- routines to display and validate numbers in each language, and code to read the language translation files 
-- (the names of which are hard-wired in to the body file).
-- 
package Model.WSC.Formatting is
   
   use Ada.Strings.Unbounded;
   use WSC_Enums;
   
   package Translations is new Translate_Utils( Languages => Languages );
   
   function Lang_Str( lang : Languages ) return String; -- en or nl
   
   procedure Load_Translations;
   
   function Is_Valid_Number( s : String; lang : Languages; is_integer : Boolean := False ) return Boolean;
   
   R_VALIDATE_ERROR : constant := -12345.0;
   VALIDATE_ERROR : constant := -12345;
   
   function Web_Format( c : Amount; lang : Languages ) return String;
   function Web_Format( c : Integer; lang : Languages ) return String;
   function Web_Format( b : Boolean; lang : Languages ) return String;

   function GL_Class( i : Amount; up_is_good : Boolean ) return String;
   function GL_Format( i : Integer; up_is_good : Boolean; lang : Languages ) return String;
   function GL_Format( i : Amount; up_is_good : Boolean; lang : Languages ) return String;
   
   
   
   procedure Web_Validate(
      input_str : String;
      lang      : Languages;
      val       : out Amount;
      message   : out Unbounded_String;
      error     : out Utils.Error_Type;
      min       : Amount := Amount'First;
      max       : Amount := Amount'Last );
      
   procedure Web_Validate(
      input_str : String;
      lang      : Languages;
      val       : out Integer;
      message   : out Unbounded_String;
      error     : out Utils.Error_Type;
      min       : Integer := Integer'First;
      max       : Integer:= Integer'Last );
    
end Model.WSC.Formatting;
