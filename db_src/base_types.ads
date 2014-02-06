--
-- Created by ada_generator.py on 2012-02-09 16:45:15.865985
-- 
with Ada.Text_IO; 
with Ada.Strings.Bounded; 
with Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Characters.Conversions;
with Ada.Characters.Latin_1;
with Ada.Calendar;

with Base_Model_Types;
with WSC_Enums;
with WSC_Web_Enums;

with Model.WSC.Run_Settings;
with Text_Utils;

package Base_Types is

   use Ada.Strings.Bounded;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use Base_Model_Types;
   --
   --  standard types we use everywhere
   --
   package wconv renames Ada.Characters.Conversions;
   package stda renames Ada.Characters.Latin_1;
   
   subtype Real is Base_Model_Types.Real;
   subtype Big_Integer is Base_Model_Types.Big_Integer;
   type Decimal is delta 0.01 digits 10;
   --
   --
   --
   DOS_NEW_LINE  : String renames Text_Utils.DOS_LINE_BREAK ;
   UNIX_NEW_LINE : String renames Text_Utils.UNIX_LINE_BREAK;
   
   -- subtype Means_Test_Result   is WSC_Enums.Means_Test_Result;
   -- subtype User_Class          is WSC_Web_Enums.User_Class;
   -- subtype Uprate_Targets      is WSC_Enums.Uprate_Targets;
   -- subtype UAP_Level           is WSC_Enums.UAP_Level;
   -- subtype Forecast_Element    is WSC_Enums.Forecast_Element;
   -- subtype Health_Type         is Model.WSC.Run_Settings.Health_Type;
   -- subtype Phase_Type          is Model.WSC.Run_Settings.Phase_Type;
   -- subtype Incomes_Type        is WSC_Enums.Incomes_Type;
   -- subtype Languages           is WSC_Enums.Languages;
    
   MISSING_I_KEY : constant := -12345678;
   MISSING_S_KEY : constant String := "-12345678";
   MISSING_W_KEY : constant Unbounded_String := To_Unbounded_String("-12345678");
   MISSING_R_KEY : constant := -12345678.0;
   MISSING_T_KEY : constant Ada.Calendar.Time := Ada.Calendar.Time_Of( 2099, 11, 11, 9.0 );
   
   FIRST_DATE    : constant Ada.Calendar.Time := Ada.Calendar.Time_Of( 1901, 1, 1, 0.0 );
   
   --
   -- used for declaring strings as fixed sets of characters
   --
   PADDING_CHAR : constant Character := '@';
   --
   -- used for trim calls on retrieved strings
   --
   PADDING_CHAR_SET : constant Ada.Strings.Maps.Character_Set := 
      Ada.Strings.Maps.To_Set ( PADDING_CHAR );

   Null_Wide : constant Unbounded_String := Null_Unbounded_String;
   
   function Slice_To_Unbounded( s : String; start : Positive; stop : Natural ) return Unbounded_String;
   
   -- 
   --  some standard io packages typed for the above
   --
   package fix_io is new Ada.Text_IO.Decimal_IO (Decimal);
   package real_io is new Ada.Text_IO.Float_IO (real);
   package std_io is new Ada.Text_IO.Integer_IO (Integer);
   package string_io renames Ada.Text_IO;
   package Str80 is new Ada.Strings.Bounded.Generic_Bounded_Length(80);
  
end Base_Types;
