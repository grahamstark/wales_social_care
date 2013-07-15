with WSC_Enums;
with Ada.Text_IO;
with Base_Model_Types;
with Model.WSC.Household;
with ONS_Definitions;

-- A not-quite generic hack at a counter for events like births, deaths.
-- Horrible bit of code, mix of general and bhps-specific
-- TODO
-- maybe extend this into a proper observer with the add being an interface and recorder being a map of some
-- Less horrible, generally.
--
generic
   
   type Event_Type is (<>);
   with function Is_Population( t : Event_Type ) return Boolean; 
   Population_Field : Event_Type;
   
package Event_Counter is
   
   use WSC_Enums;
   use Base_Model_Types;
   use Model.WSC.Household;
     
   type Recorder is tagged private;

   type Event_Array is array( Event_Type, Waves, Gender_Type, ONS_Definitions.Age_Band ) of Amount;
   
   function Get_Raw( log : Recorder ) return Event_Array;

   function Get_Weighted( log : Recorder ) return Event_Array;

   function To_String( 
      ev : Event_Array; 
      start_age : ONS_Definitions.Age_Band := ONS_Definitions.Age_Band'First ) return String;

   function To_Per_1000_People( ev : Event_Array ) return Event_Array;

   function Population_Count( 
      ev : Event_Array; 
      wave : Waves;
      start_age : ONS_Definitions.Age_Band := ONS_Definitions.Age_Band'First ) return Amount;
   
   procedure Reset( rec : in out Recorder );  
      
   procedure Add(
      log    : in out Recorder;
      event  : Event_Type;
      wave   : Waves;
      hid    : Sernum_Value;
      pid    : Sernum_Value;
      age    : Age_Range;
      sex    : Gender_Type;
      weight : Amount );
      
   procedure Multiply( log : in out Recorder; m : Amount );
      
private
   
   type Event_Rec is record
      event  : Event_Type;
      wave   : Waves;
      hid    : Sernum_Value;
      pid    : Sernum_Value;
      age    : Age_Range;
      sex    : Gender_Type;
      weight : Amount;
   end record; 
   
   type Recorder is tagged record
      weighted : Event_Array := ( others => ( others => ( others => ( others => 0.0 ))));   
      raw      : Event_Array := ( others => ( others => ( others => ( others => 0.0 ))));   
   end record;
   
end Event_Counter;
