with Ada.Strings.Unbounded;

package Event_Counter.Web_IO is
   
   use Ada.Strings.Unbounded;
   
   function To_String( r : Recorder ) return Unbounded_String;
   
end Event_Counter.Web_IO;
