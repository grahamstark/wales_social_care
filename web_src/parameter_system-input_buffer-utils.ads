with AWS.Parameters;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;
with Text_Utils;
with Utils;
with Ada.Calendar;

generic

      
package Parameter_System.Input_Buffer.Utils is

   use Ada.Strings.Unbounded;
   
   procedure Write_Parameter_File_Skeleton( 
      filename : String; 
      sys      : Parameter_System_Rec );
    
      
  
   
end Parameter_System.Input_Buffer.Utils;
