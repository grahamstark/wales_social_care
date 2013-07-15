--
-- Created by ada_generator.py on 2012-05-22 11:59:45.752658
-- 
with DB_Commons;
with Ada.Strings.Unbounded;
with WSC_Enums;

package Uap_Threshold_IO is
  
   package d renames DB_Commons;   
   use WSC_Enums;
   use Ada.Strings.Unbounded;
   
   function Retrieve( 
      run_id : integer; 
      username : Unbounded_String; 
      sysno : integer; 
      iteration : integer; 
      wave : Waves ) return UAP_Array;
    
   procedure Save( 
      run_id : integer; 
      username : Unbounded_String; 
      sysno : integer; 
      iteration : integer; 
      wave : Waves;
      uaps : UAP_Array; 
      overwrite : Boolean := True );
 
end Uap_Threshold_IO;
