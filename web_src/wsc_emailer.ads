with Model.WSC.Run_Declarations;
with Model.WSC.Users;

package WSC_Emailer is
   
use Model.WSC.Run_Declarations;
use Model.WSC.Users;

   type Email_Result is ( ok, no_email_address, email_failed ); 
   
   function Make_Run_End_Email( title : String; user : User_Type; wsc_run : Run ) return String;
   function Send_Run_End_Email( wsc_run : Run ) return Email_Result;
   --
   -- for testing!!
   --
   procedure Send_Email( title : String; to : String; complete_email : String; result : out Email_Result );
  
end WSC_Emailer;
