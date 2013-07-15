with Ada.Interrupts.Names;
with AWS.Server;

--
-- We declare the actual server here
-- so we can shut it and the connection pool down 
-- cleanly on ctl-c or kill signal
-- 
-- based on an example here:
-- http://www.pegasoft.ca/resources/boblap/14.html
--
package Signal_Handler is

   use Ada.Interrupts;
   use Ada.Interrupts.Names;
   
   aws_web_server     : AWS.Server.HTTP;
   
   protected Signal_Handler_Protected is
     
      pragma Unreserve_All_Interrupts;
      
      procedure Handle_Shutdown_Cleanly;
      
      pragma Attach_Handler( Handle_Shutdown_Cleanly, SIGTERM );
      pragma Attach_Handler( Handle_Shutdown_Cleanly, SIGINT );

   end Signal_Handler_Protected; -- protected
 
end Signal_Handler;
   
   

