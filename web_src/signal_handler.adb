with Connection_Pool;
with Ada.Text_IO;
with GNAT.OS_Lib;
with GNATColl.Traces;

package body Signal_Handler is

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "SIGNAL_HANDLER" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   
   protected body Signal_Handler_Protected is
         
      procedure Handle_Shutdown_Cleanly is
         use Ada.Text_IO;
      begin
         Log( "Handle_Shutdown_Cleanly, shutting down" );
         AWS.Server.Shutdown( aws_web_server );
         Connection_Pool.Shutdown;
         Log( "Exiting" );
         Gnat.OS_Lib.OS_Exit( 0 );
      end Handle_Shutdown_Cleanly;
         
   end Signal_Handler_Protected;

end Signal_Handler;
   
   

