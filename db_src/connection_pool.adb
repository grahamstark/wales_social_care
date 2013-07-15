with DB_Commons.ODBC;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with GNU.DB.SQLCLI; 
with Ada.Text_IO;
with Utils;
with GNATColl.Traces;

package body Connection_Pool is 

   use Ada.Text_IO;
   
   DEBUG : Boolean := False;
   
   -- FIXME make
   -- this protected type for syncronisation
   
   use Ada.Strings.Unbounded;
   use Ada.Containers;
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "CONNECTION_POOL" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;
   
   function Equal_Connections( left, right : dodbc.Database_Connection ) return Boolean is
      use GNU.DB.SQLCLI;
   begin
      return left.Environment = right.Environment and left.connection = right.connection;
   end Equal_Connections;
   
   package Connection_List_Package is new Ada.Containers.Vectors( 
      Index_Type   => Positive, 
      Element_Type => dodbc.Database_Connection,
      "=" => Equal_Connections );
   
   subtype Connection_List is Connection_List_Package.Vector;
   
   protected type Pool_Type is

      entry Get( conn : out dodbc.Database_Connection );
      
      entry Shutdown;
      
      entry Return_Connection( conn : in out dodbc.Database_Connection );
      
      entry Initialise( 
         server_name  : String;
         user_name    : String;
         password     : String;
         initial_size : Positive );
         
   private
      used_connections : Connection_List;
      free_connections : Connection_List;
      size             : Natural := 0;
   end Pool_Type;
   
   protected body Pool_Type is
      
      entry Initialise( 
         server_name  : String;
         user_name    : String;
         password     : String;
         initial_size : Positive ) when size = 0 is
         conn : dodbc.Database_Connection;
      begin
         size           := initial_size;
         for i in 1 .. initial_size loop
            conn := dodbc.Connect( server_name, user_name, password );
            free_connections.Append( conn );
         end loop;
     end Initialise;
      
      entry Get( conn : out dodbc.Database_Connection ) when free_connections.Length > 0 is
      begin
         DEBUG := Natural( free_connections.Length ) = 0 or 
            ( Natural( free_connections.Length ) < size/2 and size > 5 );
         if( DEBUG )then
            Log( "GET_Connection; used_connections = " & 
               used_connections.Length'Img & 
               " Free connections " & free_connections.Length'Img 
               & " size " & size'Img );
            Log( "called from " & Utils.Get_Stack_Trace( "Get Connection" ));
         end if;
         conn := free_connections.Last_Element;
         used_connections.Append( conn );
         free_connections.Delete_Last;         
      end Get;
      
      entry Shutdown when size > 0 is
      use Connection_List_Package;
         
         num_closed : Natural := 0;
      
         procedure Close( cur : Cursor ) is
            c : dodbc.Database_Connection := Element( cur );
         begin
            num_closed := num_closed + 1;
            dodbc.Disconnect( c );
         end Close;
         
      begin
         Log( "Connection Pool Shut down Started" );
         used_connections.Iterate( Close'Access );
         used_connections.Clear;
         free_connections.Iterate( Close'Access );
         free_connections.Clear;
         Log( "Connection Pool Shut down OK; " & num_closed'Img & " closed " );
      end Shutdown;

      entry Return_Connection( conn : in out dodbc.Database_Connection ) when used_connections.Length > 0 is
         use Connection_List_Package;
         cur : Cursor:= used_connections.Find( conn );
         unc : constant dodbc.Database_Connection := dodbc.Null_Database_Connection;   
      begin
            used_connections.Delete( cur );
            free_connections.Append( conn );
            conn := unc;
            if( DEBUG )then
               Log( "Return_Connection; used_connections = " & 
                  used_connections.Length'Img & 
                  " Free connections " & free_connections.Length'Img 
                  & " size " & size'Img );
            Log( "called from " & Utils.Get_Stack_Trace( "Return Connection" ));
            end if;
      end Return_Connection;
      
   end Pool_Type;
   
   p : Pool_Type;
   
 
   procedure Initialise( 
      server_name  : String;
      user_name    : String;
      password     : String;
      initial_size : Positive ) is
   begin
      p.Initialise( server_name, user_name, password, initial_size );
   end Initialise;
   
   function Lease return dodbc.Database_Connection is
      c : dodbc.Database_Connection;
      -- n : constant Natural := Natural( free_connections.Length );
   begin
      p.Get( c );
      return c;
   end Lease;
   
   procedure Return_Connection( c : in out dodbc.Database_Connection ) is
   use Connection_List_Package;
   begin
      p.Return_Connection( c );
   end  Return_Connection;
   
   procedure Shutdown is
   begin
      p.Shutdown;
   end Shutdown;
   
end Connection_Pool;
