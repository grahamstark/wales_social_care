with Ada.Text_IO;
with Text_Utils;
with GNAT.MD5;
with GNATColl.Traces;
with Ada.Calendar.Formatting;

package body Model.WSC.Users is

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.USERS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
   function Compare_Users( left, right : User_Type ) return Boolean is
   begin
      return left = right;
   end Compare_Users;


 
   function To_String( user : User_Type ) return String is
      s : Unbounded_String;
   begin
      s := s & "utype = " & user.utype'Img;
      s := s & "| lang        = " & user.lang'Img;  
      s := s & "| username    = " & user.username   ;
      s := s & "| password    = " & user.password   ;
      s := s & "| title       = " & user.title      ;
      s := s & "| description = " & user.description;
      s := s & "| email       = " & user.email      ;
      s := s & "| work_dir    = " & user.work_dir   ;
      s := s & "| last_used   = " & Ada.Calendar.Formatting.Image( user.last_used );
      return To_String( s );
   end To_String;

   function Validate( 
      users    : User_Maps.Map;
      username : Unbounded_String; 
      password : Unbounded_String ) return User_Type is
      
      use User_Maps;
      user : User_Type := INVALID_USER;
   begin
      Log( "looking for username " & TS( username ) & " password " & TS( password ));
      if( Contains( users, username )) then
         user := Element( users, username );
         Log( "found user " );
         if( user.password /= GNAT.MD5.Digest( TS( password ) )) then
            Log( "paswords don't match" );
            user := INVALID_USER;
         end if;
      end if;
      return user;
   end Validate;
   
  

-- begin
   -- User_Maps.Insert( 
       -- users,
       -- TuS("a_user"),
       -- ( username    => TuS("a_user"),
         -- utype       => registered,
         -- lang        => en,
         -- password    => TuS("xx"),
         -- title       => TuS("Mr X"),
         -- description => TuS(""),
         -- work_dir    => TuS(""),
         -- email       => TuS( "xx@xx.xx" ),
         -- preferences => ( use_svg_graphics=> True, others=> False ) ));
  
end Model.WSC.Users;
