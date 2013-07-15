with Ada.Command_Line;
with GNAT.MD5;
with Ada.Text_IO;

procedure Make_MD5 is
   
begin
   Ada.Text_IO.Put_Line( GNAT.MD5.Digest( Ada.Command_Line.Argument( 1 )));
end Make_MD5;
