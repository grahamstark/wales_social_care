with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Calendar;


with AWS.Response;
with AWS.Session;
with AWS.Status;

with Text_Utils;
with WSC_Web_Enums;
with WSC_Enums;

package Model.WSC.Users is

   use Ada.Strings.Unbounded;
   use Text_Utils;
   use WSC_Enums;
   use WSC_Web_Enums;
   use Ada.Calendar;
   
   DEFAULT_USERNAME : constant Unbounded_String := To_Unbounded_String( "default" );
   
   type User_Type is record
      username     : Unbounded_String;
      password     : Unbounded_String;
      title        : Unbounded_String;
      description  : Unbounded_String;
      email        : Unbounded_String;
      work_dir     : Unbounded_String;
      utype        : User_Class       := anon;
      lang         : Languages        := Languages'First;
      preferences  : Unbounded_String;
      last_used    : Time;
   end record;


   function Compare_Users( left, right : User_Type ) return Boolean;
   
   package User_Maps is new Ada.Containers.Hashed_Maps(
      Key_Type         => Unbounded_String,
      Element_Type     => User_Type,
      Hash             => Text_Utils.Hash_String,
      "="              => Compare_Users,
      Equivalent_Keys  => Text_Utils.Compare_String );
   
   INVALID_USER : constant User_Type :=
      (utype       => invalid,
       lang        => Languages'First,
       username    => Null_Unbounded_String,
       password    => Null_Unbounded_String,
       title       => Null_Unbounded_String,
       description => Null_Unbounded_String,
       email       => Null_Unbounded_String,
       work_dir    => Null_Unbounded_String,
       last_used   => Time_Of( Year_Number'First, Month_Number'First, Day_Number'First, Day_Duration'First ),
       preferences => Null_Unbounded_String );

   function Validate( 
      users    : User_Maps.Map;
      username : Unbounded_String; 
      password : Unbounded_String ) return User_Type;
      
   function To_String( user : User_Type ) return String;
      
   package User_Session_Data is new AWS.Session.Generic_Data(
       Unbounded_String,
       Null_Unbounded_String );
       
    type Login_Result is record
      user     : User_Type := INVALID_USER;
      response : AWS.Response.Data;
      validated : Boolean := False;
      new_session : Boolean := False;
   end record;
   
private

   procedure Log( s : String );
   
end Model.WSC.Users;
