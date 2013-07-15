
package Model.WSC.Users.IO is
   
   USER_FILE_NAME : constant String := "user_details.txt";

   function Read_User( filename : String ) return User_Type;
   procedure Write_User( filename : String; user : User_Type );
   
   procedure Load_Users( root : String; users : in out User_Maps.Map );
   procedure Create_User_Files( root : String;  sep : String; user : User_Type );

   -- returns the name of the working directory
   function Create_Directories_For_Run( root : String;  sep: String; user : User_Type; run_number : Positive ) return Unbounded_String;

end Model.WSC.Users.IO;
