--
-- Created by ada_generator.py on 2012-07-24 19:03:55.548032
-- 
package body Environment is
   
   SERVER_NAME     : constant String := "wsc_db";
   USER_NAME       : constant String := "postgres";
   PASSWORD        : Constant String := "";


   function Get_Server_Name return String is
   begin
      return SERVER_NAME;
   end Get_Server_Name;
   
   function Get_Username return String is
   begin
      return USER_NAME;
   end Get_Username;
   
   function Get_Password return String is
   begin
      return PASSWORD;
   end Get_Password;

end Environment;

