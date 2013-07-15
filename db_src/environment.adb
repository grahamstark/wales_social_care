--
-- Created by ada_generator.py on 2012-02-09 16:45:15.855604
-- 
package body Environment is
   
   SERVER_NAME     : constant String := "wsc_db2";
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
