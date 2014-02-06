--
-- Created by ada_generator.py on 2012-02-22 13:19:39.998894
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;
with WSC_Enums;
with WSC_Web_Enums;
with Model.WSC.Run_Settings;

-- M1
with DB_Commons.ODBC; 

package State_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   
   -- M1
   use DB_Commons.ODBC; 
   
   use WSC_Enums;
   use WSC_Web_Enums;
   use Model.WSC.Run_Settings;
   
   function Next_Free_Run_Id return integer;

   --
   -- returns true if the primary key parts of State match the defaults in Wsc_Db_Data.Null_State
   --
   function Is_Null( State : Wsc_Db_Data.State ) return Boolean;
   
   --
   -- returns the single State matching the primary key fields, or the Wsc_Db_Data.Null_State record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String; Run_Id : integer ) return Wsc_Db_Data.State;
   
   --
   -- Retrieves a list of Wsc_Db_Data.State matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.State_List.Vector;
   
   procedure Cleanup;
   
   
   --
   -- Retrieves a list of Wsc_Db_Data.State retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.State_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   -- M1
   procedure Save( State : Wsc_Db_Data.State; overwrite : Boolean := True; connection : Database_Connection := Null_Database_Connection );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_State
   --
   procedure Delete( State : in out Wsc_Db_Data.State );
   --
   -- delete the records indentified by the criteria
   --
   procedure Delete( c : d.Criteria );
   --
   -- delete all the records identified by the where SQL clause 
   --
   procedure Delete( where_Clause : String );
   --
   -- functions to retrieve records from tables with foreign keys
   -- referencing the table modelled by this package
   --

   --
   -- functions to add something to a criteria
   --
   procedure Add_Username( c : in out d.Criteria; Username : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Username( c : in out d.Criteria; Username : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Run_Id( c : in out d.Criteria; Run_Id : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Household( c : in out d.Criteria; Household : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );

   procedure Add_Other_Counter( c : in out d.Criteria; Other_Counter : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Other_Counter2( c : in out d.Criteria; Other_Counter2 : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Other_Counter3( c : in out d.Criteria; Other_Counter3 : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Other_Counter4( c : in out d.Criteria; Other_Counter4 : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );

   procedure Add_Year( c : in out d.Criteria; Year : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Phase( c : in out d.Criteria; Phase : Phase_Type; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Health( c : in out d.Criteria; Health : Health_Type; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Error_Code( c : in out d.Criteria; Error_Code : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Read_Error( c : in out d.Criteria; Read_Error : boolean; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Session_Id( c : in out d.Criteria; Session_Id : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Session_Id( c : in out d.Criteria; Session_Id : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Household_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Other_Counter_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Other_Counter2_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Other_Counter3_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Other_Counter4_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   
   procedure Add_Year_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Phase_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Health_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Error_Code_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Read_Error_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Session_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end State_IO;
