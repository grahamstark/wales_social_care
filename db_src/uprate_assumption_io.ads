--
-- Created by ada_generator.py on 2012-02-10 02:09:15.554041
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

with WSC_Web_Enums;
with Model.Run_Settings;
with WSC_Enums;

package Uprate_Assumption_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;

   use WSC_Enums;
   use WSC_Web_Enums;
   use Model.Run_Settings;
   
   function Next_Free_Run_Id return integer;

   --
   -- returns true if the primary key parts of Uprate_Assumption match the defaults in Wsc_Db_Data.Null_Uprate_Assumption
   --
   function Is_Null( Uprate_Assumption : Wsc_Db_Data.Uprate_Assumption ) return Boolean;
   
   --
   -- returns the single Uprate_Assumption matching the primary key fields, or the Wsc_Db_Data.Null_Uprate_Assumption record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String; Run_Id : integer; Target : Uprate_Targets ) return Wsc_Db_Data.Uprate_Assumption;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Uprate_Assumption matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Uprate_Assumption_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Uprate_Assumption retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Uprate_Assumption_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Uprate_Assumption : Wsc_Db_Data.Uprate_Assumption; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Uprate_Assumption
   --
   procedure Delete( Uprate_Assumption : in out Wsc_Db_Data.Uprate_Assumption );
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
   procedure Add_Percent_Change( c : in out d.Criteria; Percent_Change : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Use_Obr( c : in out d.Criteria; Use_Obr : boolean; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Target( c : in out d.Criteria; Target : Uprate_Targets; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Element( c : in out d.Criteria; Element : Forecast_Element; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Percent_Change_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Use_Obr_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Target_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Element_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Uprate_Assumption_IO;
