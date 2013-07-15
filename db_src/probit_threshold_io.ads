--
-- Created by ada_generator.py on 2012-02-15 10:51:51.381696
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;
with WSC_Enums;

package Probit_Threshold_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   use WSC_Enums;
   
   function Next_Free_Run_Id return integer;

   --
   -- returns true if the primary key parts of Probit_Threshold match the defaults in Wsc_Db_Data.Null_Probit_Threshold
   --
   function Is_Null( Probit_Threshold : Wsc_Db_Data.Probit_Threshold ) return Boolean;
   
   --
   -- returns the single Probit_Threshold matching the primary key fields, or the Wsc_Db_Data.Null_Probit_Threshold record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String; Run_Id : integer; Element : Probit_Threshold_Type ) return Wsc_Db_Data.Probit_Threshold;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Probit_Threshold matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Probit_Threshold_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Probit_Threshold retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Probit_Threshold_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Probit_Threshold : Wsc_Db_Data.Probit_Threshold; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Probit_Threshold
   --
   procedure Delete( Probit_Threshold : in out Wsc_Db_Data.Probit_Threshold );
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
   procedure Add_Element( c : in out d.Criteria; Element : Probit_Threshold_Type; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Threshold( c : in out d.Criteria; Threshold : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Element_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Threshold_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Probit_Threshold_IO;
