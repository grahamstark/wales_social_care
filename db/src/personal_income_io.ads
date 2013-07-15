--
-- Created by ada_generator.py on 2012-07-24 19:03:55.754415
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

with WSC_Enums;
with Base_Types;

package Personal_Income_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   
   use WSC_Enums;
   use Base_Types;
   
   function Next_Free_Run_Id return integer;
   function Next_Free_Pid return Big_Integer;
   function Next_Free_Sysno return integer;
   function Next_Free_Iteration return integer;

   --
   -- returns true if the primary key parts of Personal_Income match the defaults in Wsc_Db_Data.Null_Personal_Income
   --
   function Is_Null( Personal_Income : Wsc_Db_Data.Personal_Income ) return Boolean;
   
   --
   -- returns the single Personal_Income matching the primary key fields, or the Wsc_Db_Data.Null_Personal_Income record
   -- if no such record exists
   --
   function Retrieve_By_PK( Username : Unbounded_String; Run_Id : integer; Pid : Big_Integer; Sysno : integer; Iteration : integer; Wave : Unbounded_String; Income_Type : personal_income_income_type_Enum ) return Wsc_Db_Data.Personal_Income;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Personal_Income matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Personal_Income_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Personal_Income retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Personal_Income_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Personal_Income : Wsc_Db_Data.Personal_Income; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Personal_Income
   --
   procedure Delete( Personal_Income : in out Wsc_Db_Data.Personal_Income );
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
   procedure Add_Pid( c : in out d.Criteria; Pid : Big_Integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Sysno( c : in out d.Criteria; Sysno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Iteration( c : in out d.Criteria; Iteration : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Wave( c : in out d.Criteria; Wave : Unbounded_String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Wave( c : in out d.Criteria; Wave : String; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Income_Type( c : in out d.Criteria; Income_Type : personal_income_income_type_Enum; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Hid( c : in out d.Criteria; Hid : Big_Integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Buno( c : in out d.Criteria; Buno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Adno( c : in out d.Criteria; Adno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Value( c : in out d.Criteria; Value : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Username_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Run_Id_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Pid_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Sysno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Iteration_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Wave_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Income_Type_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Hid_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Buno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Adno_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Value_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Personal_Income_IO;
