--
-- Created by ada_generator.py on 2011-11-29 16:58:56.612364
-- 
with Wsc_Db_Data;
with db_commons;
with base_types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

package Maxima_And_Totals_IO is
  
   package d renames db_commons;   
   use base_types;
   use Ada.Strings.Unbounded;
   
   --
   -- returns true if the primary key parts of Maxima_And_Totals match the defaults in Wsc_Db_Data.Null_Maxima_And_Totals
   --
   function Is_Null( Maxima_And_Totals : Wsc_Db_Data.Maxima_And_Totals ) return Boolean;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Maxima_And_Totals matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Maxima_And_Totals_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Maxima_And_Totals retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Maxima_And_Totals_List.Vector;
   


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
   procedure Add_Hid( c : in out d.Criteria; Hid : Big_Integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Buno( c : in out d.Criteria; Buno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Adno( c : in out d.Criteria; Adno : integer; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
  
end Maxima_And_Totals_IO;
