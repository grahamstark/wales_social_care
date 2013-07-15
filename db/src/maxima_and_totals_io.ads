--
-- Created by ada_generator.py on 2012-07-24 19:03:55.793306
-- 
with Wsc_Db_Data;
with DB_Commons;
with Base_Types;
with ADA.Calendar;
with Ada.Strings.Unbounded;

with WSC_Enums;
with Base_Types;

package Maxima_And_Totals_IO is
  
   package d renames DB_Commons;   
   use Base_Types;
   use Ada.Strings.Unbounded;
   
   use WSC_Enums;
   use Base_Types;
   

   --
   -- returns true if the primary key parts of Maxima_And_Totals match the defaults in Wsc_Db_Data.Null_Maxima_And_Totals
   --
   function Is_Null( Maxima_And_Totals : Wsc_Db_Data.Maxima_And_Totals ) return Boolean;
   
   --
   -- returns the single Maxima_And_Totals matching the primary key fields, or the Wsc_Db_Data.Null_Maxima_And_Totals record
   -- if no such record exists
   --
   
   
   --
   -- Retrieves a list of Wsc_Db_Data.Maxima_And_Totals matching the criteria, or throws an exception
   --
   function Retrieve( c : d.Criteria ) return Wsc_Db_Data.Maxima_And_Totals_List.Vector;
   
   --
   -- Retrieves a list of Wsc_Db_Data.Maxima_And_Totals retrived by the sql string, or throws an exception
   --
   function Retrieve( sqlstr : String ) return Wsc_Db_Data.Maxima_And_Totals_List.Vector;
   
   --
   -- Save the given record, overwriting if it exists and overwrite is true, 
   -- otherwise throws DB_Exception exception. 
   --
   procedure Save( Maxima_And_Totals : Wsc_Db_Data.Maxima_And_Totals; overwrite : Boolean := True );
   
   --
   -- Delete the given record. Throws DB_Exception exception. Sets value to Wsc_Db_Data.Null_Maxima_And_Totals
   --
   procedure Delete( Maxima_And_Totals : in out Wsc_Db_Data.Maxima_And_Totals );
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
   procedure Add_Lifetime_La_Contributions( c : in out d.Criteria; Lifetime_La_Contributions : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Lifetime_Client_Contributions( c : in out d.Criteria; Lifetime_Client_Contributions : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Lifetime_Gross_Payments( c : in out d.Criteria; Lifetime_Gross_Payments : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Lifetime_Capital_Contributions( c : in out d.Criteria; Lifetime_Capital_Contributions : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   procedure Add_Highest_La_Contribution( c : in out d.Criteria; Highest_La_Contribution : Real; op : d.operation_type:= d.eq; join : d.join_type := d.join_and );
   --
   -- functions to add an ordering to a criteria
   --
   procedure Add_Lifetime_La_Contributions_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Lifetime_Client_Contributions_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Lifetime_Gross_Payments_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Lifetime_Capital_Contributions_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
   procedure Add_Highest_La_Contribution_To_Orderings( c : in out d.Criteria; direction : d.Asc_Or_Desc );
 
end Maxima_And_Totals_IO;
