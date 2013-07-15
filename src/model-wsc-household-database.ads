--
-- copyright(c) 2011 Graham Stark/ Virtual Worlds (graham.stark@virtual-worlds.biz)/ Howard Reed, Landman Economics (howard@landman-economics.co.uk)
--
-- ////////////////////////////////
--
-- This is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 3, or (at your option)
-- any later version.
-- 
-- It is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this software; see the file docs/gpl_v3.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street,
-- Boston, MA 02110-1301, USA.
-- 
-- /////////////////////////////

pragma License( Modified_GPL );

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Direct_IO;
with Ada.Strings.Unbounded;

--
-- This is the main dataset abstraction.
-- It contains code to create a database of households and people and read back the households or people by (H|P)ID
-- Internally, the dataabase is split into actual and simulated sections, with the simulated part possibly stored 
-- on a per user/run basis but 1 copy of the actual data. Access across the actual/created waves is transparent to the
-- user.
-- 
package Model.WSC.Household.Database is
   
   package Sernum_List_Package is new Ada.Containers.Vectors(
      Element_Type => Sernum_Value,
      Index_Type   => Positive
   );
   
   package Sernum_Set_Package is new Ada.Containers.Ordered_Sets(
      Element_Type => Sernum_Value,
      "<" => "<",
      "=" => "=" );      
   
   type Data_Origin is ( actual, estimated );
   
   subtype Sernum_List is Sernum_List_Package.Vector;
   subtype Sernum_Set is Sernum_Set_Package.Set;
   
   function Difference( left, right : Sernum_List ) return Sernum_List;
   function To_Set( a : Sernum_List ) return Sernum_Set;
   function To_List( s : Sernum_Set ) return Sernum_List;

   type DB_Type is tagged limited private;
   
   function Contains_Person( db : DB_Type; wave : Waves; id : Sernum_Value ) return Boolean;
   function Contains_HH( db : DB_Type; wave : Waves; id : Sernum_Value ) return Boolean;
   
   procedure Open( 
      db             : in out DB_Type; 
      data_directory : String;
      which          : Data_Origin;
      iteration      : Iteration_Number );

   procedure Create( 
      db             : out DB_Type; 
      data_directory : String;
      which          : Data_Origin;
      iteration      : Iteration_Number );
      
   procedure Close(  
      db : in out DB_Type ); 

   
   function Get_Household( 
      db   : in DB_Type; 
      wave : Waves; 
      hid  : Sernum_Value ) return Household;
      
   function Get_Person( 
      db   : in DB_Type; 
      wave : Waves; 
      pid  : Sernum_Value ) return Person;
      
   procedure Write_Household( 
      db   : in out DB_Type; 
      hh   : Household );
      
   procedure Delete_Household( 
      db   : in out DB_Type; 
      wave : Waves; 
      hid  : Sernum_Value );

   procedure Get_Household_Sernums( 
      db      : in out DB_Type; 
      wave    : Waves;
      sernums : out Sernum_List );
      
   procedure Merge( 
      out_db : in out DB_Type; 
      db1    : in out DB_Type; 
      db2    : in out DB_Type;
      origin : Data_Origin := estimated ); 
      
   function Household_Exists( db : in DB_Type; wave : Waves; hid : Sernum_Value ) return Boolean;
   
   function To_String( l : Sernum_List ) return String;
   
private

   use Ada.Strings.Unbounded;
   
   procedure Rebuild_Lists( db : in out DB_Type );

   type Id_Wave_And_Pos is record
      id   : Sernum_Value := Sernum_Value'First;
      wave : Waves;    
      pos  : Positive;
   end record;
   
   package Id_Wave_And_Pos_IO is new Ada.Direct_IO( Id_Wave_And_Pos );
   subtype Id_Wave_And_Pos_File is Id_Wave_And_Pos_IO.File_Type;  
   
   type Person_Pointer is record
      pid             : Sernum_Value;
      pno             : Integer;
      buno            : Benefit_Unit_Number;
      adno            : Adult_Count := 0;
      chno            : Child_Count := 0;
      hdsp            : Head_Or_Spouse := Neither;
      file_location   : Positive;
   end record;

   
   -- needed: list of every household in wales
   -- map pid, wave -> pos
   
   type Person_Pointer_Array is array ( Person_Number ) of Person_Pointer;

   type Household_Skeleton is record
      wave              : Waves;
      hid               : Sernum_Value;
      num_people        : Person_Count;
      num_benefit_units : Benefit_Unit_Count;
      people            : Person_Pointer_Array;
   end record;

   
   type Id_And_Wave is record
      id  : Sernum_Value := Sernum_Value'First;
      wave : Waves;      
   end record;
   
   package Person_IO is new Ada.Direct_IO( Person );
   subtype Pers_File_Type is Person_IO.File_Type;
   
   package HH_Data_IO is new Ada.Direct_IO( Household_Data );
   subtype HData_File_Type is HH_Data_IO.File_Type;
   
   package HH_Skel_IO is new Ada.Direct_IO( Household_Skeleton );
   subtype Skel_File_Type is HH_Skel_IO.File_Type;

   function Compare_Id_And_Wave_LT( l, r : Id_And_Wave ) return Boolean;   
   function Compare_Id_And_Wave_EQ( l, r : Positive ) return Boolean;   
      
   package Id_And_Wave_Map_Package is new Ada.Containers.Ordered_Maps( 
       Key_Type => Id_And_Wave, 
       Element_Type => Positive, 
       "=" => Compare_Id_And_Wave_EQ,
       "<" => Compare_Id_And_Wave_LT ); 
   subtype Id_And_Wave_Index_Map is Id_And_Wave_Map_Package.Map;
   subtype Id_And_Wave_Index_Cursor is Id_And_Wave_Map_Package.Cursor;
      
   type Sernum_By_Wave is array( Waves ) of Sernum_List;
   
   type Id_Wave_And_Pos_File_Array is array( Data_Origin ) of Id_Wave_And_Pos_File;
   type Pers_File_Type_Array is array( Data_Origin ) of Pers_File_Type;
   type Skel_File_Type_Array is array( Data_Origin ) of Skel_File_Type;
   type HData_File_Type_Array is array( Data_Origin ) of HData_File_Type;
   type Boolean_Array is array( Data_Origin ) of Boolean;
   type Directory_Name_Array is array( Data_Origin ) of Unbounded_String;
   
   type DB_Type is tagged limited record
      data_directory           : Directory_Name_Array;
      index_f                  : Id_Wave_And_Pos_File_Array;
      pers_f                   : Pers_File_Type_Array;
      skel_f                   : Skel_File_Type_Array;
      hdata_f                  : HData_File_Type_Array;
      hh_ptrs                  : Id_And_Wave_Index_Map;
      pers_ptrs                : Id_And_Wave_Index_Map;
      is_dirty                 : Boolean_Array := ( others => False );
      is_active                : Boolean_Array := ( others => False );
      hhlds_in_wave            : Sernum_By_Wave;
   end record;
   
end Model.WSC.Household.Database;
