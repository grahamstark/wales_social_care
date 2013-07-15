with Ada.Containers.Ordered_Maps;
with Ada.Direct_IO;

package Model.WSC.Household.Database is
   
   type DB_Type is tagged limited private;
   
   procedure Open( 
      db         : out DB_Type; 
      filename   : String );

   procedure Create( 
      db         : out DB_Type; 
      filename   : String );
      
   procedure Close(  
      db : in out DB_Type ); 
   
   
   function Get_Household( db : in DB_Type; wave : Waves; hid : Sernum_Value ) return Household;
   function Get_Person( db : in DB_Type; wave : Waves; pid : Sernum_Value ) return Person;
   -- function Next_In_Wave( db : in DB_Type; curr : Sernum_Value ) return HH_And_Wave;
   procedure Write_Household( db : in out DB_Type; hh : Household );
   procedure Delete_Household( db : in out DB_Type; wave : Waves; hid : Sernum_Value );

   procedure Get_Household_Sernums( 
      db : in out DB_Type; 
      wave : Waves; 
      num_hhs : out Households_Per_Wave_Count; 
      sernums : out Sernum_Array );
   
private
   
   type Person_Pointer is record
      pid             : Sernum_Value;
      pno             : Integer;
      buno            : Benefit_Unit_Number;
      adno            : Adult_Count := 0;
      chno            : Child_Count := 0;
      hdsp            : Head_Or_Spouse := Neither;
      location        : Positive;
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

   function Compare_Id_And_Wave_LT( l, r : Id_And_Wave ) return Boolean;   
   function Compare_Id_And_Wave_LE( l, r : Id_And_Wave ) return Boolean;   
   function Compare_Id_And_Wave_EQ( l, r : Id_And_Wave ) return Boolean;   
   function Get_Key( pers  : Person ) return Id_And_Wave;   
   function Get_Key( skel  : Household_Skeleton ) return Id_And_Wave;   
   function Get_Key( hdata : Household_Data ) return Id_And_Wave;   
   
   package Pers_Seq_IO is new Indexed_Sequential_IO(
         Item_Type      => Person,
         Key_Type       => Id_And_Wave,
         Index_Order    => 128,
         Sequence_Order => 128,
         Get_Key        => Get_Key,
          "="           => Compare_Id_And_Wave_EQ,
          "<"           => Compare_Id_And_Wave_LT,
          "<="          => Compare_Id_And_Wave_LE );
   subtype Pers_File_Type is Pers_Seq_IO.File_Type;
   
   package Skel_Seq_IO is new Indexed_Sequential_IO(
         Item_Type      => Household_Skeleton,
         Key_Type       => Id_And_Wave,
         Index_Order    => 128,
         Sequence_Order => 128,
         Get_Key        => Get_Key,
          "="           => Compare_Id_And_Wave_EQ,
          "<"           => Compare_Id_And_Wave_LT,
          "<="          => Compare_Id_And_Wave_LE );
   subtype Skel_File_Type is Skel_Seq_IO.File_Type;

   package HData_Seq_IO is new Indexed_Sequential_IO(
         Item_Type      => Household_Data,
         Key_Type       => Id_And_Wave,
         Index_Order    => 128,
         Sequence_Order => 128,
         Get_Key        => Get_Key,
          "="           => Compare_Id_And_Wave_EQ,
          "<"           => Compare_Id_And_Wave_LT,
          "<="          => Compare_Id_And_Wave_LE );
   subtype HData_File_Type is HData_Seq_IO.File_Type;

   
   type DB_Type is tagged limited record
      pers_f  : Pers_File_Type;
      skel_f  : Skel_File_Type;
      hdata_f : HData_File_Type;
   end record;
   
end Model.WSC.Household.Database;
