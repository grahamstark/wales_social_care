with Ada.Text_IO; use Ada.Text_IO;
--
-- from: http://www.stanford.edu/~laurik/fsmbook/examples/Einstein%27sPuzzle.html
--
-- Let us assume that there are five houses of different colors next to each other on the same road. In each house lives
-- a man of a different nationality. Every man has his favorite drink, his favorite brand of cigarettes, and keeps pets of
-- a particular kind.
-- 
-- 1 The Englishman lives in the red house.
-- 2 The Swede keeps dogs.
-- 3 The Dane drinks tea.
-- 4 The green house is just to the left of the white one.
-- 5 The owner of the green house drinks coffee.
-- 6 The Pall Mall smoker keeps birds.
-- 7 The owner of the yellow house smokes Dunhills.
-- 8 The man in the center house drinks milk.
-- 9 The Norwegian lives in the first house.
-- 10 The Blend smoker has a neighbor who keeps cats.
-- 11 The man who smokes Blue Masters drinks bier.
-- 12 The man who keeps horses lives next to the Dunhill smoker.
-- 13 The German smokes Prince.
-- 14 The Norwegian lives next to the blue house.
-- 15 The Blend smoker has a neighbor who drinks water.
-- 
-- The question to be answered is: Who keeps fish?
--

procedure Einstein is
   
   type Colour_Type  is ( unknown, blue,  green ,red ,  white,  yellow);
   type Nationality_Type  is ( unknown,  dane ,  english ,  german ,  swede ,  norwegian );
   type Drink_Type  is ( unknown, beer ,  coffee ,  milk , tea ,  water) ;
   type Fag_Type  is ( unknown, blend ,  bluemaster ,  dunhill ,  pallmall ,  prince );
   type Pet_Type  is ( unknown, bird ,  cat ,  dog ,  fish ,  horse );
   subtype House_Number_Type is Integer range 0 .. 5;
   
   subtype People_Count_Type is Integer range 0 .. 5;
   subtype People_Number_Type is People_Count_Type range 1 .. 5;
   
   type Left_Or_Right is ( left, right );
   
   type People_Rec is record
      house_number : House_Number_Type := 0;
      colour : Colour_Type := unknown;
      nationality : Nationality_Type  := unknown;
      drink : Drink_Type := unknown;
      fag : Fag_Type := unknown;
      pet : Pet_Type := unknown;
   end record;
   
   type People_Array is array ( People_Number_Type ) of People_Rec;
   
   NO_PEOPLE : constant People_Rec := ( 
      house_number => 0, 
      colour => unknown, 
      nationality => unknown, 
      drink=>unknown, 
      fag => unknown,
      pet => unknown );
      
   people : People_Array := ( others => NO_PEOPLE );
   
   procedure Print_People( pers : People_Rec ) is
   begin
      Put( " house_number = " & pers.house_number'Img );
      Put( " colour = " & pers.colour'Img );
      Put( " nationality = " & pers.nationality'Img );
      Put( " drink = " & pers.drink'Img );
      Put( " fag = " & pers.fag'Img );
      Put_Line( " pet = " & pers.pet'Img );
   end Print_People;
   
   function Neighbour( which : People_Number_Type; direction : Left_Or_Right ) return People_Rec is
      neigh : People_Rec := NO_PEOPLE;
      pers : People_Rec renames people( which );
   begin
      if( direction = left and pers.house_number = 1 ) or ( direction = right and pers.house_number = House_Number_Type'Last )then
         return NO_PEOPLE;
      end if;
      if( direction = left )then
         for i in People_Number_Type loop
            if( people( i ).house_number = pers.house_number - 1 )then
               return people( i );
            end if;
         end loop;
      else
         for i in People_Number_Type loop
            if( people( i ).house_number = pers.house_number + 1 )then
               return people( i );
            end if;
         end loop;
      end if;
      return NO_PEOPLE;
   end Neighbour;

   function Passes return Boolean is
   begin
      for p in People_Number_Type loop
         declare
            left_neigh : People_Rec := Neighbour( p, left );
            right_neigh : People_Rec := Neighbour( p, right );
            pers : People_Rec renames people( p );
         begin   
            Print_People( pers );
            case pers.nationality is
            when english =>
               if pers.colour /= red then -- 1
                  Put_Line( "fails #1" );
                  return False;
               end if;
            when swede =>
               if pers.pet /= dog then -- 2
                  Put_Line( "fails #2" );
                  return False; 
               end if;
            when dane =>
               if pers.drink /= tea then -- 3
                  Put_Line( "fails #3" );
                  return False;
               end if;
            when norwegian =>
               if( pers.house_number /= 1 )then -- 9
                  Put_Line( "fails #9" );
                  return False;
               end if;
            when others => null;
            end case;
            case pers.colour is
               when green =>
                  if right_neigh.colour /= white  then -- 4
                     Put_Line( "fails #4" );
                     Put( "Right " );Print_People( right_neigh );
                     return False;
                  end if;
                  if pers.drink /= coffee  then -- 5
                     Put_Line( "fails #5" );
                     return False;
                  end if;
               when yellow =>
                  if pers.fag /= dunhill then -- 7
                     Put_Line( "fails #7" );
                     return false;
                  end if;
               when others => null;                  
            end case;
            case pers.fag is 
               when pallmall => 
                  if pers.pet /= bird then -- 6
                     Put_Line( "fails #6" );
                     return False;
                  end if;
               when blend =>
                  if( left_neigh.pet /= cat ) and ( right_neigh.pet /= cat )then -- 10
                     Put_Line( "fails #10" );
                     Put( "Left " );Print_People( left_neigh );
                     Put( "Right " );Print_People( right_neigh );
                     return False;
                  end if;
                  if( left_neigh.drink /= water ) and ( right_neigh.drink /= water )then -- 15
                     Put_Line( "fails #15" );
                     Put( "Left " );Print_People( left_neigh );
                     Put( "Right " );Print_People( right_neigh );
                     return False;
                  end if;
               when bluemaster =>
                  if pers.drink /= beer then -- 11
                     Put_Line( "fails #11" );
                     return False;
                  end if;
               when prince =>
                  if pers.nationality /= german then -- 13
                     Put_Line( "fails #13" );
                     return False;
                  end if;
               when others => null;
            end case;
            case pers.house_number is
               when 3 =>
                  if pers.drink /= milk then -- 8
                     Put_Line( "fails #8" );
                     return False;
                  end if;
               when others => null;
            end case;
            case pers.pet is
               when horse =>
                  if( left_neigh.fag /= dunhill ) and ( right_neigh.fag /= dunhill )then -- 12
                     Put_Line( "fails #12" );
                     Put( "Left " );Print_People( left_neigh );
                     Put( "Right " );Print_People( right_neigh );
                     return False;
                  end if;
               when others => null;
            end case;
            case pers.nationality is
               when norwegian =>
                  if( left_neigh.colour /= blue ) and ( right_neigh.colour /= blue )then -- 14
                     Put_Line( "fails #14" );
                     Put( "Left " );Print_People( left_neigh );
                     Put( "Right " );Print_People( right_neigh );
                     return False;
                  end if;
               when others => null;
            end case;
         end;
      end loop;
      return True;
   end Passes;
   
   
   colour : Colour_Type := unknown;
   nationality : Nationality_Type  := unknown;
   drink : Drink_Type := unknown;
   fag : Fag_Type := unknown;
   pet : Pet_Type := unknown;

begin   
   for p in 1 .. House_Number_Type'Last loop
      people( p ).house_number := p;
   end loop;
  
   colours:
   for start_c in blue .. Colour_Type'Last loop
      colour := start_c;
      for p in People_Number_Type loop
         people( p ).colour := colour;
         if colour = Colour_Type'Last then
            colour := blue;
         else
            colour := Colour_Type'Succ( colour );
         end if;
      end loop;
      nationalities:
      for start_n in dane .. Nationality_Type'Last loop
         nationality := start_n;
         for p in People_Number_Type loop
            people( p ).nationality := nationality;
            if nationality = Nationality_Type'Last then
               nationality := dane;
            else
               nationality := Nationality_Type'Succ( nationality );
            end if;
         end loop;
         drinks:
         for start_d in beer .. Drink_Type'Last loop
            drink := start_d;
            for p in People_Number_Type loop
               people( p ).drink := drink;
               if drink = Drink_Type'Last then
                  drink := beer;
               else
                  drink := Drink_Type'Succ( drink );
               end if;
            end loop;
            fags:
            for start_f in blend .. Fag_Type'Last loop
               fag := start_f;
               for p in People_Number_Type loop
                  people( p ).fag := fag;
                  if fag = Fag_Type'Last then
                     fag := blend;
                  else
                     fag := Fag_Type'Succ( fag );
                  end if;
               end loop;
               pets:
               for start_p in bird .. Pet_Type'Last loop
                  pet := start_p;
                  for p in People_Number_Type loop
                     people( p ).pet := pet;
                     if pet = Pet_Type'Last then
                        pet := bird;
                     else
                        pet := Pet_Type'Succ( pet );
                     end if;
                  end loop;
                  if( Passes )then
                     Put_Line( "passes" );
                     exit;
                  end if;
               end loop pets;
            end loop fags;
         end loop drinks;
      end loop nationalities;
   end loop colours;
   
end  Einstein;
