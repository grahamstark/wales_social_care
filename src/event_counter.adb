with Ada.Strings.Unbounded;

with Format_Utils;
with Text_Utils;
with Model.WSC.Household;

package body Event_Counter is
   
   procedure Multiply( log : in out Recorder; m : Amount ) is
   use ONS_Definitions;
   use Model.WSC.Household;
   begin
      events:
      for event in Event_Type loop
         ages:
         for age in Age_Band loop
            waves:
            for wave in Simulation_Waves loop
               for sex in Gender_Type loop
                  log.weighted( event, wave, sex, age ) := log.weighted( event, wave, sex, age ) * m;                
               end loop;
            end loop waves;
         end loop ages;
      end loop  events;
   end Multiply;
   
   function To_String( 
      ev : Event_Array; 
      start_age : ONS_Definitions.Age_Band := ONS_Definitions.Age_Band'First ) return String is
   use Ada.Strings.Unbounded;
   use Text_Utils;
   use ONS_Definitions;
   use Model.WSC.Household;
   
      package UK_Format_Utils is new Format_Utils( Counter_Type => Counter_Type, Float_Type => Rate );
      use UK_Format_Utils;   
      s : Unbounded_String;
      pop : Amount;
   begin
      for wave in Simulation_Waves loop
         s := s & ",," & Year_Range'Image( Year_From_Wave( wave )) & " (" & Simulation_Waves'Image( wave ) & "),";
      end loop;
      s := s & LINE_BREAK;
      s := s & "Populations:," ;
      for wave in Simulation_Waves loop
         pop :=  Population_Count( ev, wave, start_age); 
         s := s & ",," & Format( pop ) & ",";
      end loop;
      s := s & LINE_BREAK;
      s := s & "EVENT,AGE," & LINE_BREAK;
      s := s & ",,";
      for wave in Simulation_Waves loop
         s := s & "MALE,FEMALE,,";
      end loop;
      s := s & LINE_BREAK;
      events:
      for event in Event_Type loop
         ages:
         for age in start_age .. Age_Band'Last loop
            s := s & Prettify_Image( Event_Type'Image( event )) & "," & Prettify_Image( Age_Band'Image( age )) & ",";
            waves:
            for wave in Simulation_Waves loop
               for sex in Gender_Type loop
                  s := s & Format( ev( event, wave, sex, age )) & ",";                
               end loop;
               s := s & ",";
            end loop waves;
            s := s & LINE_BREAK;
         end loop ages;
      end loop  events;
      return TS( s );      
   end To_String;
   
   function Get_Raw( log : Recorder ) return Event_Array is
   begin
      return log.raw;
   end Get_Raw;
   
   function Get_Weighted( log : Recorder ) return Event_Array is
   begin
      return log.weighted;
   end Get_Weighted;
   
   procedure Reset( rec : in out Recorder ) is
   begin
      rec.weighted := ( others => ( others => ( others => ( others => 0.0 ))));   
      rec.raw := ( others => ( others => ( others => ( others => 0.0 ))));   
   end Reset;
      
   
   
   procedure Add(
      log    : in out Recorder;
      event  : Event_Type;
      wave   : Waves;
      hid    : Sernum_Value;
      pid    : Sernum_Value;
      age    : Age_Range;
      sex    : Gender_Type;
      weight : Amount ) is
   use ONS_Definitions;   
      band : Age_Band := Get_Age_Band( age );
   begin
      log.raw( event, wave, sex, band ) := log.raw( event, wave, sex, band ) + 1.0;
      log.weighted( event, wave, sex, band ) := log.weighted( event, wave, sex, band ) + weight;
   end Add;
   
   function To_Per_1000_People( ev : Event_Array ) return Event_Array is
   use ONS_Definitions;   
      p : Event_Array := ev;
   begin
      for wave in Waves loop
         for event in Event_Type loop
            for age in Age_Band loop
               for sex in Gender_Type loop
                  if( Is_Population( event ))then
                     if( ev( Population_Field, wave, sex, age ) /= 0.0 )then
                        p( event, wave, sex, age ) := 1_000.0 * p( event, wave, sex, age ) / p( Population_Field, wave, sex, age );
                     end if;
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
      return p;
   end To_Per_1000_People;
   
   function Population_Count( 
      ev : Event_Array; 
      wave : Waves;
      start_age : ONS_Definitions.Age_Band := ONS_Definitions.Age_Band'First ) return Amount is
   use ONS_Definitions;
      count : Amount := 0.0;
   begin
      for age in start_age .. Age_Band'Last loop
         for sex in Gender_Type loop
            count := count + ev( Population_Field, wave, sex, age );
         end loop;
      end loop;
      return count;
   end Population_Count;      
   
end Event_Counter;
