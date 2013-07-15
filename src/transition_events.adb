package body Transition_Events is
   
   function Is_Population_Event( t : Transitions_Event_Type ) return Boolean is
   begin
      return t in total_population .. non_residential_population;
   end Is_Population_Event;

end Transition_Events;
