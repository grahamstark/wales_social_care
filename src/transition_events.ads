with Event_Counter;

package Transition_Events is
   
type Transitions_Event_Type is ( 
      total_population, -- i,e population counter
      residential_population,
      non_residential_population,
      
      new_65_plus,
      death,
      non_residential_death,
      residential_death,
      
      separation, 
      to_care_home, 

      care_received_from_household_member_starts, 
      care_received_from_non_householder_starts, 

      care_received_from_household_member, 
      care_received_from_non_householder, 
      
      health_worsening, 
      health_improving, 
      health_na,
      health_excellent,
      health_good,
      health_fair,
      health_poor,
      health_very_poor,
      
      adl_manage_stairs_help_worsening, 
      adl_get_around_house_help_worsening, 
      adl_get_in_or_out_of_bed_help_worsening, 
      adl_cut_toenails_help_worsening, 
      adl_bathing_or_showering_help_worsening, 
      adl_walk_down_road_help_worsening, 
      
      adl_manage_stairs_difficulty_worsening, 
      adl_get_around_house_difficulty_worsening, 
      adl_get_in_or_out_of_bed_difficulty_worsening, 
      adl_cut_toenails_difficulty_worsening, 
      adl_bathing_or_showering_difficulty_worsening, 
      adl_walk_down_road_difficulty_worsening, 
      
      adl_manage_stairs_difficulty_improving,
      adl_get_around_house_difficulty_improving,
      adl_get_in_or_out_of_bed_difficulty_improving,
      adl_cut_toenails_difficulty_improving,
      adl_bathing_or_showering_difficulty_improving,
      adl_walk_down_road_difficulty_improving,
      
      adl_manage_stairs_help_improving,
      adl_get_around_house_help_improving,
      adl_get_in_or_out_of_bed_help_improving,
      adl_cut_toenails_help_improving,
      adl_bathing_or_showering_help_improving,
      adl_walk_down_road_help_improving,
      
      retire,
      household_ends, 
      household_starts,
      new_benefit_unit );
      
   function Is_Population_Event( t : Transitions_Event_Type ) return Boolean;
   
   package Transition_Events_Counter is new Event_Counter( 
      Event_Type => Transitions_Event_Type,
      Population_Field => total_population,
      Is_Population => Is_Population_Event );
      
end Transition_Events;
