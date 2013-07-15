with Ada.Assertions;
with Ada.Calendar.Formatting;
with Ada.Calendar;
with Ada.Directories;
with Ada.Text_IO;

with DB_Commons;
with Line_Extractor;
with Model.WSC.Global_Settings;
with Run_IO;
with State_IO;
with Text_Utils;
with WSC_DB_Data;
with Utils;
with GNATColl.Traces;

package body Model.WSC.Run_Declarations is

   use Text_Utils;
   use WSC_DB_Data;
   use Ada.Assertions;
   package d renames DB_Commons; 
   
      
   log_trace : constant GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "Model.WSC.Run_Declarations" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   
   function Yes_No( b : Boolean ) return String is
   begin
      if( b ) then return "yes"; else return "no"; end if;
   end Yes_No;
   
   function Now_As_String return String is
   begin
      return Ada.Calendar.Formatting.Image(
         Date => Ada.Calendar.Clock,
         Include_Time_Fraction => True
       );
   end Now_As_String;
   
   function Make_Title( wsc_run : Run; as_html : Boolean := True ) return Unbounded_String is
      s : Unbounded_String;
   begin
      if( as_html )then
         return s & 
            "Title:<span class='value'>" & wsc_run.Title & "</span>&nbsp;" &
            "Run&nbsp;Id:&nbsp;<span class='value'>" & Positive'Image( wsc_run.run_id ) & "</span>&nbsp;" &
            "Username:&nbsp;<span class='value'>"& wsc_run.username & "</span>";
      else
         return s & 
            wsc_run.Title & 
            " : " & Positive'Image( wsc_run.run_id ) & 
            " : " & wsc_run.username;
      end if;
   end Make_Title;
   
   procedure Remove_Directories_For_Run( r : Run ) is
      target : constant String :=  r.Qualified_Users_Directory & Format( r.run_id );
   begin
      --
      -- couple of simple hacks to ensure (??) we don't wipe a disk somehow
      --
      Assert( target'Length > 25, " Remove_Directories_For_Run: suspiciously short target " & target );
      Assert( r.run_id > 0, "Remove_Directories_For_Run: non-positive run id " & target );
      Ada.Directories.Delete_Tree( target );
   end Remove_Directories_For_Run;

   
   function Qualified_Run_Directory( this : in Run; iteration : Natural ) return String is
      s : Unbounded_String := TuS( this.Qualified_Users_Directory ) & Format( this.run_id ) & Model.WSC.Global_Settings.dir_separator;
   begin
      if( iteration > 0 )then
         s := s & Format( iteration ) & Model.WSC.Global_Settings.dir_separator;
      end if;
      return TS( s );
   end Qualified_Run_Directory;

   function Qualified_Output_Directory( this : in Run; iteration : Positive ) return String is 
   begin 
      return this.Qualified_Run_Directory( iteration ) & "output" & Model.WSC.Global_Settings.dir_separator; 
   end Qualified_Output_Directory;
    
   function Qualified_Users_Directory( this : in Run ) return String is 
   begin 
      return Model.WSC.Global_Settings.Working_Root & Model.WSC.Global_Settings.dir_separator & TS( this.username ) & Model.WSC.Global_Settings.dir_separator; 
   end Qualified_Users_Directory;

   function To_String( wsc_run : Run ) return String is
      use UK_Key_Value_IO;
      use Ada.Text_IO;
      use Text_Utils;
      s : Unbounded_String;
    begin
      s := s & "wsc_run.title=" & TS( wsc_run.title ) & LINE_BREAK;
      s := s & "wsc_run.working_root=" & TS( wsc_run.working_root ) & LINE_BREAK;
      s := s & "wsc_run.comparison_run_id=" & wsc_run.comparison_run_id'Img & LINE_BREAK;      
      s := s & "wsc_run.use_random_threshold=" & wsc_run.use_random_threshold'Img & LINE_BREAK;
      s := s & "wsc_run.assumed_interest_rate_pct=" & wsc_run.interest_rate_pct'Img & LINE_BREAK;
      s := s & "wsc_run.start_year=" & wsc_run.start_year'Img & LINE_BREAK;
      s := s & "wsc_run.end_year=" & wsc_run.end_year'Img & LINE_BREAK;
      s := s & "wsc_run.real_terms=" & wsc_run.real_terms'Img & LINE_BREAK;
      s := s & "wsc_run.weighting_function" & Distance_Function_Type'Image( wsc_run.weighting_function ) & LINE_BREAK;
      s := s & "wsc_run.weighting_lower_bound=" & wsc_run.weighting_lower_bound'Img & LINE_BREAK;
      s := s & "wsc_run.weighting_upper_bound=" & wsc_run.weighting_upper_bound'Img & LINE_BREAK;
      for a in Uprate_Targets loop
         declare
            key : String := Line_Extractor.Make_Key( "wsc_run.uprate", Censor_String( Uprate_Targets'Image( a )), "" );
         begin
            s := s &  key & ".percent_change" & wsc_run.uprate_assumptions( a ).percent_change'Img & LINE_BREAK;
            s := s &  key & ".use_obr" & wsc_run.uprate_assumptions( a ).use_obr'Img  & LINE_BREAK;
            s := s &  key & ".element" & Forecast_Element'Image( wsc_run.uprate_assumptions( a ).element ) & LINE_BREAK;
         end;
      end loop;
      for p in Probit_Threshold_Type loop
         declare
            key : String := Line_Extractor.Make_Key( "wsc_run.probit_thresholds", Censor_String( Probit_Threshold_Type'Image( p )), "value" );
         begin
            s := s &  key & wsc_run.probit_thresholds( p )'Img & LINE_BREAK;
         end;
      end loop;
      s := s & "wsc_run.dataset_name" & TS( wsc_run.dataset_name ) & LINE_BREAK;
      s := s &  "wsc_run.default_run_dir_id" & wsc_run.default_run_dir_id'Img & LINE_BREAK;
      return To_String( s );
    end To_String;
   
   function Create_Directories_For_Run( r : Run ) return Unbounded_String is         
   use Ada.Directories;
       sep : String := Model.WSC.Global_Settings.dir_separator;
   begin
      --
      -- we only need 1 params directory
      -- 
      Utils.Make_Directory_Path( r.Qualified_Run_Directory( 0 ) & sep & "params" );         
      for i in 1 .. r.num_iterations loop
         --
         -- but data dirs for each iteration
         -- 
       declare
          run_dir : String := r.Qualified_Run_Directory( i );
       begin
         Utils.Make_Directory_Path( run_dir & sep & "log" );
         Utils.Make_Directory_Path( run_dir & sep & "output" );
         Utils.Make_Directory_Path( run_dir & sep & "data" );
         Utils.Make_Directory_Path( run_dir & sep & "data/tmp" );
      end;
      end loop;
      return TuS( r.Qualified_Run_Directory( 0 ));
   end Create_Directories_For_Run;
 
   procedure Update_From_Text_Buffer( wsc_run : in out Run; buff : Text_Buffer ) is
      use UK_Key_Value_IO;
      use Ada.Text_IO;
   begin
      wsc_run.title := Read( buff, "wsc_run.title" );
      -- wsc_run.working_root := Read( buff, "wsc_run.working_root" );
      -- wsc_run.comparison_run_id := Read( buff, "wsc_run.comparison_run_id" );      
      wsc_run.use_random_threshold := Read( buff, "wsc_run.use_random_threshold" );
      wsc_run.interest_rate_pct := Read( buff, "wsc_run.assumed_interest_rate_pct" );
      wsc_run.start_year := Read( buff, "wsc_run.start_year" );
      wsc_run.end_year := Read( buff, "wsc_run.end_year" );
      wsc_run.num_iterations := Read( buff, "wsc_run.num_iterations" );
      wsc_run.dataset_name := Read( buff, "wsc_run.dataset_name" );
      wsc_run.do_reweighting := Read( buff, "wsc_run.do_reweighting" );
      -- wsc_run.real_terms := Read( buff, "wsc_run.real_terms" );
      wsc_run.weighting_function := Distance_Function_Type'Value( Read( buff, "wsc_run.weighting_function" ));
      wsc_run.weighting_lower_bound := Read( buff, "wsc_run.weighting_lower_bound" );
      wsc_run.weighting_upper_bound := Read( buff, "wsc_run.weighting_upper_bound" );
      
      for a in Uprate_Targets'First .. upr_care_costs_1 loop
         declare
            key : String := Line_Extractor.Make_Key( "wsc_run.uprate", Censor_String( Uprate_Targets'Image( a )), "" );
            -- key : String := "wsc_run." & Censor_String( Uprate_Targets'Image( a )( 5 .. Uprate_Targets'Image( a )'Length ));
         begin
            Put_Line( "uprate targets key |" & key & "| " );
            wsc_run.uprate_assumptions( a ).percent_change := Read( buff, key & ".percent_change" );
            wsc_run.uprate_assumptions( a ).use_obr := Read( buff, key & ".use_obr" );
            declare
               fes : String := Read( buff, key & ".element" );
            begin
               Put_Line( "got .element s as |" & fes & "| " );
               if( wsc_run.uprate_assumptions( a ).use_obr ) and ( fes /= "uprate_none" )then
                  wsc_run.uprate_assumptions( a ).element := Forecast_Element'Value( fes );
               else
                  wsc_run.uprate_assumptions( a ).element := Forecast_Element'First;
               end if;
            end;
         end;
      end loop;
      for p in Probit_Threshold_Type loop
         declare
            key : String := Line_Extractor.Make_Key( "wsc_run.probit_thresholds", Censor_String( Probit_Threshold_Type'Image( p )), "value" );
         begin
            Put_Line( "probit thresholds; key |" & key & "| " );
            wsc_run.probit_thresholds( p ) := Read( buff, key );
         end;
      end loop;
      -- wsc_run.dataset_dir_id := Read( buff, "wsc_run.dataset_dir_id" );
      wsc_run.comparison_run_id := Read( buff, "wsc_run.comparison_run_id" );
   end Update_From_Text_Buffer;
   
   function To_Text_Buffer( wsc_run : Run ) return Text_Buffer is
      use UK_Key_Value_IO;
      use Ada.Text_IO;
      tvb : Text_Buffer;  
    begin
      Write( tvb, "wsc_run.title", wsc_run.title );
      Write( tvb, "wsc_run.working_root", wsc_run.working_root );
      Write( tvb, "wsc_run.comparison_run_id", wsc_run.comparison_run_id );      
      Write( tvb, "wsc_run.use_random_threshold", wsc_run.use_random_threshold );
      Write( tvb, "wsc_run.assumed_interest_rate_pct", wsc_run.interest_rate_pct );
      Write( tvb, "wsc_run.start_year", wsc_run.start_year );
      Write( tvb, "wsc_run.end_year", wsc_run.end_year );
      Write( tvb, "wsc_run.num_iterations", wsc_run.num_iterations ); 
      Write( tvb, "wsc_run.real_terms", wsc_run.real_terms );
      Write( tvb, "wsc_run.weighting_function", Distance_Function_Type'Image( wsc_run.weighting_function ));
      Write( tvb, "wsc_run.weighting_lower_bound", wsc_run.weighting_lower_bound );
      Write( tvb, "wsc_run.weighting_upper_bound", wsc_run.weighting_upper_bound );
      Write( tvb, "wsc_run.do_reweighting", wsc_run.do_reweighting );
      for a in Uprate_Targets loop
         declare
            key : String := Line_Extractor.Make_Key( "wsc_run.uprate", Censor_String( Uprate_Targets'Image( a )), "" );
         begin
            Put_Line( "uprate targets; key |" & key & "| " );
            Write( tvb, key & ".percent_change", wsc_run.uprate_assumptions( a ).percent_change );
            Write( tvb, key & ".use_obr", wsc_run.uprate_assumptions( a ).use_obr );
            if( wsc_run.uprate_assumptions( a ).use_obr )then
               Write( tvb, key & ".element", Forecast_Element'Image( wsc_run.uprate_assumptions( a ).element ));
            else
               Write( tvb, key & ".element", "uprate_none" );
            end if;
         end;
      end loop;
      for p in Probit_Threshold_Type loop
         declare
            key : String := Line_Extractor.Make_Key( "wsc_run.probit_thresholds", Censor_String( Probit_Threshold_Type'Image( p )), "value" );
         begin
            Put_Line( "probit thresholds key |" & key & "| " );
            Write( tvb, key, wsc_run.probit_thresholds( p ));
         end;
      end loop;
      Write( tvb, "wsc_run.dataset_name", wsc_run.dataset_name );
      Write( tvb, "wsc_run.default_run_dir_id", wsc_run.default_run_dir_id );
      return tvb;
    end To_Text_Buffer;

   
   procedure Write_Settings( filename : String; wsc_run : Run ) is
      use UK_Key_Value_IO;
      use Ada.Text_IO;
      file : Ada.Text_IO.File_Type;  
    begin
      Create( file, Out_File, filename );
      Write( file, "wsc_run.title", wsc_run.title );
      Write( file, "wsc_run.working_root", wsc_run.working_root );
      Write( file, "wsc_run.comparison_run_id", wsc_run.comparison_run_id );      
      Write( file, "wsc_run.use_random_threshold", wsc_run.use_random_threshold );
      Write( file, "wsc_run.assumed_interest_rate_pct", wsc_run.interest_rate_pct );
      Write( file, "wsc_run.start_year", wsc_run.start_year );
      Write( file, "wsc_run.end_year", wsc_run.end_year );
      Write( file, "wsc_run.num_iterations", wsc_run.num_iterations ); 
      Write( file, "wsc_run.real_terms", wsc_run.real_terms );
      Write( file, "wsc_run.weighting_function", Distance_Function_Type'Image( wsc_run.weighting_function ));
      Write( file, "wsc_run.weighting_lower_bound", wsc_run.weighting_lower_bound );
      Write( file, "wsc_run.weighting_upper_bound", wsc_run.weighting_upper_bound );
      Write( file, "wsc_run.do_reweighting", wsc_run.do_reweighting );
      for a in Uprate_Targets loop
         declare
            -- key : String := "wsc_run." & Censor_String( Uprate_Targets'Image( a )( 5 .. Uprate_Targets'Image( a )'Length ));
            key : String := Line_Extractor.Make_Key( "wsc_run.uprate", Censor_String( Uprate_Targets'Image( a )), "" );
         begin
            Put_Line( "uprate targets; key |" & key & "| " );
            Write( file, key & ".percent_change", wsc_run.uprate_assumptions( a ).percent_change );
            Write( file, key & ".use_obr", wsc_run.uprate_assumptions( a ).use_obr );
            Write( file, key & ".element", Forecast_Element'Image(wsc_run.uprate_assumptions( a ).element ));
         end;
      end loop;
      for p in Probit_Threshold_Type loop
         declare
            key : String := Line_Extractor.Make_Key( "wsc_run.probit_thresholds", Censor_String( Probit_Threshold_Type'Image( p )), "value" );
         begin
            Put_Line( "probit thresholds key |" & key & "| " );
            Write( file, key, wsc_run.probit_thresholds( p ));
         end;
      end loop;
      Write( file, "wsc_run.dataset_name", wsc_run.dataset_name );
      Write( file, "wsc_run.default_run_dir_id", wsc_run.default_run_dir_id );
      Close( file );
    end Write_Settings;
    
   function Read_Settings( filename : String ) return Run is
      use UK_Key_Value_IO;
      use Ada.Text_IO;
      file : Ada.Text_IO.File_Type;  
      wsc_run : Run;
   begin
      Open( file, In_File, filename );
      wsc_run.title := Read( file, "wsc_run.title" );
      wsc_run.working_root := Read( file, "wsc_run.working_root" );
      wsc_run.comparison_run_id := Read( file, "wsc_run.comparison_run_id" );      
      wsc_run.use_random_threshold := Read( file, "wsc_run.use_random_threshold" );
      wsc_run.interest_rate_pct := Read( file, "wsc_run.assumed_interest_rate_pct" );
      wsc_run.start_year := Read( file, "wsc_run.start_year" );
      wsc_run.end_year := Read( file, "wsc_run.end_year" );
      wsc_run.num_iterations := Read( file, "wsc_run.num_iterations" );
      
      wsc_run.real_terms := Read( file, "wsc_run.real_terms" );
      wsc_run.weighting_function := Distance_Function_Type'Value( Read( file, "wsc_run.weighting_function" ));
      wsc_run.weighting_lower_bound := Read( file, "wsc_run.weighting_lower_bound" );
      wsc_run.weighting_upper_bound := Read( file, "wsc_run.weighting_upper_bound" );
      wsc_run.do_reweighting := Read( file, "wsc_run.do_reweighting" );
      
      for a in Uprate_Targets loop
         declare
            key : String := Line_Extractor.Make_Key( "wsc_run.uprate", Censor_String( Uprate_Targets'Image( a )), "" );
            -- key : String := "wsc_run." & Censor_String( Uprate_Targets'Image( a )( 5 .. Uprate_Targets'Image( a )'Length ));
         begin
            Put_Line( "uprate targets key |" & key & "| " );
            wsc_run.uprate_assumptions( a ).percent_change := Read( file, key & ".percent_change" );
            wsc_run.uprate_assumptions( a ).use_obr := Read( file, key & ".use_obr" );
            wsc_run.uprate_assumptions( a ).element := Forecast_Element'Value( Read( file, key & ".element" ));
         end;
      end loop;
      for p in Probit_Threshold_Type loop
         declare
            key : String := Line_Extractor.Make_Key( "wsc_run.probit_thresholds", Censor_String( Probit_Threshold_Type'Image( p )), "value" );
         begin
            Put_Line( "probit thresholds; key |" & key & "| " );
            wsc_run.probit_thresholds( p ) := Read( file, key );
         end;
      end loop;
      wsc_run.dataset_name := Read( file, "wsc_run.dataset_name" );
      wsc_run.default_run_dir_id := Read( file, "wsc_run.default_run_dir_id" );
      Close( file );
      return wsc_run;
   end Read_Settings;
   

end Model.WSC.Run_Declarations;
