with Ada.Calendar.Formatting;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Assertions;
with Model.Run_Settings;

with Model.WSC.Formatting;
with Model.WSC.Globals;
with Model.WSC.Output;
with Model.WSC.Household.Transitions;
with Model.WSC.Parameter_System_Declarations;
with Model.WSC.Parameters.IO;
with Model.WSC.Global_Settings;
with Model.WSC.Output.Web_IO;
with Text_Utils;
with Utils;
with GNATColl.Traces;
with WSC_Emailer;
with State_IO;
with Run_IO;
with Dataset_IO;
--
-- FIXME abort somehow
--

package body Model.WSC.Dynamic_Driver.Web_Runner is

   use Ada.Strings.Unbounded;
   use Text_Utils;
   use Ada.Text_IO;
   use Ada.Calendar;
   use Model.WSC.Formatting;
   use Ada.Assertions;
   
   package globals renames Model.WSC.Globals;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.DYNAMIC_DRIVER.WEB_RUNNER" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;


   function Get_New_Run_Id return String is
   begin
      return Censor_String( "run_" & Ada.Calendar.Formatting.Image( Ada.Calendar.Clock ) & "_run" );
   end Get_New_Run_Id;
   
   type Web_Observer is new Model.Run_Settings.Model_Observer with null record;
   overriding procedure Update( observer : access Web_Observer ) is
      run_state  : State_Type;
      session_id : AWS.Session.Id;
      username   : Unbounded_String := TuS( observer.m.Get_Owner );
      run_id     : Natural := observer.m.Get_Id;
      aborting   : Boolean := False;
      wsc_run    : Run;
      y          : Integer;
   begin
      run_state := State_IO.Retrieve_By_PK( username, run_id );
      if( observer.m.Get_Is_In_Error )then
         run_state.phase := complete;
         run_state.health := in_error;
         run_state.error_code := observer.m.Get_Counter( 6 );
         run_state.message := TuS( observer.m.Get_Message & ": stack trace: " &  observer.m.Get_Stack_Trace );
         Log( "ERROR: username |" & TS( username ) & "| run id |" & run_id'Img & "| message |" &
            TS( run_state.message )  & Utils.Get_Stack_Trace );
         Log( "error message " & observer.m.Get_Message );
      else
         run_state.phase := observer.m.Get_Stage;
         run_state.health := observer.m.Get_Health;
      end if;
      run_state.household := observer.m.Get_Counter( 1 );
      y := observer.m.Get_Counter( 2 );
      if( y < Year_Number'First )then
         y := Year_Number'First;
      elsif( y > Year_Number'Last )then
         y := Year_Number'Last;
      end if;
      run_state.year := y;
      run_state.other_counter  := observer.m.Get_Counter( 3 );
      run_state.other_counter2 := observer.m.Get_Counter( 4 );
      run_state.other_counter3 := observer.m.Get_Counter( 5 );
      run_state.other_counter4 := observer.m.Get_Counter( 6 );
         
      State_IO.Save( run_state );
      session_id := AWS.Session.Value( TS( run_state.session_id ));
      aborting := AWS.Session.Get( session_id, globals.SESSION_ABORTING );
      aborting := observer.m.Is_Aborting or aborting;  
      if( aborting )then
         observer.m.Set_Abort;
         Log( "Got ABORT state" );
         AWS.Session.Set( session_id, globals.SESSION_ABORTING, False );
         wsc_run := Run_IO.Retrieve_By_PK( run_id, username );
         if( not observer.m.Get_Is_In_Error )then
            wsc_run.Remove_Directories_For_Run;
            Run_IO.Delete( wsc_run );
         end if;
      elsif( run_state.phase = complete )then
         Run_IO.Clear_Status_Flag_For( username, displayed );
         Log( "Update_Run_State; Phase = complete" );
         wsc_run := Run_IO.Retrieve_By_PK( run_id, username );
         wsc_run.status := displayed;
         Run_IO.Save( wsc_run );   
         AWS.Session.Set( session_id, globals.SESSION_RUN_JUST_ENDED, True );
      end if;
   end Update;

   type Run_Entry_Type is record
      wsc_run : Run;
      session_id   : AWS.Session.Id;
      params       : Parameters_Array;
   end record;
   
   package Job_Queue_Package is new Ada.Containers.Vectors(
      Index_Type => Positive,
      Element_Type => Run_Entry_Type );      
   
   subtype Job_List is Job_Queue_Package.Vector;
   
   protected type Job_Queue_Type is
      procedure Deque( rc : out Run_Entry_Type );
      function Size return Natural;
      procedure Register_Worker( wc : in out Natural );
      procedure Enque( 
            session_id   : AWS.Session.Id; 
            wsc_run : Run;
            params       : Parameters_Array );
   private      
      queue           : Job_List;
      job_count       : Natural := 0;
      worker_count    : Natural := 0;
   end Job_Queue_Type;   
   
   protected body Job_Queue_Type is   
   
      procedure Deque( rc : out Run_Entry_Type ) is
      begin
         rc := ( Null_Run, AWS.Session.No_Session, Get_Null_Parameters );
         if( Size > 0 ) then
            Log( "In Queue handler Dequing " );
            rc := queue.Element( 1 );
            queue.Delete( 1 );
            job_count := job_count + 1;
         end if;
      end Deque;
      
      procedure Register_Worker( wc : in out Natural ) is
      begin
         Log( "Worker count " & Natural'Image( worker_count ));
         worker_count := worker_count + 1;
         wc := worker_count;
      end Register_Worker;
      
      function Size return Natural is 
      begin
         return Natural( queue.Length );
      end Size;
      
      procedure Enque( 
            session_id : AWS.Session.Id; 
            wsc_run    : Run;
            params     : Parameters_Array ) is
         rc :  Run_Entry_Type := ( wsc_run => wsc_run, session_id=>session_id, params => params );
      begin
         Log( "enqueing job with session id |" & AWS.Session.Image( session_id ) & "|" );
         queue.Append( rc );     
      end Enque;
      
   end Job_Queue_Type;
    
   job_queue : Job_Queue_Type;
   
   procedure Submit_Run( 
      session_id   : AWS.Session.Id; 
      wsc_run      : Run;
      params       : Parameters_Array ) is
   begin
      job_queue.Enque( session_id, wsc_run, params  );
   end Submit_Run;
   
   task type Model_Runner_Task_Type is
      -- 
      -- For why this pragma:
      -- See: http://gcc.gnu.org/onlinedocs/gcc-3.3.6/gnat_rm/Storage_005fSize-Clauses.html
      -- and: http://coding.derkeiler.com/Archive/Ada/comp.lang.ada/2005-05/msg00281.html
      -- 
    pragma Storage_Size( Utils.Default_Stack_Size );
     -- entry Start( qno : Positive );
 
   end Model_Runner_Task_Type;
   
  task body Model_Runner_Task_Type is
      use Model.WSC.Output;
      --
      -- FIXME: this is wrong because is presupposes that the user's session is
      -- still there when the job runs. We need need to store everything we need
      -- in the Regime_And_Control record and use it all from there.
      -- Not fixing it now, though.
      -- Also, we have out_dir from the session and   working_directory from the RandC record
      -- which are (almost) the same thing.
      -- 
      rc            : Run_Entry_Type; 
      i             : Natural  := 0; 
      worker_number : Natural  := 0;
  begin
      job_queue.Register_Worker( worker_number );
      loop
         if( job_queue.Size > 0 ) then
            Log( "job allocated to queue " & Natural'Image( worker_number ));
            job_queue.Deque( rc ); 
            Log( "job dequed " & rc.wsc_run.run_id'Img & " " & TS( rc.wsc_run.username ));
            if( rc.wsc_run /= Null_Run )then
               declare
                  state : State_Type;
               begin
                  Log( "starting run " & rc.wsc_run.run_id'Img & " username " & TS( rc.wsc_run.username ));
                  state.phase := run_starting;
                  state.username := rc.wsc_run.username;
                  state.run_id := rc.wsc_run.run_id;
                  state.session_id := TuS( AWS.Session.Image( rc.session_id ));
                  Assert( rc.wsc_run.status = running_or_queued, 
                     " rc.status should be running_or_queued was " & 
                     rc.wsc_run.status'Img );
                  State_IO.Save( state );
                  Log( "dequeing; set session id as |" & TS( state.session_id ));
                  case rc.wsc_run.type_of_run is
                  when simulation =>
                     declare
                        monitor        : aliased Model.Run_Settings.Model_Monitor;
                        web_obs        : Web_Observer( monitor'Access );
                        -- text_obs       : Model.Run_Settings.Model_Observer( monitor'Access );
                        email_result   : WSC_Emailer.Email_Result;
                        default_params : Parameters_Array := 
                           Model.WSC.Parameter_System_Declarations.Get_Default_Model_Parameters( rc.wsc_run );
                     begin
                        monitor.Reset;
                        Log( "run starting for user " & TS( rc.wsc_run.username ) & " run_id " & rc.wsc_run.run_id'Img );
                        Put_Line( "run starting for user " & TS( rc.wsc_run.username ) & " run_id " & rc.wsc_run.run_id'Img );
                        monitor.Set_Id( rc.wsc_run.run_id );
                        monitor.Set_Owner( TS( rc.wsc_run.username ));
                        monitor.Set_Stage( run_starting );
                        monitor.Set_Health( normal );
                        Model.WSC.Dynamic_Driver.Run_Model(
                           rc.wsc_run,
                           default_params,
                           rc.params,
                           state,
                           monitor );
                        state.phase := generating_output;
                        declare
                           main_table : Unbounded_String := Model.WSC.Output.Web_IO.Get_All_Years_Summary( rc.wsc_run );
                        begin
                           Text_Utils.Write_Whole_File( 
                              rc.wsc_run.Qualified_Output_Directory( 1 ) & "main_output.html", 
                              main_table );
                        end;
                        email_result := WSC_Emailer.Send_Run_End_Email( rc.wsc_run ); 
                        Log( "send run end email to user " & TS( rc.wsc_run.username ) & " result " & email_result'Img );
                     end;
                  when data_creation =>
                     declare
                        monitor        : aliased Model.Run_Settings.Model_Monitor;
                        -- text_obs       : Model.Run_Settings.Model_Observer( monitor'Access );
                        web_obs        : Web_Observer( monitor'Access );
                        ds : Dataset;
                        email_result : WSC_Emailer.Email_Result;
                     begin
                        ds.creator := rc.wsc_run.username;
                        ds.name    := Censor_String( rc.wsc_run.title );
                        ds.title   := rc.wsc_run.title;
                        monitor.Set_Id( rc.wsc_run.run_id );
                        monitor.Set_Owner( To_String( rc.wsc_run.username ));
                        monitor.Set_Stage( run_starting );
                        monitor.Set_Health( normal );
                        rc.wsc_run.dataset_name := ds.name;
                        Run_IO.Save( rc.wsc_run );
                        Dataset_IO.Save( ds );
                        Model.WSC.Household.Transitions.Create_Simulation_Data(
                           rc.wsc_run,
                           monitor,
                           no_uprating
                        );
                        state := State_IO.Retrieve_By_PK( rc.wsc_run.username, rc.wsc_run.run_id );
                        if( state.health = normal and state.phase = complete )then
                           Dataset_IO.Save( ds );
                        end if;
                        email_result := WSC_Emailer.Send_Run_End_Email( rc.wsc_run );
                        Log( "send run end email to user " & TS( rc.wsc_run.username ) & " result " & email_result'Img );
                     end;
                  end case;
               end;
               Log( Natural'Image( worker_number ) & " : job completed and queue freed"  );
            end if; -- not a nasty null run
         end if; -- something in the queue to deque
         delay 1.0;
         i := i + 1;
         if( i = 100 ) then
            Log( "The queue# " & Positive'Image( worker_number ) & 
               " is listening " &
               Natural'Image( job_queue.size ) & " are in the queue " );
               i := 1;
          end if;
      end loop; -- forever
   end Model_Runner_Task_Type;
   
   MAX_RUNNING_JOBS : constant := 3; -- FIXME to settings
   type Runner_Array is array( 1 .. MAX_RUNNING_JOBS ) of Model_Runner_Task_Type;
   r : Runner_Array;
   
   function Monitor_To_HTML( wsc_run: Run; state : State_Type ) return Unbounded_String is
      MAX_YEARS       : constant := 31;
      APPROX_HH_COUNT : constant := 1_100.0; 
      
      function Make_Years_Table( year : Year_Number ) return Unbounded_String is
         s : Unbounded_String := TuS("<table width='100%' cellspacing='1'><tr class='yearbox' >" );
         class : Unbounded_String := TuS( "year_done" );
       begin  
         for y in wsc_run.start_year .. wsc_run.end_year loop
            if y = year then
               s := s & "<td width='1px' class='year_active'></td>";       
               class := TuS( "year_todo" );
            else
               s := s & "<td width='1px' class='" & class & "'></td>";
            end if;
         end loop;
         s := s  & "<td>" & Format( year ) & "&nbsp;Done</td></tr></table>";
         return s;
       end Make_Years_Table;

       function Make_Iterations_Table( iteration : Natural ) return Unbounded_String is
         s : Unbounded_String := TuS("<table width='100%'><tr class='yearbox' >" );
         class : Unbounded_String := TuS( "year_done" );
       begin  
         for m in 1 .. wsc_run.num_iterations loop
            if( m = iteration ) then
               s := s & "<td class='year_active'></td>";       
               class := TuS( "year_todo" );
            else
               s := s & "<td class='" & class & "'></td>";
            end if;
         end loop;
         s := s  & "<td>" & Format( iteration ) & "&nbsp;of&nbsp;" & Format( wsc_run.num_iterations ) & "</td></tr></table>";
         return s;
      end Make_Iterations_Table;
    
      function Make_Household_Progress_Table( pct_done : Natural ) return Unbounded_String is
         use Text_Utils;
         
         s : Unbounded_String;
         pct_to_do : Integer;
      begin
         pct_to_do := 100 - pct_done;
         s := s &"<table width='100%' border='0' cellspacing='0'  class='progressBar'>";
         s := s &"<tr>";
         if( pct_done > 0 ) then
            s := s & "<td width='" & Format( pct_done ) & "%' class='households_done'>&nbsp;</td>";
            s := s & "<td width='" & Format( pct_to_do ) & "%' class='households_todo'>&nbsp;</td>";
         else
            s := s & "<td>&nbsp;</td>";
         end if;
         s := s & "</tr>";
         s := s & "</table>";
         return s;
      end Make_Household_Progress_Table;
      
      type Local_Phase_Type is ( unknown, generating_weights, loading_initial_data, calculating_uap, doing_calculations, creating_data );
      local_phase      : Local_Phase_Type := unknown;
      table_str        : Unbounded_String;
      years_table      : Unbounded_String;
      households_table : Unbounded_String;
      iterations_table : Unbounded_String;
      pct_done         : Natural := 0;
      next_iter        : Natural := state.other_counter+1;
      phase_string     : Unbounded_String;
   begin
      Log( "Monitor_To_HTML entered; run_id " & wsc_run.run_id'Img & " username " & TS( wsc_run.username ) &
           "Phase " & state.phase'Img );
      if( state.health = in_error ) or ( state.error_code /= 0 )then
         table_str := table_str & "<div class='error'>Error encountered<pre>" & state.message & "</pre>" &
              "<p>state.health : <b>" & 
              state.health'Img & "</b></p><p>state.error_code: <b>" & state.error_code'Img & "</b></p></div>" & LINE_BREAK;
      else
         table_str := table_str &"<table width='98%' class='statusTable' >" & LINE_BREAK;
         table_str := table_str & "      <tr>" & LINE_BREAK;
         table_str := table_str & "<td>Run ID:&nbsp;" & wsc_run.run_id'Img & "</td>";
         case state.phase is
            when not_started | queued => 
                  table_str := table_str & "  <td align='middle' class='phase'>Job in queue and waiting to start</td>" & LINE_BREAK;
            when run_starting => 
                  table_str := table_str & "   <td align='middle' class='phase'>Run Starting Up</td>" & LINE_BREAK;
            when pre_calculations | running =>
               if( wsc_run.type_of_run = data_creation )then
                  local_phase := creating_data;
                  phase_string := Tus( "Generating New Dataset" );
               elsif( state.phase = running )then
                  local_phase := doing_calculations;
                  phase_string := TuS( "Doing Calculations" );
               elsif state.phase = pre_calculations then
                  if state.other_counter3 = 1 then
                     local_phase := generating_weights;
                     phase_string := TuS( "Generating Weights" );
                  elsif state.other_counter3 = 3 then
                     local_phase := loading_initial_data;
                     phase_string := TuS( "Loading historic data into results" );
                  elsif state.other_counter3 = 4 then
                     local_phase := calculating_uap;
                     phase_string := TuS( "Calculating UAP Thresholds for system " & state.other_counter2'Img );
                  end if;
               end if;
               if( local_phase /= unknown )then    
                  if( state.household > 0 )then
                    pct_done := Natural( 100.0 * Amount( state.household ) / APPROX_HH_COUNT );
                    households_table := Make_Household_Progress_Table( pct_done );
                  end if;
                  table_str := table_str & "<td width='20%'>Phase:&nbsp;<span class='phase'>" & phase_string & "</span></td>";
                  if( wsc_run.num_iterations > 1 )then
                    iterations_table := Make_Iterations_Table( state.other_counter );
                    table_str := table_str & "<td width='20%'>Iteration:&nbsp;" & iterations_table & "</td>";
                  end if;
                  if wsc_run.type_of_run = simulation and local_phase /= loading_initial_data then
                    years_table := Make_Years_Table( state.year );
                    table_str := table_str & "<td width='20%'>Year:&nbsp;" & years_table & "</td>";
                  end if;
                  table_str := table_str & "<td width='40%'>Household:&nbsp;" & households_table & "</td>";
                  table_str := table_str & "<td><input id='Abort_Submit_Button' type='submit' name='abort' value='Abort' /></td>";
               end if;
            when generating_output => 
               table_str := table_str & "<td align='middle' class='phase'>Generating Output</td>";
            when complete => 
               table_str := table_str & "<td align='middle' class='phase'>Job Complete</td>";
         end case;
         
         table_str := table_str & LINE_BREAK & "  </tr>" & LINE_BREAK;
         table_str := table_str & "</table>" & LINE_BREAK;
      end if;
      return table_str;
   end  Monitor_To_HTML;  
   
   function Get_State_Of_Run_As_HTML(
      wsc_run   : Run;
      run_state : State_Type ) return Unbounded_String is
      s : Unbounded_String;
      root_str : constant String := Model.WSC.Global_Settings.WSC_Web_Root;
   begin
      s := Monitor_To_HTML( wsc_run, run_state );
      if( run_state.health = in_error ) or ( run_state.health = aborted ) then
         s := s & "<script>" & LINE_BREAK; 
         s := s & "      updater.stop();"  & LINE_BREAK; 
         s := s & "      $( 'run_submit_button' ).disabled=false;" & LINE_BREAK;
         s := s & "</script>" & LINE_BREAK;
      elsif( run_state.phase in Running_Phase )then
         null;
         -- s := s & "<script>" & LINE_BREAK; 
         -- send back javascript to disable the output link while running.
         -- see (e.g.): http://stackoverflow.com/questions/893208/changing-the-onclick-event-delegate-of-an-html-button
         -- s := s & "      $( 'output_page_link' ).disabled=true;" & LINE_BREAK;
         -- s := s & "      $( 'output_page_link' ).observe('click', function( event ){ event.stop() });" & LINE_BREAK;
         -- s := s & "</script>" & LINE_BREAK;
      elsif( run_state.phase = complete )then
         Log( "complete; sending switch off code " );
      end if;
      Log( "Monitor_To_HTML; made table as |" & TS( s ) & "|" );
      return s;
   end Get_State_Of_Run_As_HTML;   
   
end Model.WSC.Dynamic_Driver.Web_Runner;
