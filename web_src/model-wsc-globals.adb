with Ada.Text_IO;
with Ada.Command_Line;
with Model.WSC.Global_Settings;
with GNATColl.Traces;
with Model.WSC.Formatting;
with Model.WSC.Users.IO;
with Model.WSC.Parameters.IO;
with Connection_Pool;
with Environment;  -- DB;
with Run_IO;
with Model.WSC.Users;
with Model.WSC.Parameters.DAO;
with Text_Utils;
with User_Type_IO;
with State_IO;
with GNATColl.Traces;
with Keyed_Text_Buffer;
with Model.WSC.Uprate;


package body Model.WSC.Globals is
   use Model.WSC.Parameters;
   use runsett;
   use Ada.Text_IO;
   use Text_Utils;
   use type runsett.Run_Status_Type;

   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.GLOBALS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;
   
   procedure Reset( 
      wsc_run              : runsett.Run;
      target_subsystem     : wsc_params.WSC_Parameter_Editing_System.Parameter_System_Rec;
      path_str             : Unbounded_String;
      default_system       : wsc_params.WSC_Parameter_Editing_System.Parameter_System_Rec;
      param_editing_buffer : in out wsc_params.WSC_Editing_Buffer;
      action               : Reset_Actions; 
      year                 : Year_Number ) is
      start_year, end_year : Year_Number;
      comparison_run        : runsett.Run                   := wsc_params.Get_Comparison_Run( wsc_run );
      default_parameter_map : Keyed_Text_Buffer.Text_Buffer := Run_IO.Map_Associated_Params_To_Text_Buffer( comparison_run );
   begin
      Log( "Reset action: " & Reset_Actions'Image( action ) & " year: " & Year_Number'Image( year ));
      case action is
         when this_year_these_items_only =>
            start_year := year;
            end_year := year;
            param_editing_buffer.Set_Current_Year( year );
            wsc_params.WSC_Parameter_Editing_System_IO.Load( 
                param_editing_buffer,
                default_system,
                target_subsystem,
                default_parameter_map,
                default_parameter_map,
                param_editing_buffer.Year_Prefix_Str & path_str,
                year );
         when this_year_all_items  =>
            start_year := year;
            end_year := year;
            param_editing_buffer.Set_Current_Year( year );
            wsc_params.WSC_Parameter_Editing_System_IO.Load( 
                param_editing_buffer,
                default_system,
                default_system,
                default_parameter_map,
                default_parameter_map,
                param_editing_buffer.Year_Prefix_Str,
                year );
         when all_years_these_items_only =>
            start_year := wsc_run.start_year;
            end_year := wsc_run.end_year;
            for y in wsc_run.start_year .. wsc_run.end_year loop
               param_editing_buffer.Set_Current_Year( y );
               wsc_params.WSC_Parameter_Editing_System_IO.Load( 
                   param_editing_buffer,
                   default_system,
                   target_subsystem,
                   default_parameter_map,
                   default_parameter_map,
                   param_editing_buffer.Year_Prefix_Str & path_str,
                   y
                    );
            end loop;
         when all_years_all_items        =>
            start_year := wsc_run.start_year;
            end_year := wsc_run.end_year;
            for y in wsc_run.start_year .. wsc_run.end_year loop
               param_editing_buffer.Set_Current_Year( y );
               wsc_params.WSC_Parameter_Editing_System_IO.Load( 
                   param_editing_buffer,
                   default_system,
                   default_system,
                   default_parameter_map,
                   default_parameter_map,
                   param_editing_buffer.Year_Prefix_Str,
                   y
                   );
            end loop;
      end case;
      Map_And_Set_Parameters( wsc_run, param_editing_buffer, year, get_all, start_year, end_year );
      param_editing_buffer.Set_Current_Year( year );
   end Reset;
   
   function Get_Uprate_Amount( v : Amount; year : Year_Number; index : Which_Uprate_Index ) return Amount is
      a : Amount;
      f : Forecast_Element;
   begin
      case index is
         when uprate_none => null;
         when uprate_rpi =>  f := rpi;
         when uprate_rpix => f := rpix;
         when uprate_earnings => f := obr_earnings;
         when uprate_cpi => f := cpi;
         when uprate_gdp => f :=  gdp_at_market_prices;
      end case;
      if( index /= uprate_none )then
         a := Model.WSC.Uprate.Get_Ratio_Between( f, year-1, 3, year, 3 );
      else
         a := 1.0;
      end if;
      a := a + v/100.0;
      return a;
   end Get_Uprate_Amount;
      
   procedure Uprate( 
      wsc_run              : runsett.Run;
      param_editing_buffer : in out wsc_params.WSC_Editing_Buffer;
      path_str             : Unbounded_String; 
      action               : Uprate_Actions; 
      index                : Which_Uprate_Index; 
      year                 : Year_Number; 
      uprate_amount        : Rate ) is
      upr : Amount;
      start_year, end_year : Year_Number;
   begin
      Log( "Uprate : action |" & action'Img & "| index |" & index'Img & "| m |" & m'Img & "| path_str |" & TS( path_str ) & "|" );
      case action is
      when this_year_these_items_only =>
         start_year := year;
         end_year := year;
         upr := Get_Uprate_Amount( uprate_amount, year, index );
         param_editing_buffer.Operate( path_str, upr, 0, year, year );
      when this_year_all_items =>
         start_year := year;
         end_year := year;
         upr := Get_Uprate_Amount( uprate_amount, year, index );
         param_editing_buffer.Operate( Null_Unbounded_String, upr, 0, year, year );
      when this_and_subsequent_years_these_items_only =>
         start_year := year;
         end_year := wsc_run.end_year;
         upr := 1.0;            
         for y in start_year .. end_year loop
            upr := upr * Get_Uprate_Amount( uprate_amount, y, index );
            param_editing_buffer.Operate( path_str, upr, 0, y, y );
         end loop;
      when this_and_subsequent_years_all_items =>
         start_year := year;
         end_year := wsc_run.end_year;
         upr := 1.0;            
         for y in start_year .. end_year loop
            upr := upr * Get_Uprate_Amount( uprate_amount, y, index );
            param_editing_buffer.Operate( Null_Unbounded_String, upr, 0, y, y );
         end loop;
      end case;
      Map_And_Set_Parameters( wsc_run, param_editing_buffer, year, get_changed_values, start_year, end_year );
      param_editing_buffer.Set_Current_Year( year );
   end Uprate;
   
   procedure Map_And_Set_Parameters( 
      wsc_run              : runsett.Run;
      param_editing_buffer : in out wsc_params.WSC_Editing_Buffer;
      current_year         : Year_Number; 
      which                : Buffer_Retrieval_Type; 
      start_year           : Year_Number; 
      end_year             : Year_Number ) is
   use Model.WSC.Parameters;
      kvb : Keyed_Text_Buffer.Text_Buffer;
      error_count : constant Natural := param_editing_buffer.Get_Num_Errors;
   begin
      Log( "Map_And_Set_Parameters entered; num_errors = " & error_count'Img & 
           " which = " & which'Img & 
           " start_year " & start_year'Img & 
           " end_year " & end_year'Img );
      if( error_count = 0 )then
         kvb :=  param_editing_buffer.To_Text_Buffer( which, start_year, end_year );
         Run_IO.Bulk_Save_Parameters( wsc_run, 1, kvb );
      end if; 
      param_editing_buffer.Set_Current_Year( current_year );
   end Map_And_Set_Parameters; 

 
   
   function Get_Default_Run return Run is
   begin
      Log( "Get_Default_Run Entered" );
      return Run_IO.Get_Current_Run_For( Model.WSC.Users.DEFAULT_USERNAME, edited );
   end Get_Default_Run;

   function Validate( 
      username : Unbounded_String; 
      password : Unbounded_String ) return Model.WSC.Users.User_Type is
   use Model.WSC.Users;
      user : User_Type := User_Type_IO.Retrieve_By_PK( username ); 
   begin
      Log( "Validate; got username as |" & TS( username ) & "| password |" & TS( password ) & "| user password |" & TS( user.password ) & "|");
      if( password /= Null_Unbounded_String ) and then 
        ( user /= INVALID_USER ) and then 
        ( user.password = password )then
         User_Type_IO.Update_Last_Used( user );
         return user;
      end if;
      return INVALID_USER;
   end Validate;
   
   procedure Shutdown_Globals_And_Pools is
   begin
      Connection_Pool.Shutdown;
   end Shutdown_Globals_And_Pools;

   
   procedure Initialise_Globals_And_Pools( connection_pool_size : Positive ) is
   use Ada.Text_IO;
   begin
      --
      -- add opening Logger here
      --
      Connection_Pool.Initialise(
            Environment.Get_Server_Name,
            Environment.Get_Username,
            Environment.Get_Password,
            connection_pool_size );
      State_IO.Cleanup;
      Log( "Initialise_Globals_And_Pools: state IO cleanup OK" );   
      Run_IO.Clear_Status_Flag_For( NULL_UNBOUNDED_STRING, running_or_queued );
      Log( "Initialise_Globals_And_Pools: status flag cleanup OK" );   
      Model.WSC.Formatting.Load_Translations;
      Log( "Initialise_Globals_And_Pools: translations loaded" );   
   end Initialise_Globals_And_Pools;      
   
 
end Model.WSC.Globals;
