with Ada.Calendar.Formatting;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Keyed_Text_Buffer;
with Text_Utils;
with XML_Utils;
-- with Model.WSC.Run_Results;
with Model.WSC.Global_Settings;
with GNATColl.Traces;
with Run_IO;
with Model.WSC.Globals;
with Model.WSC.Parameters.DAO;

package body Model.WSC.Parameter_System_Declarations is

   use Ada.Text_IO;
   use Text_Utils;
   use Ada.Strings.Unbounded;
   
   text_params_loaded     : Boolean := False; 
   run_settings_loaded    : Boolean := False; 
   xml_params_loaded      : Boolean := False;
   xml_runsett_loaded     : Boolean := False;
   WSC_PARAMETERS         : WSC_Parameter_Editing_System.Parameter_System_Rec;
   WSC_RUNSETTINGS        : WSC_Parameter_Editing_System.Parameter_System_Rec;
   DEFAULT_PARAMS         : Keyed_Text_Buffer.Text_Buffer;
   DEFAULT_RUN_SETTINGS   : Keyed_Text_Buffer.Text_Buffer;
   
   log_trace : GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "MODEL.WSC.PARAMETER_SYSTEM_DECLARATIONS" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   procedure Uprate( 
               c    : in out Amount; 
               mult : Amount; 
               rec  : WSC_Parameter_Editing_System.Parameter_Rec; 
               which_operation : Integer := 0 ) is
   use WSC_Parameter_Editing_System;
   begin
      case rec.logical_type is
         -- need a rooker wise thing here
         when any_kind | tax_allowance |
              tax_band |  benefit | poverty_line =>
                 if( Long_Float( c * mult ) < Long_Float( Amount'Last ))then
                    c := c * mult;
                 end if;
         when others => null;
      end case;
   end Uprate;
   
   function Get_WSC_Parameter_Editing_System return WSC_Parameter_Editing_System.Parameter_System_Rec is
      use XML_Utils; -- domc
   begin
      if( not xml_params_loaded )then
         declare
            doc : domc.Document; 
            filename : constant String := Model.WSC.Global_Settings.Parameters_XML_File;
         begin
            Log( "Get_WSC_Parameter_Editing_System: opening " & filename );
            doc := XML_Utils.Get_Doc( filename, False );
            WSC_PARAMETERS := WSC_Parameter_Editing_System_XML.Load( doc );
            xml_params_loaded := True;
            Log( "Get_WSC_Parameter_Editing_System; successfully loaded" );
         end;
      end if;
      return WSC_PARAMETERS;
   end Get_WSC_Parameter_Editing_System;

   function Get_WSC_Run_Settings return WSC_Parameter_Editing_System.Parameter_System_Rec is
      use XML_Utils; -- domc
   begin
      if( not xml_runsett_loaded )then
         declare
            doc : domc.Document; 
            filename : constant String := Model.WSC.Global_Settings.Run_Settings_XML_File;
         begin
            Log( "Get_WSC_Run_Settings: opening " & filename );
            doc := XML_Utils.Get_Doc( filename, False );
            WSC_RUNSETTINGS := WSC_Parameter_Editing_System_XML.Load( doc );
            xml_runsett_loaded := True;
            Log( "Get_WSC_Run_Settings; successfully loaded" );
         end;
      end if;
      return WSC_RUNSETTINGS;
   end Get_WSC_Run_Settings;

   
   
   function Get_Default_Parameter_Keyed_Text_Buffer return Keyed_Text_Buffer.Text_Buffer is
      use Keyed_Text_Buffer;  
      filename : String := Model.WSC.Global_Settings.Default_Text_Parameter_File_Name;
   begin
      if( not text_params_loaded )then
         Log( "Get_Default_Parameter_Keyed_Text_Buffers: opening " & filename );
         DEFAULT_PARAMS := Load( filename, single_line_delimited );
         text_params_loaded := True;   
         Log( "Get_Default_Parameter_Keyed_Text_Buffer; successfully loaded" );
      end if;
      return DEFAULT_PARAMS;
   end Get_Default_Parameter_Keyed_Text_Buffer;

   
   function Get_Loaded_Input_Buffer( 
      wsc_run  : Run;
      lang     : Languages := en ) return WSC_Editing_Buffer is
      use Text_Utils;
      comparison_run  : Run := Get_Comparison_Run( wsc_run );
      params_map : Keyed_Text_Buffer.Text_Buffer := Run_IO.Map_Associated_Params_To_Text_Buffer( wsc_run );  
      default_hashmap_of_parameters : Keyed_Text_Buffer.Text_Buffer := Run_IO.Map_Associated_Params_To_Text_Buffer( comparison_run );
      sys_desc   : WSC_Parameter_Editing_System.Parameter_System_Rec := Get_WSC_Parameter_Editing_System;
      buff       : WSC_Editing_Buffer;
   begin
      Log( "Get_Loaded_Input_Buffer; loading" );
      buff := WSC_Parameter_Editing_System_IO.Init( 
         lang, 
         sys_desc, 
         params_map,
         default_hashmap_of_parameters,
         wsc_run.start_year,
         wsc_run.end_year,
         YEAR_PREFIX );
      Log( "Get_Loaded_Input_Buffer; success" );
      return buff;
   end Get_Loaded_Input_Buffer;

   function Get_Default_Run_Settings_Keyed_Text_Buffer return Keyed_Text_Buffer.Text_Buffer is
      use Keyed_Text_Buffer;
      default_run : Run; 
   begin
      if( not run_settings_loaded )then
         default_run := Model.WSC.Globals.Get_Default_Run;
         Log( "Get_Default_Parameter_Keyed_Text_Buffers: opening "  );
         DEFAULT_RUN_SETTINGS := default_run.To_Text_Buffer;
         run_settings_loaded := True;   
         Log( "Get_Default_Run_Settings_Keyed_Text_Buffer; successfully loaded" );
      end if;
      return DEFAULT_RUN_SETTINGS;
   end Get_Default_Run_Settings_Keyed_Text_Buffer;

   function Get_Comparison_Run( wsc_run : Run ) return Run is
      crun : Run;
      username : Unbounded_String := wsc_run.username;
      rid      : Natural := wsc_run.comparison_run_id;
   begin
      if( rid > 1_000_000 )then
         username := TuS( "default" );
         rid := rid - 1_000_000;
      end if;
      Log( "Get_Comparison_Run; rid= " & rid'Img & " username |" & TS( username ) & "|" );
      crun := Run_IO.Retrieve_By_PK( rid, username );
      return crun;
   end Get_Comparison_Run;
   
      
   function Get_Default_Model_Parameters( wsc_run : Run ) return Model.WSC.Parameters.Parameters_Array is
      r : Run := Get_Comparison_Run( wsc_run );
   begin
      Log( "Get_Default_Model_Parameters entered" );
      return Model.WSC.Parameters.DAO.Read( r, r.start_year, r.end_year );
   end Get_Default_Model_Parameters;


   
   
   function Get_Loaded_Run_Settings_Buffer( wsc_run : Run ) return WSC_Editing_Buffer is
      use Text_Utils;
      hashmap_of_parameters         : Keyed_Text_Buffer.Text_Buffer;
      default_hashmap_of_parameters : Keyed_Text_Buffer.Text_Buffer;
      comparison_run                : Run := Get_Comparison_Run( wsc_run );
      system_structure              : WSC_Parameter_Editing_System.Parameter_System_Rec;
      loaded_editing_buffer         : WSC_Editing_Buffer;
   begin
      Log( "Get_Loaded_Run_Settings_Buffer; at start Input system is " & To_String( wsc_run ));
      hashmap_of_parameters := wsc_run.To_Text_Buffer;
      default_hashmap_of_parameters := comparison_run.To_Text_Buffer;
      Log( "got hashmap_of_parameters as " & To_String( hashmap_of_parameters ));
      system_structure := Get_WSC_Run_Settings;
      Log( "got system_structure ok" );
      loaded_editing_buffer := WSC_Parameter_Editing_System_IO.Init( 
         language => en, 
         system_structure => system_structure, 
         hashmap_of_parameters => hashmap_of_parameters,
         default_hashmap => default_hashmap_of_parameters );
      Log( "Get_Loaded_Run_Settings_Buffer; success" );
      return loaded_editing_buffer;
   end Get_Loaded_Run_Settings_Buffer;
      
   
   function Get_Null_WSC_Editing_Buffer return WSC_Editing_Buffer is
      b : WSC_Editing_Buffer;
   begin
      return b;
   end Get_Null_WSC_Editing_Buffer;

begin
   WSC_Parameter_Editing_System.Set_Delimiter( DELIMITER );   
end Model.WSC.Parameter_System_Declarations;
