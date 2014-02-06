with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;

with AUnit.Assertions;
with AUnit.Test_Cases;
with DOM.Core.Attrs;
with DOM.Core.Documents;    
with DOM.Core.Nodes;
with DOM.Core;        
with DOM.Readers; 
with Sax.Readers;     
with Templates_Parser;
with GNAT.MD5;

with AWS.Config;
-- with AWS.Log;
with AWS.Messages;
with AWS.Mime;
with AWS.Resources;
with AWS.Resources;
with AWS.Response.Set;
with AWS.Response;
with AWS.Server;

with Callbacks;

with Base_Model_Types;
with Text_Utils;
with Input_Sources.File;
with Keyed_Text_Buffer;
with Line_Extractor;
with Web_Utils;
with XML_Utils.Conversions;
with XML_Utils;

with Model.WSC.Run_Settings;
with Model.WSC.Formatting;
with Model.WSC.Global_Settings;
with Model.WSC.Globals;
with Model.WSC.Parameter_System_Declarations;
with Model.WSC.Parameters.IO;
with Model.WSC.Parameters;
with Model.WSC.Run_Declarations;
with Model.WSC.Users.IO;
with Model.WSC.Users;
with Parameter_System.Input_Buffer;
with Parameter_System.Input_Buffer.HTML_Renderer;
with Parameter_System.Input_Buffer.WSC_Renderer;
with Parameter_System.Iterator;
with Parameter_System.XML;
with Parameter_System;
with Parameter_System_IO_Commons;
with WSC_Enums;


with Time_Series_Chart_Generator;

--
-- tmp tmp tmp
--
with Disaggregated_Data_Table_Cell_description_io;
with Disaggregated_Data_Table_Cell_io;
with Disaggregated_Data_Table_Description_io;
with Disaggregated_Data_Table_Io;
with Key_Value_Parameter_IO;
with Personal_income_io;
with Personal_results_io;
with Probit_threshold_io;
with Run_io;
with State_io;
with Table_stats_io;
with Uprate_assumption_io;
with User_type_io;
with Key_Value_DAO;
with Model.WSC.Parameters.DAO;
with Tabulator.DAO;
with Costs_Tabulator.DAO;
with Model.WSC.Summary_Items_DAO;
with Model.WSC.Output.DAO;
with Parameter_System_IO_Commons;

package body DOM_And_Parameter_Tests is

   use AUnit.Assertions;
   use AUnit.Test_Cases.Registration;
   use Ada.Text_IO;
   use Text_Utils;
   use Base_Model_Types;
   use Parameter_System_IO_Commons;
   
   use WSC_Enums;
   use Model.WSC.Parameter_System_Declarations;
   use type Parameter_System_IO_Commons.Buffer_Retrieval_Type;
 
   DELIMITER : constant Character := '.';
   default_wsc_run : Model.WSC.Run_Declarations.Run := Model.WSC.Globals.Get_Default_Run;

   function Get_Std_Translations return Templates_Parser.Translate_Set is
   -- ( request : in AWS.Status.Data; user : users.User_Type ) return Templates_Parser.Translate_Set is
   use Templates_Parser;
      -- URI    : constant String := AWS.Status.URI( Request );
      translations : Translate_Set;
   begin
      Insert( translations, Assoc( "LANG", Model.WSC.Formatting.Lang_Str( en )));
      Insert( translations, Assoc( "TEMPLATE_ROOT",  Model.WSC.Global_Settings.template_components_path ));
      Insert( translations, Assoc( "SEP", Model.WSC.Global_Settings.Dir_Separator ));
      Insert( translations, Assoc( "ROOT", Model.WSC.Global_Settings.WSC_Web_Root ));
      Insert( translations, Assoc( "USERNAME", "HELLE" ));
      Insert( translations, Assoc( "RANDOM_STRING", "HEHEHRAND" ));
      Insert( translations, Assoc( "URI", "THE URI" ));
      Insert( translations, Assoc( "PAGE-TITLE", "THE TITLE" ));
      return translations;
   end Get_Std_Translations;

   procedure Test_Load_Run_Settings( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use type Model.WSC.Run_Declarations.Run;
      rsb : WSC_Editing_Buffer;
      r1,r2,r3 : Model.WSC.Run_Declarations.Run;
      buff : keyed_text_buffer.Text_Buffer;
   begin
      r1 := default_wsc_run;
      r2 := r1;
      Put_Line( "Test_Load_Run_Settings started" );
      rsb := Get_Loaded_Run_Settings_Buffer( default_wsc_run );
      Put_Line( "Test_Load_Run_Settings ended OK" );
      Put_Line( "Test_Load_Run_Settings  KEYS ARE |" & rsb.Print_Keys );
       -- sys := globals.Get_BE_Parameter_System;
      buff :=  rsb.To_Text_Buffer( get_all );
      Put_Line( "Test_Load_Run_Settings startedCOMPLETE PARAMSET IS " & To_String( buff )); 
      Model.WSC.Run_Declarations.Update_From_Text_Buffer( r2, buff );
      Run_IO.Save( r2 );
      r3 := Run_IO.Retrieve_By_PK( r2.run_id, r2.username );
      Assert( r1 = r2, " runs unequal r1 r2 " );
      Assert( r2 = r3, " runs unequal r2 r3 " );
   end Test_Load_Run_Settings;
   
   procedure Test_Parameters_IO( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      -- doc : domc.Document;
      use Model.WSC.Users;
      use type keyed_text_buffer.Text_Buffer;
      
      sys : WSC_Parameter_Editing_System.Parameter_System_Rec;
      tax_sys : WSC_Parameter_Editing_System.Parameter_System_Rec;
      loaded_buff : WSC_Editing_Buffer;
      m        : Amount;
      user     : User_Type;
      page     : Unbounded_String;
      depth    : Natural;
      extra_translations : Templates_Parser.Translate_Set := Get_Std_Translations;
      menu     : Unbounded_String;
      path     : Unbounded_String_List;
      path_str : Unbounded_String;
      kvb1, kvb2  : keyed_text_buffer.Text_Buffer;
   begin
      Put_Line( "Test_Parameters_IO" );
      sys := Model.WSC.Parameter_System_Declarations.Get_WSC_Parameter_Editing_System;
      Model.WSC.Parameter_System_Declarations.WSC_Utils.Write_Parameter_File_Skeleton( "tmp/wsc_skel.prm", sys );
      Put_Line( "written skel" );
      loaded_buff := Model.WSC.Parameter_System_Declarations.Get_Loaded_Input_Buffer( default_wsc_run );
      
      for y in default_wsc_run.Start_Year .. 2012 loop
         loaded_buff.Set_Current_Year( y );
         m := loaded_buff.Get( TuS( "wsc.benefits.state_pension.class_a" ));
         Put_Line( y'Img & " : " & m'Img );
      end loop;
      loaded_buff.Operate( Null_Unbounded_String, 0.10, 0, default_wsc_run.Start_Year, 2012 );
      for y in default_wsc_run.Start_Year .. 2012 loop
         loaded_buff.Set_Current_Year( y );
         m := loaded_buff.Get( TuS( "social_care.means_test.non_residential.income.maximum_charge" ));
         Put_Line( y'Img & " : " & m'Img );
      end loop;
      
      kvb1 := loaded_buff.To_Text_Buffer( get_all );
      Run_IO.Store_Params_From_Text_Buffer( default_wsc_run, kvb1 );
      kvb2 := Run_IO.Map_Associated_Params_To_Text_Buffer( default_wsc_run );
      
      Assert( kvb1 = kvb2, "kvb1 /= kvb2 1=" & To_String( kvb1 ) & " 2 = " & To_String( kvb2 ));
      
      Put_Line( "KEYS ARE |" & loaded_buff.Print_Keys );
       -- sys := globals.Get_BE_Parameter_System;
      Put_Line( "COMPLETE PARAMSET IS " & To_String( kvb1 )); 
      -- path.Append( TuS( "default_wsc_run.Start_Year" ));
      path.Append( TuS( "benefits" ));
      path.Append( TuS( "pension_credit" ));
      path.Append( TuS( "guaranteed_credit" ));
      -- sys.Complete_Path_To_Left( path, depth );
      path_str := TuS( "wsc" ) & TuS( Join( path, '.' ));
      
      Put_Line( "PATH TO LEFT |" & TS( path_str ) & "|" );
      menu := WSC_Renderer.Make_Parameter_Menu( 
         default_wsc_run.Start_Year,
         default_wsc_run.End_Year,
         default_wsc_run.Start_Year,
         sys,
         path,
         TuS( "/wsc/parameters_page/" ),
         en,
         user );
      Put_Line( TS( menu ));
      loaded_buff.Set_Current_Year( default_wsc_run.Start_Year );
      tax_sys := sys.Get( TS( path_str ));
      
      page := WSC_Renderer.Create_Input_Page( 
            title               => TuS( "THE TITLE" ),
            buff                => loaded_buff,
            model_menu          => menu,
            breadcrumb          => TuS( "BREADCRUMBS" ),
            base_sys            => sys, 
            sys                 => tax_sys,
            parameter_prefix    => TuS( Join( path, '.' )),
            main_error_message  => TuS( "ERROR MESSAGE" ),
            job_is_running      => False,
            user                => user,
            extra_translations  => extra_translations );
      Put_Line( "PAGE == " );
      Put_Line( TS( page ));
      
   end Test_Parameters_IO;
   
   procedure Test_Run_Settings_IO( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.WSC.Run_Declarations;
      rs1, rs2 : Model.WSC.Run_Declarations.Run;
   begin
      rs1 := default_wsc_run;
      Write_Settings( "tmp/wsc_run.txt", rs1 );
      rs2 := Read_Settings( "tmp/wsc_run.txt" );
      Assert( rs1 = rs2, "settings should match " );
   end Test_Run_Settings_IO;

   procedure Test_Run_State_IO( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.WSC.Run_Settings;
      rs1, rs2 : State_Type;
   begin
      rs1.household := 1001;
      rs1.other_counter := 1002;
      rs1.year := 2020;
      rs1.phase := generating_output;
      rs1.health := normal;
      rs1.error_code := -99;
      rs1.message := TuS( "HELLO MUM" );
      rs1.read_error := True;
      Write_State( "tmp/state.txt", rs1 );
      rs2 := Read_State( "tmp/state.txt" );
      Assert( rs1 = rs2, "settings should match " );
   end Test_Run_State_IO;
   
   
   procedure Test_A_Parameters_IO( t : in out AUnit.Test_Cases.Test_Case'Class ) is
   use Model.WSC.Parameters;
   use Model.WSC.Parameters.IO;
      p1, p2, p3, p4 : Parameters_Array;
   begin
      p1 := Read( "etc/wsc.prm", default_wsc_run.Start_Year, default_wsc_run.End_Year );
      p4 := Read( "etc/wsc.prm", default_wsc_run.Start_Year, default_wsc_run.End_Year );
      Write( p1, "tmp/wsc.prm", default_wsc_run.Start_Year, default_wsc_run.End_Year );
      p2 := Read( "tmp/wsc.prm", default_wsc_run.Start_Year, default_wsc_run.End_Year );
      p3 := Read( "tmp/wsc.prm", default_wsc_run.Start_Year, default_wsc_run.End_Year );
      Write( p2, "tmp/wsc2.prm", default_wsc_run.Start_Year, default_wsc_run.End_Year );
      for y in default_wsc_run.Start_Year .. default_wsc_run.End_Year loop
         Assert( p1( y ) = p4( y ), "params p1 p4 should match for year " & y'Img );
         Assert( p2( y ) = p3( y ), "params p3 p2 should match for year " & y'Img );
         Assert( p2( y ) = p1( y ), "params p1 p2 should match for year " & y'Img );
      end loop;
   end Test_A_Parameters_IO;
   
   procedure User_Tests( t : in out AUnit.Test_Cases.Test_Case'Class ) is
      use Model.WSC.Users;
      use Model.WSC.Users.IO;
      user : User_Type;
      wsc_run : Model.WSC.Run_Declarations.Run := Model.WSC.Globals.Get_Default_Run;
      sep : constant String := Model.WSC.Global_Settings.Dir_Separator;
      working_dir : Unbounded_String;
   begin
      for uno in 1 .. 20 loop
         declare
            nstr : String := Positive'Image( uno );
            username : Unbounded_String := TuS( "test_user" ) & nstr( 2 .. nstr'Length );
            run_num : Positive;
         begin
            user.username := username;
            user.description  := TuS( "A TEST USER" );
            user.password := TuS( GNAT.MD5.Digest( "some password" ));
            Create_User_Files( Model.WSC.Global_Settings.Working_Root, sep, user );
            for rn in 1 .. 20 loop
               declare
                  run_num_str : String := rn'Img; -- Model.WSC.Model.Runner.Get_New_Run_Id;
               begin
                  Put_Line( "Got working root as |" & Model.WSC.Global_Settings.Working_Root & "| " );
                  user.work_dir := Create_Directories_For_Run( Model.WSC.Global_Settings.Working_Root, sep, user, rn );
               end;
            end loop;
            -- Assert( run_num = 20, "run num should always finish on 20 but was " & Positive'Image( run_num ));
         end;
      end loop;
      -- -- Assert( not is_indexed_key, " 'FRED[ xxx ].jojo' shouldn't be indexed key but wasn" );
      -- for uno in 1 .. 20 loop
         -- declare
            -- default_pwd : constant Unbounded_String := TuS( GNAT.MD5.Digest( "some password" ));
            -- tmp_user : User_Type;
            -- nstr : String := Positive'Image( uno );
            -- username : Unbounded_String := TuS( "test_user" ) & nstr( 2 .. nstr'Length );
            -- filename : Unbounded_String := wsc_run.Working_Root & sep & username & sep & USER_FILE_NAME;
            -- run_num : Positive;
            -- param_buffer  : Param_Buff;
         -- begin
            -- tmp_user :=  Read_User( TS( filename ));
            -- Assert( tmp_user.username = username, "username mismatch; should be " & TS( username ) & "| was " & TS( tmp_user.username ));
            -- Assert( tmp_user.password = default_pwd, "password mismatch; should be " & TS( default_pwd ) & "| was " & TS( tmp_user.password ));
            -- -- run_num :=  Get_Next_Run_Number( wsc_run.Working_Root, sep, user );
            -- param_buffer := globals.Get_Loaded_Input_Buffer( tmp_user.lang );
            -- -- Assert( run_num = 21, "run num should be 21 on re-read but was " & Positive'Image( run_num ));
         -- end;
      -- end loop;
   end User_Tests;
   
   procedure Register_Tests( t : in out Test_Case ) is
   begin 
      Register_Routine( T,  Test_Load_Run_Settings'Access, "Test_Load_Run_Settings" );
      --Register_Routine( T,  Test_Run_State_IO'Access, "Test_Run_State_IO" );
      Register_Routine( T, Test_A_Parameters_IO'Access, "Test_Parameters_IO" );
      -- Register_Routine( T, Test_Run_Settings_IO'Access, "Test_Run_Settings_IO" );
      Register_Routine( T, Test_Parameters_IO'Access, "Test Parameter IO" );
      -- Register_Routine( T, User_Tests'Access, "User Tests" );
   end Register_Tests;

   procedure Set_Up (T : in out Test_Case) is
   begin
      null;
   end Set_Up;

   ----------
   -- Name --
   ----------
   function Name ( T : Test_Case ) return Message_String is
   begin
      return Format( "DOM and Parameter Tests" );
   end Name;


end DOM_And_Parameter_Tests;
