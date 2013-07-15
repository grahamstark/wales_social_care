--
-- Created by ada_generator.py on 2012-07-24 19:03:56.071549
-- 


with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded; 

with AUnit.Assertions;             
with AUnit.Test_Cases; 
   
with DB_Logger;
with Base_Types;
with Environment;
with DB_Commons.ODBC;
with DB_Commons;
with Wsc_Db_Data;

with Connection_Pool;

with User_Type_IO;
with Dataset_IO;
with Run_IO;
with State_IO;
with Key_Value_Parameter_IO;
with Uprate_Assumption_IO;
with Probit_Threshold_IO;
with Personal_Results_IO;
with Personal_Income_IO;
with Disaggregated_Data_Table_Description_IO;
with Disaggregated_Data_Table_Cell_Description_IO;
with Disaggregated_Data_Table_IO;
with Disaggregated_Data_Table_Cell_IO;
with Maxima_And_Totals_IO;
with Uap_Threshold_IO;
with Table_Stats_IO;
with Household_Capital_IO;
with Gain_Lose_IO;
with Household_Data_IO;
with Person_IO;

package body Wsc_Db_Test is

   RECORDS_TO_ADD     : constant Integer := 100;
   RECORDS_TO_DELETE  : constant Integer := 50;
   RECORDS_TO_ALTER   : constant Integer := 50;
   
   package d renames DB_Commons;
   
   use Base_Types;
   use ada.strings.Unbounded;
   use Wsc_Db_Data;
   
   use AUnit.Test_Cases;
   use AUnit.Assertions;
   use AUnit.Test_Cases.Registration;
   
   use Ada.Strings.Unbounded;
   use Ada.Exceptions;
   use Ada.Calendar;
   
   
--
-- test creating and deleting records  
--
--
   procedure User_Type_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : User_Type_List.Cursor ) is 
      User_Type_Test_Item : Wsc_Db_Data.User_Type;
      begin
         User_Type_Test_Item := User_Type_List.element( pos );
         DB_Logger.info( To_String( User_Type_Test_Item ));
      end print;

   
      User_Type_Test_Item : Wsc_Db_Data.User_Type;
      User_Type_test_list : Wsc_Db_Data.User_Type_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test User_Type_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      User_Type_io.Delete( criteria );
      
      DB_Logger.info( "User_Type_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         User_Type_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         User_Type_Test_Item.Password := To_Unbounded_String("dat forPassword");
         User_Type_Test_Item.Title := To_Unbounded_String("dat forTitle");
         User_Type_Test_Item.Description := To_Unbounded_String("dat forDescription");
         User_Type_Test_Item.Email := To_Unbounded_String("dat forEmail");
         User_Type_Test_Item.Work_Dir := To_Unbounded_String("dat forWork_Dir");
         -- missingUser_Type_Test_Item declaration ;
         -- missingUser_Type_Test_Item declaration ;
         User_Type_Test_Item.Preferences := To_Unbounded_String("dat forPreferences");
         User_Type_Test_Item.Last_Used := Ada.Calendar.Clock;
         User_Type_io.Save( User_Type_Test_Item, False );         
      end loop;
      
      User_Type_test_list := User_Type_Io.Retrieve( criteria );
      
      DB_Logger.info( "User_Type_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         User_Type_Test_Item := User_Type_List.element( User_Type_test_list, i );
         User_Type_Test_Item.Password := To_Unbounded_String("Altered::dat forPassword");
         User_Type_Test_Item.Title := To_Unbounded_String("Altered::dat forTitle");
         User_Type_Test_Item.Description := To_Unbounded_String("Altered::dat forDescription");
         User_Type_Test_Item.Email := To_Unbounded_String("Altered::dat forEmail");
         User_Type_Test_Item.Work_Dir := To_Unbounded_String("Altered::dat forWork_Dir");
         User_Type_Test_Item.Preferences := To_Unbounded_String("Altered::dat forPreferences");
         User_Type_io.Save( User_Type_Test_Item );         
      end loop;
      
      DB_Logger.info( "User_Type_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         User_Type_Test_Item := User_Type_List.element( User_Type_test_list, i );
         User_Type_io.Delete( User_Type_Test_Item );         
      end loop;
      
      DB_Logger.info( "User_Type_Create_Test: retrieve all records" );
      User_Type_List.iterate( User_Type_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test User_Type_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "User_Type_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "User_Type_Create_Test : exception thrown " & Exception_Information(Error) );
   end User_Type_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Dataset_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Dataset_List.Cursor ) is 
      Dataset_Test_Item : Wsc_Db_Data.Dataset;
      begin
         Dataset_Test_Item := Dataset_List.element( pos );
         DB_Logger.info( To_String( Dataset_Test_Item ));
      end print;

   
      Dataset_Test_Item : Wsc_Db_Data.Dataset;
      Dataset_test_list : Wsc_Db_Data.Dataset_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Dataset_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Dataset_io.Delete( criteria );
      
      DB_Logger.info( "Dataset_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Dataset_Test_Item.Name := To_Unbounded_String( "k_" & i'Img );
         Dataset_Test_Item.Creator := To_Unbounded_String("dat forCreator");
         Dataset_Test_Item.Title := To_Unbounded_String("dat forTitle");
         -- missingDataset_Test_Item declaration ;
         Dataset_io.Save( Dataset_Test_Item, False );         
      end loop;
      
      Dataset_test_list := Dataset_Io.Retrieve( criteria );
      
      DB_Logger.info( "Dataset_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Dataset_Test_Item := Dataset_List.element( Dataset_test_list, i );
         Dataset_Test_Item.Creator := To_Unbounded_String("Altered::dat forCreator");
         Dataset_Test_Item.Title := To_Unbounded_String("Altered::dat forTitle");
         Dataset_io.Save( Dataset_Test_Item );         
      end loop;
      
      DB_Logger.info( "Dataset_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Dataset_Test_Item := Dataset_List.element( Dataset_test_list, i );
         Dataset_io.Delete( Dataset_Test_Item );         
      end loop;
      
      DB_Logger.info( "Dataset_Create_Test: retrieve all records" );
      Dataset_List.iterate( Dataset_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Dataset_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Dataset_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Dataset_Create_Test : exception thrown " & Exception_Information(Error) );
   end Dataset_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Run_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Run_List.Cursor ) is 
      Run_Test_Item : Wsc_Db_Data.Run;
      begin
         Run_Test_Item := Run_List.element( pos );
         DB_Logger.info( To_String( Run_Test_Item ));
      end print;

   
      Run_Test_Item : Wsc_Db_Data.Run;
      Run_test_list : Wsc_Db_Data.Run_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Run_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Run_io.Delete( criteria );
      
      DB_Logger.info( "Run_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Run_Test_Item.Run_Id := Run_io.Next_Free_Run_Id;
         Run_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         Run_Test_Item.Comparison_Username := To_Unbounded_String("dat forComparison_Username");
         -- missingRun_Test_Item declaration ;
         Run_Test_Item.Title := To_Unbounded_String("dat forTitle");
         -- missingRun_Test_Item declaration ;
         -- missingRun_Test_Item declaration ;
         Run_Test_Item.Interest_Rate_Pct := 1010100.012;
         -- missingRun_Test_Item declaration ;
         -- missingRun_Test_Item declaration ;
         Run_Test_Item.Working_Root := To_Unbounded_String("dat forWorking_Root");
         Run_Test_Item.Users_Directory := To_Unbounded_String("dat forUsers_Directory");
         Run_Test_Item.Output_Directory := To_Unbounded_String("dat forOutput_Directory");
         Run_Test_Item.Dir_Separator := To_Unbounded_String("dat forDir_Separator");
         Run_Test_Item.Session_Id := To_Unbounded_String("dat forSession_Id");
         Run_Test_Item.Dataset_Name := To_Unbounded_String("dat forDataset_Name");
         -- missingRun_Test_Item declaration ;
         -- missingRun_Test_Item declaration ;
         -- missingRun_Test_Item declaration ;
         -- missingRun_Test_Item declaration ;
         Run_Test_Item.Weighting_Lower_Bound := 1010100.012;
         Run_Test_Item.Weighting_Upper_Bound := 1010100.012;
         -- missingRun_Test_Item declaration ;
         -- missingRun_Test_Item declaration ;
         Run_io.Save( Run_Test_Item, False );         
      end loop;
      
      Run_test_list := Run_Io.Retrieve( criteria );
      
      DB_Logger.info( "Run_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Run_Test_Item := Run_List.element( Run_test_list, i );
         Run_Test_Item.Comparison_Username := To_Unbounded_String("Altered::dat forComparison_Username");
         Run_Test_Item.Title := To_Unbounded_String("Altered::dat forTitle");
         Run_Test_Item.Working_Root := To_Unbounded_String("Altered::dat forWorking_Root");
         Run_Test_Item.Users_Directory := To_Unbounded_String("Altered::dat forUsers_Directory");
         Run_Test_Item.Output_Directory := To_Unbounded_String("Altered::dat forOutput_Directory");
         Run_Test_Item.Dir_Separator := To_Unbounded_String("Altered::dat forDir_Separator");
         Run_Test_Item.Session_Id := To_Unbounded_String("Altered::dat forSession_Id");
         Run_Test_Item.Dataset_Name := To_Unbounded_String("Altered::dat forDataset_Name");
         Run_io.Save( Run_Test_Item );         
      end loop;
      
      DB_Logger.info( "Run_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Run_Test_Item := Run_List.element( Run_test_list, i );
         Run_io.Delete( Run_Test_Item );         
      end loop;
      
      DB_Logger.info( "Run_Create_Test: retrieve all records" );
      Run_List.iterate( Run_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Run_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Run_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Run_Create_Test : exception thrown " & Exception_Information(Error) );
   end Run_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure State_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : State_List.Cursor ) is 
      State_Test_Item : Wsc_Db_Data.State;
      begin
         State_Test_Item := State_List.element( pos );
         DB_Logger.info( To_String( State_Test_Item ));
      end print;

   
      State_Test_Item : Wsc_Db_Data.State;
      State_test_list : Wsc_Db_Data.State_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test State_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      State_io.Delete( criteria );
      
      DB_Logger.info( "State_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         State_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         State_Test_Item.Run_Id := State_io.Next_Free_Run_Id;
         -- missingState_Test_Item declaration ;
         -- missingState_Test_Item declaration ;
         -- missingState_Test_Item declaration ;
         -- missingState_Test_Item declaration ;
         -- missingState_Test_Item declaration ;
         -- missingState_Test_Item declaration ;
         -- missingState_Test_Item declaration ;
         State_Test_Item.Session_Id := To_Unbounded_String("dat forSession_Id");
         State_io.Save( State_Test_Item, False );         
      end loop;
      
      State_test_list := State_Io.Retrieve( criteria );
      
      DB_Logger.info( "State_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         State_Test_Item := State_List.element( State_test_list, i );
         State_Test_Item.Session_Id := To_Unbounded_String("Altered::dat forSession_Id");
         State_io.Save( State_Test_Item );         
      end loop;
      
      DB_Logger.info( "State_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         State_Test_Item := State_List.element( State_test_list, i );
         State_io.Delete( State_Test_Item );         
      end loop;
      
      DB_Logger.info( "State_Create_Test: retrieve all records" );
      State_List.iterate( State_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test State_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "State_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "State_Create_Test : exception thrown " & Exception_Information(Error) );
   end State_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Key_Value_Parameter_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Key_Value_Parameter_List.Cursor ) is 
      Key_Value_Parameter_Test_Item : Wsc_Db_Data.Key_Value_Parameter;
      begin
         Key_Value_Parameter_Test_Item := Key_Value_Parameter_List.element( pos );
         DB_Logger.info( To_String( Key_Value_Parameter_Test_Item ));
      end print;

   
      Key_Value_Parameter_Test_Item : Wsc_Db_Data.Key_Value_Parameter;
      Key_Value_Parameter_test_list : Wsc_Db_Data.Key_Value_Parameter_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Key_Value_Parameter_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Key_Value_Parameter_io.Delete( criteria );
      
      DB_Logger.info( "Key_Value_Parameter_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Key_Value_Parameter_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         Key_Value_Parameter_Test_Item.Run_Id := Key_Value_Parameter_io.Next_Free_Run_Id;
         Key_Value_Parameter_Test_Item.Key := To_Unbounded_String( "k_" & i'Img );
         Key_Value_Parameter_Test_Item.Val := To_Unbounded_String("dat forVal");
         Key_Value_Parameter_io.Save( Key_Value_Parameter_Test_Item, False );         
      end loop;
      
      Key_Value_Parameter_test_list := Key_Value_Parameter_Io.Retrieve( criteria );
      
      DB_Logger.info( "Key_Value_Parameter_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Key_Value_Parameter_Test_Item := Key_Value_Parameter_List.element( Key_Value_Parameter_test_list, i );
         Key_Value_Parameter_Test_Item.Val := To_Unbounded_String("Altered::dat forVal");
         Key_Value_Parameter_io.Save( Key_Value_Parameter_Test_Item );         
      end loop;
      
      DB_Logger.info( "Key_Value_Parameter_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Key_Value_Parameter_Test_Item := Key_Value_Parameter_List.element( Key_Value_Parameter_test_list, i );
         Key_Value_Parameter_io.Delete( Key_Value_Parameter_Test_Item );         
      end loop;
      
      DB_Logger.info( "Key_Value_Parameter_Create_Test: retrieve all records" );
      Key_Value_Parameter_List.iterate( Key_Value_Parameter_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Key_Value_Parameter_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Key_Value_Parameter_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Key_Value_Parameter_Create_Test : exception thrown " & Exception_Information(Error) );
   end Key_Value_Parameter_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Uprate_Assumption_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Uprate_Assumption_List.Cursor ) is 
      Uprate_Assumption_Test_Item : Wsc_Db_Data.Uprate_Assumption;
      begin
         Uprate_Assumption_Test_Item := Uprate_Assumption_List.element( pos );
         DB_Logger.info( To_String( Uprate_Assumption_Test_Item ));
      end print;

   
      Uprate_Assumption_Test_Item : Wsc_Db_Data.Uprate_Assumption;
      Uprate_Assumption_test_list : Wsc_Db_Data.Uprate_Assumption_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Uprate_Assumption_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Uprate_Assumption_io.Delete( criteria );
      
      DB_Logger.info( "Uprate_Assumption_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Uprate_Assumption_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         Uprate_Assumption_Test_Item.Run_Id := Uprate_Assumption_io.Next_Free_Run_Id;
         Uprate_Assumption_Test_Item.Run_Id := Uprate_Assumption_io.Next_Free_Run_Id;
         Uprate_Assumption_Test_Item.Percent_Change := 1010100.012;
         -- missingUprate_Assumption_Test_Item declaration ;
         -- missingUprate_Assumption_Test_Item declaration ;
         Uprate_Assumption_io.Save( Uprate_Assumption_Test_Item, False );         
      end loop;
      
      Uprate_Assumption_test_list := Uprate_Assumption_Io.Retrieve( criteria );
      
      DB_Logger.info( "Uprate_Assumption_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Uprate_Assumption_Test_Item := Uprate_Assumption_List.element( Uprate_Assumption_test_list, i );
         Uprate_Assumption_io.Save( Uprate_Assumption_Test_Item );         
      end loop;
      
      DB_Logger.info( "Uprate_Assumption_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Uprate_Assumption_Test_Item := Uprate_Assumption_List.element( Uprate_Assumption_test_list, i );
         Uprate_Assumption_io.Delete( Uprate_Assumption_Test_Item );         
      end loop;
      
      DB_Logger.info( "Uprate_Assumption_Create_Test: retrieve all records" );
      Uprate_Assumption_List.iterate( Uprate_Assumption_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Uprate_Assumption_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Uprate_Assumption_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Uprate_Assumption_Create_Test : exception thrown " & Exception_Information(Error) );
   end Uprate_Assumption_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Probit_Threshold_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Probit_Threshold_List.Cursor ) is 
      Probit_Threshold_Test_Item : Wsc_Db_Data.Probit_Threshold;
      begin
         Probit_Threshold_Test_Item := Probit_Threshold_List.element( pos );
         DB_Logger.info( To_String( Probit_Threshold_Test_Item ));
      end print;

   
      Probit_Threshold_Test_Item : Wsc_Db_Data.Probit_Threshold;
      Probit_Threshold_test_list : Wsc_Db_Data.Probit_Threshold_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Probit_Threshold_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Probit_Threshold_io.Delete( criteria );
      
      DB_Logger.info( "Probit_Threshold_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Probit_Threshold_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         Probit_Threshold_Test_Item.Run_Id := Probit_Threshold_io.Next_Free_Run_Id;
         Probit_Threshold_Test_Item.Run_Id := Probit_Threshold_io.Next_Free_Run_Id;
         Probit_Threshold_Test_Item.Threshold := 1010100.012;
         Probit_Threshold_io.Save( Probit_Threshold_Test_Item, False );         
      end loop;
      
      Probit_Threshold_test_list := Probit_Threshold_Io.Retrieve( criteria );
      
      DB_Logger.info( "Probit_Threshold_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Probit_Threshold_Test_Item := Probit_Threshold_List.element( Probit_Threshold_test_list, i );
         Probit_Threshold_io.Save( Probit_Threshold_Test_Item );         
      end loop;
      
      DB_Logger.info( "Probit_Threshold_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Probit_Threshold_Test_Item := Probit_Threshold_List.element( Probit_Threshold_test_list, i );
         Probit_Threshold_io.Delete( Probit_Threshold_Test_Item );         
      end loop;
      
      DB_Logger.info( "Probit_Threshold_Create_Test: retrieve all records" );
      Probit_Threshold_List.iterate( Probit_Threshold_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Probit_Threshold_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Probit_Threshold_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Probit_Threshold_Create_Test : exception thrown " & Exception_Information(Error) );
   end Probit_Threshold_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Personal_Results_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Personal_Results_List.Cursor ) is 
      Personal_Results_Test_Item : Wsc_Db_Data.Personal_Results;
      begin
         Personal_Results_Test_Item := Personal_Results_List.element( pos );
         DB_Logger.info( To_String( Personal_Results_Test_Item ));
      end print;

   
      Personal_Results_Test_Item : Wsc_Db_Data.Personal_Results;
      Personal_Results_test_list : Wsc_Db_Data.Personal_Results_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Personal_Results_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Personal_Results_io.Delete( criteria );
      
      DB_Logger.info( "Personal_Results_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Personal_Results_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         Personal_Results_Test_Item.Run_Id := Personal_Results_io.Next_Free_Run_Id;
         Personal_Results_Test_Item.Sysno := Personal_Results_io.Next_Free_Sysno;
         Personal_Results_Test_Item.Iteration := Personal_Results_io.Next_Free_Iteration;
         Personal_Results_Test_Item.Pid := Personal_Results_io.Next_Free_Pid;
         Personal_Results_Test_Item.Wave := To_Unbounded_String( "k_" & i'Img );
         -- missingPersonal_Results_Test_Item declaration ;
         -- missingPersonal_Results_Test_Item declaration ;
         -- missingPersonal_Results_Test_Item declaration ;
         -- missingPersonal_Results_Test_Item declaration ;
         -- missingPersonal_Results_Test_Item declaration ;
         -- missingPersonal_Results_Test_Item declaration ;
         -- missingPersonal_Results_Test_Item declaration ;
         -- missingPersonal_Results_Test_Item declaration ;
         -- missingPersonal_Results_Test_Item declaration ;
         Personal_Results_Test_Item.La_Contributions := 1010100.012;
         Personal_Results_Test_Item.Client_Contributions := 1010100.012;
         Personal_Results_Test_Item.Gross_Care_Costs := 1010100.012;
         Personal_Results_Test_Item.Total_Payments_To_Date := 1010100.012;
         Personal_Results_Test_Item.Disposable_Income := 1010100.012;
         Personal_Results_Test_Item.Net_Income := 1010100.012;
         Personal_Results_Test_Item.Marginal_Rate := 1010100.012;
         Personal_Results_Test_Item.Capital_Contribution := 1010100.012;
         Personal_Results_Test_Item.Minimum_Income_Guarantee := 1010100.012;
         Personal_Results_Test_Item.Hours_Of_Care_La := 1010100.012;
         Personal_Results_Test_Item.Hours_Of_Care_Private := 1010100.012;
         -- missingPersonal_Results_Test_Item declaration ;
         Personal_Results_Test_Item.Remaining_Capital_Stock := 1010100.012;
         Personal_Results_io.Save( Personal_Results_Test_Item, False );         
      end loop;
      
      Personal_Results_test_list := Personal_Results_Io.Retrieve( criteria );
      
      DB_Logger.info( "Personal_Results_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Personal_Results_Test_Item := Personal_Results_List.element( Personal_Results_test_list, i );
         Personal_Results_io.Save( Personal_Results_Test_Item );         
      end loop;
      
      DB_Logger.info( "Personal_Results_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Personal_Results_Test_Item := Personal_Results_List.element( Personal_Results_test_list, i );
         Personal_Results_io.Delete( Personal_Results_Test_Item );         
      end loop;
      
      DB_Logger.info( "Personal_Results_Create_Test: retrieve all records" );
      Personal_Results_List.iterate( Personal_Results_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Personal_Results_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Personal_Results_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Personal_Results_Create_Test : exception thrown " & Exception_Information(Error) );
   end Personal_Results_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Personal_Income_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Personal_Income_List.Cursor ) is 
      Personal_Income_Test_Item : Wsc_Db_Data.Personal_Income;
      begin
         Personal_Income_Test_Item := Personal_Income_List.element( pos );
         DB_Logger.info( To_String( Personal_Income_Test_Item ));
      end print;

   
      Personal_Income_Test_Item : Wsc_Db_Data.Personal_Income;
      Personal_Income_test_list : Wsc_Db_Data.Personal_Income_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Personal_Income_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Personal_Income_io.Delete( criteria );
      
      DB_Logger.info( "Personal_Income_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Personal_Income_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         Personal_Income_Test_Item.Run_Id := Personal_Income_io.Next_Free_Run_Id;
         Personal_Income_Test_Item.Pid := Personal_Income_io.Next_Free_Pid;
         Personal_Income_Test_Item.Sysno := Personal_Income_io.Next_Free_Sysno;
         Personal_Income_Test_Item.Iteration := Personal_Income_io.Next_Free_Iteration;
         Personal_Income_Test_Item.Wave := To_Unbounded_String( "k_" & i'Img );
         Personal_Income_Test_Item.Wave := To_Unbounded_String( "k_" & i'Img );
         -- missingPersonal_Income_Test_Item declaration ;
         -- missingPersonal_Income_Test_Item declaration ;
         -- missingPersonal_Income_Test_Item declaration ;
         Personal_Income_Test_Item.Value := 1010100.012;
         Personal_Income_io.Save( Personal_Income_Test_Item, False );         
      end loop;
      
      Personal_Income_test_list := Personal_Income_Io.Retrieve( criteria );
      
      DB_Logger.info( "Personal_Income_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Personal_Income_Test_Item := Personal_Income_List.element( Personal_Income_test_list, i );
         Personal_Income_io.Save( Personal_Income_Test_Item );         
      end loop;
      
      DB_Logger.info( "Personal_Income_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Personal_Income_Test_Item := Personal_Income_List.element( Personal_Income_test_list, i );
         Personal_Income_io.Delete( Personal_Income_Test_Item );         
      end loop;
      
      DB_Logger.info( "Personal_Income_Create_Test: retrieve all records" );
      Personal_Income_List.iterate( Personal_Income_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Personal_Income_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Personal_Income_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Personal_Income_Create_Test : exception thrown " & Exception_Information(Error) );
   end Personal_Income_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Disaggregated_Data_Table_Description_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Disaggregated_Data_Table_Description_List.Cursor ) is 
      Disaggregated_Data_Table_Description_Test_Item : Wsc_Db_Data.Disaggregated_Data_Table_Description;
      begin
         Disaggregated_Data_Table_Description_Test_Item := Disaggregated_Data_Table_Description_List.element( pos );
         DB_Logger.info( To_String( Disaggregated_Data_Table_Description_Test_Item ));
      end print;

   
      Disaggregated_Data_Table_Description_Test_Item : Wsc_Db_Data.Disaggregated_Data_Table_Description;
      Disaggregated_Data_Table_Description_test_list : Wsc_Db_Data.Disaggregated_Data_Table_Description_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Disaggregated_Data_Table_Description_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Disaggregated_Data_Table_Description_io.Delete( criteria );
      
      DB_Logger.info( "Disaggregated_Data_Table_Description_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Disaggregated_Data_Table_Description_Test_Item.Model_Table_Name := To_Unbounded_String( "k_" & i'Img );
         Disaggregated_Data_Table_Description_Test_Item.Filename := To_Unbounded_String("dat forFilename");
         -- missingDisaggregated_Data_Table_Description_Test_Item declaration ;
         Disaggregated_Data_Table_Description_Test_Item.Table_Type := To_Unbounded_String("dat forTable_Type");
         Disaggregated_Data_Table_Description_Test_Item.Table_Subtype := To_Unbounded_String("dat forTable_Subtype");
         Disaggregated_Data_Table_Description_io.Save( Disaggregated_Data_Table_Description_Test_Item, False );         
      end loop;
      
      Disaggregated_Data_Table_Description_test_list := Disaggregated_Data_Table_Description_Io.Retrieve( criteria );
      
      DB_Logger.info( "Disaggregated_Data_Table_Description_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Disaggregated_Data_Table_Description_Test_Item := Disaggregated_Data_Table_Description_List.element( Disaggregated_Data_Table_Description_test_list, i );
         Disaggregated_Data_Table_Description_Test_Item.Filename := To_Unbounded_String("Altered::dat forFilename");
         Disaggregated_Data_Table_Description_Test_Item.Table_Type := To_Unbounded_String("Altered::dat forTable_Type");
         Disaggregated_Data_Table_Description_Test_Item.Table_Subtype := To_Unbounded_String("Altered::dat forTable_Subtype");
         Disaggregated_Data_Table_Description_io.Save( Disaggregated_Data_Table_Description_Test_Item );         
      end loop;
      
      DB_Logger.info( "Disaggregated_Data_Table_Description_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Disaggregated_Data_Table_Description_Test_Item := Disaggregated_Data_Table_Description_List.element( Disaggregated_Data_Table_Description_test_list, i );
         Disaggregated_Data_Table_Description_io.Delete( Disaggregated_Data_Table_Description_Test_Item );         
      end loop;
      
      DB_Logger.info( "Disaggregated_Data_Table_Description_Create_Test: retrieve all records" );
      Disaggregated_Data_Table_Description_List.iterate( Disaggregated_Data_Table_Description_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Disaggregated_Data_Table_Description_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Disaggregated_Data_Table_Description_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Disaggregated_Data_Table_Description_Create_Test : exception thrown " & Exception_Information(Error) );
   end Disaggregated_Data_Table_Description_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Disaggregated_Data_Table_Cell_Description_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Disaggregated_Data_Table_Cell_Description_List.Cursor ) is 
      Disaggregated_Data_Table_Cell_Description_Test_Item : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description;
      begin
         Disaggregated_Data_Table_Cell_Description_Test_Item := Disaggregated_Data_Table_Cell_Description_List.element( pos );
         DB_Logger.info( To_String( Disaggregated_Data_Table_Cell_Description_Test_Item ));
      end print;

   
      Disaggregated_Data_Table_Cell_Description_Test_Item : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description;
      Disaggregated_Data_Table_Cell_Description_test_list : Wsc_Db_Data.Disaggregated_Data_Table_Cell_Description_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Disaggregated_Data_Table_Cell_Description_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Disaggregated_Data_Table_Cell_Description_io.Delete( criteria );
      
      DB_Logger.info( "Disaggregated_Data_Table_Cell_Description_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Disaggregated_Data_Table_Cell_Description_Test_Item.Model_Table_Name := To_Unbounded_String( "k_" & i'Img );
         Disaggregated_Data_Table_Cell_Description_Test_Item.Cell_Pos := Disaggregated_Data_Table_Cell_Description_io.Next_Free_Cell_Pos;
         Disaggregated_Data_Table_Cell_Description_Test_Item.Cell_Label := To_Unbounded_String("dat forCell_Label");
         Disaggregated_Data_Table_Cell_Description_Test_Item.Cell_Type := To_Unbounded_String("dat forCell_Type");
         -- missingDisaggregated_Data_Table_Cell_Description_Test_Item declaration ;
         Disaggregated_Data_Table_Cell_Description_io.Save( Disaggregated_Data_Table_Cell_Description_Test_Item, False );         
      end loop;
      
      Disaggregated_Data_Table_Cell_Description_test_list := Disaggregated_Data_Table_Cell_Description_Io.Retrieve( criteria );
      
      DB_Logger.info( "Disaggregated_Data_Table_Cell_Description_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Disaggregated_Data_Table_Cell_Description_Test_Item := Disaggregated_Data_Table_Cell_Description_List.element( Disaggregated_Data_Table_Cell_Description_test_list, i );
         Disaggregated_Data_Table_Cell_Description_Test_Item.Cell_Label := To_Unbounded_String("Altered::dat forCell_Label");
         Disaggregated_Data_Table_Cell_Description_Test_Item.Cell_Type := To_Unbounded_String("Altered::dat forCell_Type");
         Disaggregated_Data_Table_Cell_Description_io.Save( Disaggregated_Data_Table_Cell_Description_Test_Item );         
      end loop;
      
      DB_Logger.info( "Disaggregated_Data_Table_Cell_Description_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Disaggregated_Data_Table_Cell_Description_Test_Item := Disaggregated_Data_Table_Cell_Description_List.element( Disaggregated_Data_Table_Cell_Description_test_list, i );
         Disaggregated_Data_Table_Cell_Description_io.Delete( Disaggregated_Data_Table_Cell_Description_Test_Item );         
      end loop;
      
      DB_Logger.info( "Disaggregated_Data_Table_Cell_Description_Create_Test: retrieve all records" );
      Disaggregated_Data_Table_Cell_Description_List.iterate( Disaggregated_Data_Table_Cell_Description_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Disaggregated_Data_Table_Cell_Description_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Disaggregated_Data_Table_Cell_Description_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Disaggregated_Data_Table_Cell_Description_Create_Test : exception thrown " & Exception_Information(Error) );
   end Disaggregated_Data_Table_Cell_Description_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Disaggregated_Data_Table_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Disaggregated_Data_Table_List.Cursor ) is 
      Disaggregated_Data_Table_Test_Item : Wsc_Db_Data.Disaggregated_Data_Table;
      begin
         Disaggregated_Data_Table_Test_Item := Disaggregated_Data_Table_List.element( pos );
         DB_Logger.info( To_String( Disaggregated_Data_Table_Test_Item ));
      end print;

   
      Disaggregated_Data_Table_Test_Item : Wsc_Db_Data.Disaggregated_Data_Table;
      Disaggregated_Data_Table_test_list : Wsc_Db_Data.Disaggregated_Data_Table_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Disaggregated_Data_Table_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Disaggregated_Data_Table_io.Delete( criteria );
      
      DB_Logger.info( "Disaggregated_Data_Table_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Disaggregated_Data_Table_Test_Item.Run_Id := Disaggregated_Data_Table_io.Next_Free_Run_Id;
         Disaggregated_Data_Table_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         Disaggregated_Data_Table_Test_Item.Model_Table_Name := To_Unbounded_String( "k_" & i'Img );
         Disaggregated_Data_Table_Test_Item.Iteration := Disaggregated_Data_Table_io.Next_Free_Iteration;
         Disaggregated_Data_Table_io.Save( Disaggregated_Data_Table_Test_Item, False );         
      end loop;
      
      Disaggregated_Data_Table_test_list := Disaggregated_Data_Table_Io.Retrieve( criteria );
      
      DB_Logger.info( "Disaggregated_Data_Table_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Disaggregated_Data_Table_Test_Item := Disaggregated_Data_Table_List.element( Disaggregated_Data_Table_test_list, i );
         Disaggregated_Data_Table_io.Save( Disaggregated_Data_Table_Test_Item );         
      end loop;
      
      DB_Logger.info( "Disaggregated_Data_Table_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Disaggregated_Data_Table_Test_Item := Disaggregated_Data_Table_List.element( Disaggregated_Data_Table_test_list, i );
         Disaggregated_Data_Table_io.Delete( Disaggregated_Data_Table_Test_Item );         
      end loop;
      
      DB_Logger.info( "Disaggregated_Data_Table_Create_Test: retrieve all records" );
      Disaggregated_Data_Table_List.iterate( Disaggregated_Data_Table_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Disaggregated_Data_Table_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Disaggregated_Data_Table_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Disaggregated_Data_Table_Create_Test : exception thrown " & Exception_Information(Error) );
   end Disaggregated_Data_Table_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Disaggregated_Data_Table_Cell_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Disaggregated_Data_Table_Cell_List.Cursor ) is 
      Disaggregated_Data_Table_Cell_Test_Item : Wsc_Db_Data.Disaggregated_Data_Table_Cell;
      begin
         Disaggregated_Data_Table_Cell_Test_Item := Disaggregated_Data_Table_Cell_List.element( pos );
         DB_Logger.info( To_String( Disaggregated_Data_Table_Cell_Test_Item ));
      end print;

   
      Disaggregated_Data_Table_Cell_Test_Item : Wsc_Db_Data.Disaggregated_Data_Table_Cell;
      Disaggregated_Data_Table_Cell_test_list : Wsc_Db_Data.Disaggregated_Data_Table_Cell_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Disaggregated_Data_Table_Cell_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Disaggregated_Data_Table_Cell_io.Delete( criteria );
      
      DB_Logger.info( "Disaggregated_Data_Table_Cell_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Disaggregated_Data_Table_Cell_Test_Item.Run_Id := Disaggregated_Data_Table_Cell_io.Next_Free_Run_Id;
         Disaggregated_Data_Table_Cell_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         Disaggregated_Data_Table_Cell_Test_Item.Model_Table_Name := To_Unbounded_String( "k_" & i'Img );
         Disaggregated_Data_Table_Cell_Test_Item.Row_Num := Disaggregated_Data_Table_Cell_io.Next_Free_Row_Num;
         Disaggregated_Data_Table_Cell_Test_Item.Col_Num := Disaggregated_Data_Table_Cell_io.Next_Free_Col_Num;
         Disaggregated_Data_Table_Cell_Test_Item.Wave := To_Unbounded_String( "k_" & i'Img );
         Disaggregated_Data_Table_Cell_Test_Item.Iteration := Disaggregated_Data_Table_Cell_io.Next_Free_Iteration;
         Disaggregated_Data_Table_Cell_Test_Item.System_Number := Disaggregated_Data_Table_Cell_io.Next_Free_System_Number;
         Disaggregated_Data_Table_Cell_Test_Item.Value1 := 1010100.012;
         Disaggregated_Data_Table_Cell_Test_Item.Value2 := 1010100.012;
         Disaggregated_Data_Table_Cell_Test_Item.Value3 := 1010100.012;
         Disaggregated_Data_Table_Cell_Test_Item.Value4 := 1010100.012;
         Disaggregated_Data_Table_Cell_Test_Item.Value5 := 1010100.012;
         Disaggregated_Data_Table_Cell_Test_Item.Value6 := 1010100.012;
         -- missingDisaggregated_Data_Table_Cell_Test_Item declaration ;
         -- missingDisaggregated_Data_Table_Cell_Test_Item declaration ;
         -- missingDisaggregated_Data_Table_Cell_Test_Item declaration ;
         -- missingDisaggregated_Data_Table_Cell_Test_Item declaration ;
         Disaggregated_Data_Table_Cell_io.Save( Disaggregated_Data_Table_Cell_Test_Item, False );         
      end loop;
      
      Disaggregated_Data_Table_Cell_test_list := Disaggregated_Data_Table_Cell_Io.Retrieve( criteria );
      
      DB_Logger.info( "Disaggregated_Data_Table_Cell_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Disaggregated_Data_Table_Cell_Test_Item := Disaggregated_Data_Table_Cell_List.element( Disaggregated_Data_Table_Cell_test_list, i );
         Disaggregated_Data_Table_Cell_io.Save( Disaggregated_Data_Table_Cell_Test_Item );         
      end loop;
      
      DB_Logger.info( "Disaggregated_Data_Table_Cell_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Disaggregated_Data_Table_Cell_Test_Item := Disaggregated_Data_Table_Cell_List.element( Disaggregated_Data_Table_Cell_test_list, i );
         Disaggregated_Data_Table_Cell_io.Delete( Disaggregated_Data_Table_Cell_Test_Item );         
      end loop;
      
      DB_Logger.info( "Disaggregated_Data_Table_Cell_Create_Test: retrieve all records" );
      Disaggregated_Data_Table_Cell_List.iterate( Disaggregated_Data_Table_Cell_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Disaggregated_Data_Table_Cell_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Disaggregated_Data_Table_Cell_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Disaggregated_Data_Table_Cell_Create_Test : exception thrown " & Exception_Information(Error) );
   end Disaggregated_Data_Table_Cell_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Maxima_And_Totals_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Maxima_And_Totals_List.Cursor ) is 
      Maxima_And_Totals_Test_Item : Wsc_Db_Data.Maxima_And_Totals;
      begin
         Maxima_And_Totals_Test_Item := Maxima_And_Totals_List.element( pos );
         DB_Logger.info( To_String( Maxima_And_Totals_Test_Item ));
      end print;

   
      Maxima_And_Totals_Test_Item : Wsc_Db_Data.Maxima_And_Totals;
      Maxima_And_Totals_test_list : Wsc_Db_Data.Maxima_And_Totals_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Maxima_And_Totals_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Maxima_And_Totals_io.Delete( criteria );
      
      DB_Logger.info( "Maxima_And_Totals_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Maxima_And_Totals_Test_Item.Lifetime_La_Contributions := 1010100.012;
         Maxima_And_Totals_Test_Item.Lifetime_Client_Contributions := 1010100.012;
         Maxima_And_Totals_Test_Item.Lifetime_Gross_Payments := 1010100.012;
         Maxima_And_Totals_Test_Item.Lifetime_Capital_Contributions := 1010100.012;
         Maxima_And_Totals_Test_Item.Highest_La_Contribution := 1010100.012;
         Maxima_And_Totals_io.Save( Maxima_And_Totals_Test_Item, False );         
      end loop;
      
      Maxima_And_Totals_test_list := Maxima_And_Totals_Io.Retrieve( criteria );
      
      DB_Logger.info( "Maxima_And_Totals_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Maxima_And_Totals_Test_Item := Maxima_And_Totals_List.element( Maxima_And_Totals_test_list, i );
         Maxima_And_Totals_io.Save( Maxima_And_Totals_Test_Item );         
      end loop;
      
      DB_Logger.info( "Maxima_And_Totals_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Maxima_And_Totals_Test_Item := Maxima_And_Totals_List.element( Maxima_And_Totals_test_list, i );
         Maxima_And_Totals_io.Delete( Maxima_And_Totals_Test_Item );         
      end loop;
      
      DB_Logger.info( "Maxima_And_Totals_Create_Test: retrieve all records" );
      Maxima_And_Totals_List.iterate( Maxima_And_Totals_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Maxima_And_Totals_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Maxima_And_Totals_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Maxima_And_Totals_Create_Test : exception thrown " & Exception_Information(Error) );
   end Maxima_And_Totals_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Uap_Threshold_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Uap_Threshold_List.Cursor ) is 
      Uap_Threshold_Test_Item : Wsc_Db_Data.Uap_Threshold;
      begin
         Uap_Threshold_Test_Item := Uap_Threshold_List.element( pos );
         DB_Logger.info( To_String( Uap_Threshold_Test_Item ));
      end print;

   
      Uap_Threshold_Test_Item : Wsc_Db_Data.Uap_Threshold;
      Uap_Threshold_test_list : Wsc_Db_Data.Uap_Threshold_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Uap_Threshold_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Uap_Threshold_io.Delete( criteria );
      
      DB_Logger.info( "Uap_Threshold_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Uap_Threshold_Test_Item.Run_Id := Uap_Threshold_io.Next_Free_Run_Id;
         Uap_Threshold_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         Uap_Threshold_Test_Item.Sysno := Uap_Threshold_io.Next_Free_Sysno;
         Uap_Threshold_Test_Item.Iteration := Uap_Threshold_io.Next_Free_Iteration;
         Uap_Threshold_Test_Item.Wave := To_Unbounded_String( "k_" & i'Img );
         Uap_Threshold_Test_Item.Wave := To_Unbounded_String( "k_" & i'Img );
         Uap_Threshold_Test_Item.Threshold := 1010100.012;
         Uap_Threshold_io.Save( Uap_Threshold_Test_Item, False );         
      end loop;
      
      Uap_Threshold_test_list := Uap_Threshold_Io.Retrieve( criteria );
      
      DB_Logger.info( "Uap_Threshold_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Uap_Threshold_Test_Item := Uap_Threshold_List.element( Uap_Threshold_test_list, i );
         Uap_Threshold_io.Save( Uap_Threshold_Test_Item );         
      end loop;
      
      DB_Logger.info( "Uap_Threshold_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Uap_Threshold_Test_Item := Uap_Threshold_List.element( Uap_Threshold_test_list, i );
         Uap_Threshold_io.Delete( Uap_Threshold_Test_Item );         
      end loop;
      
      DB_Logger.info( "Uap_Threshold_Create_Test: retrieve all records" );
      Uap_Threshold_List.iterate( Uap_Threshold_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Uap_Threshold_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Uap_Threshold_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Uap_Threshold_Create_Test : exception thrown " & Exception_Information(Error) );
   end Uap_Threshold_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Table_Stats_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Table_Stats_List.Cursor ) is 
      Table_Stats_Test_Item : Wsc_Db_Data.Table_Stats;
      begin
         Table_Stats_Test_Item := Table_Stats_List.element( pos );
         DB_Logger.info( To_String( Table_Stats_Test_Item ));
      end print;

   
      Table_Stats_Test_Item : Wsc_Db_Data.Table_Stats;
      Table_Stats_test_list : Wsc_Db_Data.Table_Stats_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Table_Stats_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Table_Stats_io.Delete( criteria );
      
      DB_Logger.info( "Table_Stats_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Table_Stats_Test_Item.Run_Id := Table_Stats_io.Next_Free_Run_Id;
         Table_Stats_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         Table_Stats_Test_Item.Model_Table_Name := To_Unbounded_String( "k_" & i'Img );
         Table_Stats_Test_Item.Row_Num := Table_Stats_io.Next_Free_Row_Num;
         Table_Stats_Test_Item.Col_Num := Table_Stats_io.Next_Free_Col_Num;
         Table_Stats_Test_Item.Wave := To_Unbounded_String( "k_" & i'Img );
         Table_Stats_Test_Item.System_Number := Table_Stats_io.Next_Free_System_Number;
         -- missingTable_Stats_Test_Item declaration ;
         Table_Stats_Test_Item.Rmean_1 := 1010100.012;
         Table_Stats_Test_Item.Rmin_1 := 1010100.012;
         Table_Stats_Test_Item.Rmax_1 := 1010100.012;
         Table_Stats_Test_Item.Rmed_1 := 1010100.012;
         Table_Stats_Test_Item.Sddev_1 := 1010100.012;
         Table_Stats_Test_Item.Dec1_1 := 1010100.012;
         Table_Stats_Test_Item.Dec10_1 := 1010100.012;
         Table_Stats_Test_Item.Rmean_2 := 1010100.012;
         Table_Stats_Test_Item.Rmin_2 := 1010100.012;
         Table_Stats_Test_Item.Rmax_2 := 1010100.012;
         Table_Stats_Test_Item.Rmed_2 := 1010100.012;
         Table_Stats_Test_Item.Sddev_2 := 1010100.012;
         Table_Stats_Test_Item.Dec1_2 := 1010100.012;
         Table_Stats_Test_Item.Dec10_2 := 1010100.012;
         Table_Stats_Test_Item.Rmean_3 := 1010100.012;
         Table_Stats_Test_Item.Rmin_3 := 1010100.012;
         Table_Stats_Test_Item.Rmax_3 := 1010100.012;
         Table_Stats_Test_Item.Rmed_3 := 1010100.012;
         Table_Stats_Test_Item.Sddev_3 := 1010100.012;
         Table_Stats_Test_Item.Dec1_3 := 1010100.012;
         Table_Stats_Test_Item.Dec10_3 := 1010100.012;
         Table_Stats_Test_Item.Rmean_4 := 1010100.012;
         Table_Stats_Test_Item.Rmin_4 := 1010100.012;
         Table_Stats_Test_Item.Rmax_4 := 1010100.012;
         Table_Stats_Test_Item.Rmed_4 := 1010100.012;
         Table_Stats_Test_Item.Sddev_4 := 1010100.012;
         Table_Stats_Test_Item.Dec1_4 := 1010100.012;
         Table_Stats_Test_Item.Dec10_4 := 1010100.012;
         Table_Stats_Test_Item.Rmean_5 := 1010100.012;
         Table_Stats_Test_Item.Rmin_5 := 1010100.012;
         Table_Stats_Test_Item.Rmax_5 := 1010100.012;
         Table_Stats_Test_Item.Rmed_5 := 1010100.012;
         Table_Stats_Test_Item.Sddev_5 := 1010100.012;
         Table_Stats_Test_Item.Dec1_5 := 1010100.012;
         Table_Stats_Test_Item.Dec10_5 := 1010100.012;
         Table_Stats_Test_Item.Rmean_6 := 1010100.012;
         Table_Stats_Test_Item.Rmin_6 := 1010100.012;
         Table_Stats_Test_Item.Rmax_6 := 1010100.012;
         Table_Stats_Test_Item.Rmed_6 := 1010100.012;
         Table_Stats_Test_Item.Sddev_6 := 1010100.012;
         Table_Stats_Test_Item.Dec1_6 := 1010100.012;
         Table_Stats_Test_Item.Dec10_6 := 1010100.012;
         Table_Stats_io.Save( Table_Stats_Test_Item, False );         
      end loop;
      
      Table_Stats_test_list := Table_Stats_Io.Retrieve( criteria );
      
      DB_Logger.info( "Table_Stats_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Table_Stats_Test_Item := Table_Stats_List.element( Table_Stats_test_list, i );
         Table_Stats_io.Save( Table_Stats_Test_Item );         
      end loop;
      
      DB_Logger.info( "Table_Stats_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Table_Stats_Test_Item := Table_Stats_List.element( Table_Stats_test_list, i );
         Table_Stats_io.Delete( Table_Stats_Test_Item );         
      end loop;
      
      DB_Logger.info( "Table_Stats_Create_Test: retrieve all records" );
      Table_Stats_List.iterate( Table_Stats_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Table_Stats_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Table_Stats_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Table_Stats_Create_Test : exception thrown " & Exception_Information(Error) );
   end Table_Stats_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Household_Capital_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Household_Capital_List.Cursor ) is 
      Household_Capital_Test_Item : Wsc_Db_Data.Household_Capital;
      begin
         Household_Capital_Test_Item := Household_Capital_List.element( pos );
         DB_Logger.info( To_String( Household_Capital_Test_Item ));
      end print;

   
      Household_Capital_Test_Item : Wsc_Db_Data.Household_Capital;
      Household_Capital_test_list : Wsc_Db_Data.Household_Capital_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Household_Capital_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Household_Capital_io.Delete( criteria );
      
      DB_Logger.info( "Household_Capital_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Household_Capital_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         Household_Capital_Test_Item.Run_Id := Household_Capital_io.Next_Free_Run_Id;
         Household_Capital_Test_Item.Sysno := Household_Capital_io.Next_Free_Sysno;
         Household_Capital_Test_Item.Iteration := Household_Capital_io.Next_Free_Iteration;
         Household_Capital_Test_Item.Hid := Household_Capital_io.Next_Free_Hid;
         Household_Capital_Test_Item.Wave := To_Unbounded_String( "k_" & i'Img );
         Household_Capital_Test_Item.Capital_Stock := 1010100.012;
         Household_Capital_io.Save( Household_Capital_Test_Item, False );         
      end loop;
      
      Household_Capital_test_list := Household_Capital_Io.Retrieve( criteria );
      
      DB_Logger.info( "Household_Capital_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Household_Capital_Test_Item := Household_Capital_List.element( Household_Capital_test_list, i );
         Household_Capital_io.Save( Household_Capital_Test_Item );         
      end loop;
      
      DB_Logger.info( "Household_Capital_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Household_Capital_Test_Item := Household_Capital_List.element( Household_Capital_test_list, i );
         Household_Capital_io.Delete( Household_Capital_Test_Item );         
      end loop;
      
      DB_Logger.info( "Household_Capital_Create_Test: retrieve all records" );
      Household_Capital_List.iterate( Household_Capital_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Household_Capital_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Household_Capital_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Household_Capital_Create_Test : exception thrown " & Exception_Information(Error) );
   end Household_Capital_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Gain_Lose_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Gain_Lose_List.Cursor ) is 
      Gain_Lose_Test_Item : Wsc_Db_Data.Gain_Lose;
      begin
         Gain_Lose_Test_Item := Gain_Lose_List.element( pos );
         DB_Logger.info( To_String( Gain_Lose_Test_Item ));
      end print;

   
      Gain_Lose_Test_Item : Wsc_Db_Data.Gain_Lose;
      Gain_Lose_test_list : Wsc_Db_Data.Gain_Lose_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Gain_Lose_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Gain_Lose_io.Delete( criteria );
      
      DB_Logger.info( "Gain_Lose_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Gain_Lose_Test_Item.Username := To_Unbounded_String( "k_" & i'Img );
         Gain_Lose_Test_Item.Run_Id := Gain_Lose_io.Next_Free_Run_Id;
         Gain_Lose_Test_Item.Iteration := Gain_Lose_io.Next_Free_Iteration;
         Gain_Lose_Test_Item.Pid := Gain_Lose_io.Next_Free_Pid;
         Gain_Lose_Test_Item.Wave := To_Unbounded_String( "k_" & i'Img );
         -- missingGain_Lose_Test_Item declaration ;
         Gain_Lose_Test_Item.Pre := 1010100.012;
         Gain_Lose_Test_Item.Post := 1010100.012;
         Gain_Lose_io.Save( Gain_Lose_Test_Item, False );         
      end loop;
      
      Gain_Lose_test_list := Gain_Lose_Io.Retrieve( criteria );
      
      DB_Logger.info( "Gain_Lose_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Gain_Lose_Test_Item := Gain_Lose_List.element( Gain_Lose_test_list, i );
         Gain_Lose_io.Save( Gain_Lose_Test_Item );         
      end loop;
      
      DB_Logger.info( "Gain_Lose_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Gain_Lose_Test_Item := Gain_Lose_List.element( Gain_Lose_test_list, i );
         Gain_Lose_io.Delete( Gain_Lose_Test_Item );         
      end loop;
      
      DB_Logger.info( "Gain_Lose_Create_Test: retrieve all records" );
      Gain_Lose_List.iterate( Gain_Lose_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Gain_Lose_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Gain_Lose_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Gain_Lose_Create_Test : exception thrown " & Exception_Information(Error) );
   end Gain_Lose_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Household_Data_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Household_Data_List.Cursor ) is 
      Household_Data_Test_Item : Wsc_Db_Data.Household_Data;
      begin
         Household_Data_Test_Item := Household_Data_List.element( pos );
         DB_Logger.info( To_String( Household_Data_Test_Item ));
      end print;

   
      Household_Data_Test_Item : Wsc_Db_Data.Household_Data;
      Household_Data_test_list : Wsc_Db_Data.Household_Data_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Household_Data_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Household_Data_io.Delete( criteria );
      
      DB_Logger.info( "Household_Data_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Household_Data_Test_Item.Dataset_Name := To_Unbounded_String( "k_" & i'Img );
         Household_Data_Test_Item.Iteration := Household_Data_io.Next_Free_Iteration;
         Household_Data_Test_Item.Wave := To_Unbounded_String( "k_" & i'Img );
         Household_Data_Test_Item.Hid := Household_Data_io.Next_Free_Hid;
         Household_Data_Test_Item.Origin_Hid := Household_Data_io.Next_Free_Origin_Hid;
         Household_Data_Test_Item.Interview_Date := Ada.Calendar.Clock;
         Household_Data_Test_Item.Current_Simulated_Date := Ada.Calendar.Clock;
         -- missingHousehold_Data_Test_Item declaration ;
         -- missingHousehold_Data_Test_Item declaration ;
         Household_Data_Test_Item.Gross_Rent := 1010100.012;
         Household_Data_Test_Item.Net_Rent := 1010100.012;
         Household_Data_Test_Item.Mortgage_Outstanding := 1010100.012;
         Household_Data_Test_Item.Gross_Housing_Costs := 1010100.012;
         Household_Data_Test_Item.Net_Housing_Costs := 1010100.012;
         Household_Data_Test_Item.Total_Income := 1010100.012;
         Household_Data_Test_Item.House_Value := 1010100.012;
         Household_Data_Test_Item.Other_Property_Value := 1010100.012;
         Household_Data_Test_Item.Mortgage_Payment := 1010100.012;
         -- missingHousehold_Data_Test_Item declaration ;
         -- missingHousehold_Data_Test_Item declaration ;
         Household_Data_Test_Item.Weight_Basic := 1010100.012;
         Household_Data_Test_Item.Weight_Extended_1 := 1010100.012;
         Household_Data_Test_Item.Weight_Extended_2 := 1010100.012;
         -- missingHousehold_Data_Test_Item declaration ;
         -- missingHousehold_Data_Test_Item declaration ;
         Household_Data_io.Save( Household_Data_Test_Item, False );         
      end loop;
      
      Household_Data_test_list := Household_Data_Io.Retrieve( criteria );
      
      DB_Logger.info( "Household_Data_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Household_Data_Test_Item := Household_Data_List.element( Household_Data_test_list, i );
         Household_Data_io.Save( Household_Data_Test_Item );         
      end loop;
      
      DB_Logger.info( "Household_Data_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Household_Data_Test_Item := Household_Data_List.element( Household_Data_test_list, i );
         Household_Data_io.Delete( Household_Data_Test_Item );         
      end loop;
      
      DB_Logger.info( "Household_Data_Create_Test: retrieve all records" );
      Household_Data_List.iterate( Household_Data_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Household_Data_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Household_Data_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Household_Data_Create_Test : exception thrown " & Exception_Information(Error) );
   end Household_Data_Create_Test;

   
--
-- test creating and deleting records  
--
--
   procedure Person_Create_Test(  T : in out AUnit.Test_Cases.Test_Case'Class ) is
      --
      -- local print iteration routine
      --
      procedure Print( pos : Person_List.Cursor ) is 
      Person_Test_Item : Wsc_Db_Data.Person;
      begin
         Person_Test_Item := Person_List.element( pos );
         DB_Logger.info( To_String( Person_Test_Item ));
      end print;

   
      Person_Test_Item : Wsc_Db_Data.Person;
      Person_test_list : Wsc_Db_Data.Person_List.Vector;
      criteria  : d.Criteria;
      startTime : Time;
      endTime   : Time;
      elapsed   : Duration;
   begin
      startTime := Clock;
      DB_Logger.info( "Starting test Person_Create_Test" );
      
      DB_Logger.info( "Clearing out the table" );
      Person_io.Delete( criteria );
      
      DB_Logger.info( "Person_Create_Test: create tests" );
      for i in 1 .. RECORDS_TO_ADD loop
         Person_Test_Item.Dataset_Name := To_Unbounded_String( "k_" & i'Img );
         Person_Test_Item.Iteration := Person_io.Next_Free_Iteration;
         Person_Test_Item.Wave := To_Unbounded_String( "k_" & i'Img );
         Person_Test_Item.Hid := Person_io.Next_Free_Hid;
         Person_Test_Item.Pid := Person_io.Next_Free_Pid;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         Person_Test_Item.Activities_Of_Daily_Living_Score := 1010100.012;
         Person_Test_Item.Health_Score := 1010100.012;
         -- missingPerson_Test_Item declaration ;
         Person_Test_Item.Respondent_Weight_Basic := 1010100.012;
         Person_Test_Item.Respondent_Weight_Extended_1 := 1010100.012;
         Person_Test_Item.Respondent_Weight_Extended_2 := 1010100.012;
         Person_Test_Item.Enumeration_Weight_Basic := 1010100.012;
         Person_Test_Item.Enumeration_Weight_Extended_1 := 1010100.012;
         Person_Test_Item.Enumeration_Weight_Extended_2 := 1010100.012;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         Person_Test_Item.Base_Personal_Wealth := 1010100.012;
         Person_Test_Item.Personal_Wealth := 1010100.012;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         -- missingPerson_Test_Item declaration ;
         Person_io.Save( Person_Test_Item, False );         
      end loop;
      
      Person_test_list := Person_Io.Retrieve( criteria );
      
      DB_Logger.info( "Person_Create_Test: alter tests" );
      for i in 1 .. RECORDS_TO_ALTER loop
         Person_Test_Item := Person_List.element( Person_test_list, i );
         Person_io.Save( Person_Test_Item );         
      end loop;
      
      DB_Logger.info( "Person_Create_Test: delete tests" );
      for i in RECORDS_TO_DELETE .. RECORDS_TO_ADD loop
         Person_Test_Item := Person_List.element( Person_test_list, i );
         Person_io.Delete( Person_Test_Item );         
      end loop;
      
      DB_Logger.info( "Person_Create_Test: retrieve all records" );
      Person_List.iterate( Person_test_list, print'Access );
      endTime := Clock;
      elapsed := endTime - startTime;
      DB_Logger.info( "Ending test Person_Create_Test. Time taken = " & elapsed'Img );

   exception 
      when Error : others =>
         DB_Logger.error( "Person_Create_Test execute query failed with message " & Exception_Information(Error) );
         assert( False,  
            "Person_Create_Test : exception thrown " & Exception_Information(Error) );
   end Person_Create_Test;

   
   
   
   
   
   
   
   
   
   procedure Register_Tests (T : in out Test_Case) is
   begin
      --
      -- Tests of record creation/deletion
      --
      Register_Routine (T, User_Type_Create_Test'Access, "Test of Creation and deletion of User_Type" );
      Register_Routine (T, Dataset_Create_Test'Access, "Test of Creation and deletion of Dataset" );
      Register_Routine (T, Run_Create_Test'Access, "Test of Creation and deletion of Run" );
      Register_Routine (T, State_Create_Test'Access, "Test of Creation and deletion of State" );
      Register_Routine (T, Key_Value_Parameter_Create_Test'Access, "Test of Creation and deletion of Key_Value_Parameter" );
      Register_Routine (T, Uprate_Assumption_Create_Test'Access, "Test of Creation and deletion of Uprate_Assumption" );
      Register_Routine (T, Probit_Threshold_Create_Test'Access, "Test of Creation and deletion of Probit_Threshold" );
      Register_Routine (T, Personal_Results_Create_Test'Access, "Test of Creation and deletion of Personal_Results" );
      Register_Routine (T, Personal_Income_Create_Test'Access, "Test of Creation and deletion of Personal_Income" );
      Register_Routine (T, Disaggregated_Data_Table_Description_Create_Test'Access, "Test of Creation and deletion of Disaggregated_Data_Table_Description" );
      Register_Routine (T, Disaggregated_Data_Table_Cell_Description_Create_Test'Access, "Test of Creation and deletion of Disaggregated_Data_Table_Cell_Description" );
      Register_Routine (T, Disaggregated_Data_Table_Create_Test'Access, "Test of Creation and deletion of Disaggregated_Data_Table" );
      Register_Routine (T, Disaggregated_Data_Table_Cell_Create_Test'Access, "Test of Creation and deletion of Disaggregated_Data_Table_Cell" );
      Register_Routine (T, Maxima_And_Totals_Create_Test'Access, "Test of Creation and deletion of Maxima_And_Totals" );
      Register_Routine (T, Uap_Threshold_Create_Test'Access, "Test of Creation and deletion of Uap_Threshold" );
      Register_Routine (T, Table_Stats_Create_Test'Access, "Test of Creation and deletion of Table_Stats" );
      Register_Routine (T, Household_Capital_Create_Test'Access, "Test of Creation and deletion of Household_Capital" );
      Register_Routine (T, Gain_Lose_Create_Test'Access, "Test of Creation and deletion of Gain_Lose" );
      Register_Routine (T, Household_Data_Create_Test'Access, "Test of Creation and deletion of Household_Data" );
      Register_Routine (T, Person_Create_Test'Access, "Test of Creation and deletion of Person" );
      --
      -- Tests of foreign key relationships
      --
      --  not implemented yet Register_Routine (T, User_Type_Child_Retrieve_Test'Access, "Test of Finding Children of User_Type" );
      --  not implemented yet Register_Routine (T, Dataset_Child_Retrieve_Test'Access, "Test of Finding Children of Dataset" );
      --  not implemented yet Register_Routine (T, Run_Child_Retrieve_Test'Access, "Test of Finding Children of Run" );
      --  not implemented yet Register_Routine (T, Disaggregated_Data_Table_Description_Child_Retrieve_Test'Access, "Test of Finding Children of Disaggregated_Data_Table_Description" );
      --  not implemented yet Register_Routine (T, Disaggregated_Data_Table_Child_Retrieve_Test'Access, "Test of Finding Children of Disaggregated_Data_Table" );
      --  not implemented yet Register_Routine (T, Household_Data_Child_Retrieve_Test'Access, "Test of Finding Children of Household_Data" );
   end Register_Tests;
   
   --  Register routines to be run
   
   
   function Name ( t : Test_Case ) return Message_String is
   begin
          return Format( "Wsc_Db_Test Test Suite" );
   end Name;

   
   --  Preparation performed before each routine:
   procedure Set_Up( t : in out Test_Case ) is
   begin
      null;
   end Set_Up;
   
   --  Preparation performed before each routine:
   procedure Shut_Down( t : in out Test_Case ) is
   begin
      null; -- Connection_Pool.Shutdown;
   end Shut_Down;
begin
        DB_Logger.set_Log_Level( DB_Logger.debug_level );
      Connection_Pool.Initialise(
         Environment.Get_Server_Name,
         Environment.Get_Username,
         Environment.Get_Password,
         50 );

end Wsc_Db_Test;
