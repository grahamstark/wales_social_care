with Ada.Exceptions; 
with Line_Extractor;
with Ada.Text_IO;
with GNATColl.Traces;
with Ada.Strings.Unbounded.Text_IO;
with Text_Utils;

package body Parameter_System.Input_Buffer.Utils is
  
   use Ada.Text_IO;
   use Text_Utils;
   use Ada.Strings.Unbounded.Text_IO;
   
   function Get_Num_Errors( buff : Buffer ) return Natural is
   begin
      return 0;
   end Get_Num_Errors;
   
   log_trace : constant GNATColl.Traces.Trace_Handle := GNATColl.Traces.Create( "Parameter_System.Input_Buffer.Utils" );
   
   procedure Log( s : String ) is
   begin
      GNATColl.Traces.Trace( log_trace, s );
   end Log;

   
   --
   -- FIXME use this as Skeleton for a visitor pattern
   --
   procedure Iterate( 
      file     : File_Type; 
      base_sys : Parameter_System_Rec; 
      sys      : Parameter_System_Rec; 
      prefix   : Unbounded_String ) is
   prefix_copy : Unbounded_String := prefix;
   
   use Text_Utils.String_Maps_Package;
   use Ada.Exceptions; 
   
   begin
      Log( "key is now " & TS( prefix ));
      Iterate_Parameters:
      declare
      use Parameter_Rec_Package;
         num_params : constant Natural := Natural( sys.parameters.Length );  
         parameter  : Parameter_Rec;
         key        : Unbounded_String;
         default    : Unbounded_String;
         enum_type  : Enumerated_Type_Rec;
       begin
         for pno in 1 .. num_params loop
            parameter := sys.parameters.Element( pno );
            -- FIXME MISSING array params 
            if( parameter.collection_type = singular )then
                  key := prefix & DELIMITER & parameter.instance_name;
                  if( parameter.enum_type_ref /= Null_Unbounded_String )then
                     enum_type := 
                        Enum_Search.Get_Enum( base_sys, To_String( parameter.enum_type_ref ));   
                  end if;
                  Put_Line( file, key & "=XXX;" );
            else
               Raise_Exception( Param_Exception'Identity, 
                   "Parameter-System.Input_Buffer : non single param is not inplemented parameter" & To_String( key ));
               
            end if;
         end loop;
      end Iterate_Parameters;
      
      Iterate_References:
      declare
      use Parameter_System_Reference_Rec_Package;  
      use Parameter_Rec_Package;
         num_references : constant Natural := Natural( sys.parameter_system_references.Length );  
         key            : Unbounded_String;
         ref            : Parameter_System_Reference_Rec;
         reffed_system  : Parameter_System_Rec;
      begin
         --
         -- FIXME: this really isn't sensible
         -- treat this symmetrically with Subsystems below 
         for rno in 1 .. num_references loop
            Put_Line( file, "# -- from refrences " );
            ref := sys.parameter_system_references.Element( rno );
            reffed_system := base_sys.Get( To_String( ref.sys_type ), by_name );
            key := prefix & DELIMITER & ref.instance_name;
            Log( " ref.collection_type " & ref.collection_type'Img );
            case ref.collection_type is
            when singular =>  
               Iterate( 
                  file,
                  base_sys,
                  reffed_system,
                  prefix & DELIMITER & ref.instance_name );
            when set      => 
                Raise_Exception( Param_Exception'Identity, 
                   "Parameter-System.Input_Buffer : set of params is not inplemented key:" & To_String( key ));
            when Parameter_System.map      => 
                Raise_Exception( Param_Exception'Identity, 
                   "Parameter-System.Input_Buffer : map of params is not inplemented key:" & To_String( key ));
            when list     => 
               case ref.index_type is
                  when none => 
                     Raise_Exception( Param_Exception'Identity, 
                        "Parameter-System.Input_Buffer : inconsistent state list: but no collection type key:" & To_String( key ));
                  when integer_type =>
                     declare
                     use Value_And_Error_Map_Package;
                        parameter  : Parameter_Rec;
                        num_params : constant Natural := Natural( reffed_system.parameters.Length );  
                        index_key     : Unbounded_String;
                        counter_key   : constant Unbounded_String := key & DELIMITER & "count";
                        value         : Value_And_Error_Access; 
                        vel           : Value_And_Error_Vector;
                        counter       : constant Natural := 10;
                     begin
                        Put_Line( file, counter_key & " = 10;" );
                        each_param:
                        for pno in 1 .. num_params loop
                           parameter := reffed_system.parameters.Element( pno );
                           each_index:
                           for i in 1 .. counter loop
                              index_key := Line_Extractor.Make_Key( key, i, parameter.instance_name );
                              Put_Line( file, index_key & "=XXX;" );
                           end loop each_index;
                        end loop each_param;
                     end;
                  when enumerated_type =>
                     Raise_Exception( Param_Exception'Identity, 
                         "Parameter-System.Input_Buffer : enumerated type index is not inplemented key:" & To_String( key ));
                     -- find enum type ref
                     -- loop through elements; key is +[id]
                  when string_type => --
                     Raise_Exception( Param_Exception'Identity, 
                         "Parameter-System.Input_Buffer : list of params using string index is not inplemented key:" & To_String( key ));
               end case;
            end case;
         end loop;
      end Iterate_References;
      
      Iterate_Subsystems:
      declare
         use Parameter_System_Rec_Package;
         subsys : Parameter_System_Rec;
         num_subsystems : constant Natural := Natural( sys.parameter_systems.Length );
      begin
         for sno in 1 .. num_subsystems loop
            subsys := sys.parameter_systems.Element( sno ).all;
            if( subsys.instance_name /= Null_Unbounded_String ) then
               Iterate( 
                  file,
                  base_sys,
                  subsys,
                  prefix & DELIMITER & subsys.instance_name );
               end if;
         end loop;
      
      end Iterate_Subsystems;
   
   end Iterate;
   
   procedure Write_Parameter_File_Skeleton( 
      filename : String; 
      sys      : Parameter_System_Rec ) is
      
      path : Unbounded_String := Null_Unbounded_String;
      
      file : File_Type;
      
   begin
      Create( file, Out_File, filename );
      path := path & sys.instance_name;
      Iterate( 
            file,
            sys,
            sys,
            path );
      Close( file );
   end Write_Parameter_File_Skeleton;
 
   
end Parameter_System.Input_Buffer.Utils;
