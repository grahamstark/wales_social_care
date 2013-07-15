with Keyed_Text_Buffer;

package Model.WSC.Parameters.IO is
   use  Keyed_Text_Buffer;
   -- function To_Keyed_Buffer( sys : Parameters_Array; start_year, end_year : Simulation_Years ) return Keyed_Text_Buffer.Text_Buffer;
   procedure Write( sys : Parameters_Array; filename : String; start_year, end_year : Simulation_Years );
   procedure Read( buff : Text_Buffer; start_year, end_year : Simulation_Years; sys : in out Parameters_Array );
   function Read( filename : String; start_year, end_year : Simulation_Years ) return Parameters_Array;
end Model.WSC.Parameters.IO;
