with Model.WSC.Parameter_System_Declarations;

package Model.WSC.Parameters.Web_IO is
   
   use Model.WSC.Parameter_System_Declarations;
   -- function To_Keyed_Buffer( sys : Parameters_Array; start_year, end_year : Simulation_Years ) return Keyed_Text_Buffer.Text_Buffer;
   function Read( buff : WSC_Editing_Buffer; year : Simulation_Years ) return Parameters_Rec;
   
end Model.WSC.Parameters.Web_IO;
