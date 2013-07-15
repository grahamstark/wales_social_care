package Model.WSC.Parameters.Historic is
   
   subtype Historic_Years is Integer range 1991 .. 2010;
   
   function Get_Parameters( wave : Waves ) return Parameters_Rec; 
   function Get_Parameters( year : Historic_Years ) return Parameters_Rec;

end  Model.WSC.Parameters.Historic;
