with Ada.Text_IO;

package body Model.WSC.Inequality is

   procedure Print_Inequality_List( filename : String; ineq : Inequality_List ) is
      use Ada.Text_IO;
      f : File_Type;
      num_quantiles : constant Positive := Positive( ineq.Length );
      q : Inequality_Record;
   begin
      Create( f, Out_File, filename );
      for p in 1 .. num_quantiles loop
         q := ineq.element( p );
         Put_Line( f, 
            Amount'Image( q.population ) & "," & 
            Amount'Image( q.income )  & "," &
            Adult_Age_Band'Image( q.age_of_head ) & "," &
            Tenure_Type'Image( q.tenure ) & "," &
            Decile_Number'Image( q.decile ));
      end loop;
      Close( f );      
   end Print_Inequality_List;
   
end Model.WSC.Inequality;
