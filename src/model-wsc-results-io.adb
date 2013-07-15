with Ada.Containers.Ordered_Maps;
with Ada.Direct_IO;
with Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;
with GNAT.String_Split;

package body Model.WSC.Results.IO is

   package uio renames Ada.Strings.Unbounded.Text_IO;
   package tio renames Ada.Text_IO;
   package Sernum_IO is new Ada.Text_IO.Integer_IO( Sernum_Value );
   package UAP_IO is new Ada.Text_IO.Enumeration_IO( UAP_Level );
   package Means_Test_Result_IO is new Ada.Text_IO.Enumeration_IO( Means_Test_Result );
   use Ada.Strings.Unbounded;
   
   
   procedure Dump( filename : String; map : Individual_Results_Map ) is
   use Ada.Text_IO;
   use Individual_Results_Package;
      f : File_Type;
      
      procedure Dump_One( c : Individual_Results_Cursor ) is
         res : Personal_Result := Element( c );
      begin
         Put( f, Sernum_Value'Image( res.sernum ) & " " );
         Put( f, Boolean'Image( res.is_residential ) & " " );
         Put( f, Boolean'Image( res.receives_social_care ) & " " );
         for i in Calculated_Incomes loop
            Put( f, Amount'Image( res.income( i )) & " " );
         end loop;      
         Put( f, Amount'Image( res.la_contributions ) & " " );
         Put( f, Amount'Image( res.client_contributions ) & " " );
         Put( f, Amount'Image( res.gross_care_costs ) & " " );
         Put( f, Amount'Image( res.disposable_income ) & " " );
         Put( f, Amount'Image( res.total_payments_to_date ) & " " );
         Put( f, Amount'Image( res.net_income ) & " " );
         Put( f, Amount'Image( res.marginal_rate ) & " " );
         Put( f, Amount'Image( res.capital_contribution ) & " " );
         Put( f, Amount'Image( res.minimum_income_guarantee ) & " " );
         Put( f, Amount'Image( res.tarriff_income ) & " " );
         Put( f, Amount'Image( res.highest_la_contribution ) & " " );
         for s in Summary_Items_Type loop
            Put( f, Amount'Image( res.summary( s )) & " " );
         end loop;
         for c in Costs_Type loop
            Put( f, Amount'Image( res.costs_summary( c )));
         end loop;

         Ada.Text_IO.Put( f, " " );
         Means_Test_Result_IO.Put( f, res.passes_non_residential_income_test );
         Ada.Text_IO.Put( f, " " );
         Means_Test_Result_IO.Put( f, res.passes_non_residential_capital_test );
         Ada.Text_IO.Put( f, " " );
         Means_Test_Result_IO.Put( f, res.passes_residential_income_test );
         Ada.Text_IO.Put( f, " " );
         Means_Test_Result_IO.Put( f, res.passes_residential_capital_test );
         
         Ada.Text_IO.Put( f, " " );
         Means_Test_Result_IO.Put( f, res.passes_residential_means_test );
         Ada.Text_IO.Put( f, " " );
         Means_Test_Result_IO.Put( f, res.passes_non_residential_means_test );
         Ada.Text_IO.Put( f, " " );
         Amount_IO.Put( f, res.hours_of_care_la );
         Ada.Text_IO.Put( f, " " );
         Amount_IO.Put( f, res.hours_of_care_private );
         Ada.Text_IO.Put( f, " " );
         Ada.Text_IO.Put( f, res.uap'Img );
         if( c /= map.Last )then
            New_Line( f );
         end if;
      end Dump_One;
      
   begin
      Create( f, Out_File, filename );
      map.Iterate( Dump_One'Access );
      Close( f );
   end Dump;
   
   procedure Restore( filename : String; map : out Individual_Results_Map ) is
   use Ada.Text_IO;
      f : File_Type;
      res : Personal_Result;
   begin
      Open( f, In_File, filename );
      loop 
         Sernum_IO.Get( f, res.sernum ); 
         Put_Line( "got sernum " & Sernum_Value'Image( res.sernum ));
         Boolean_IO.Get( f, res.is_residential );
         Boolean_IO.Get( f, res.receives_social_care );
         for i in Calculated_Incomes loop
            Amount_IO.Get( f, res.income( i ));
         end loop;      
         Amount_IO.Get( f, res.la_contributions );
         Amount_IO.Get( f, res.client_contributions );
         Amount_IO.Get( f, res.gross_care_costs );
         Amount_IO.Get( f, res.disposable_income );
         Amount_IO.Get( f, res.total_payments_to_date );
         Amount_IO.Get( f, res.net_income );
         Amount_IO.Get( f, res.marginal_rate );
         Amount_IO.Get( f, res.capital_contribution );
         Amount_IO.Get( f, res.minimum_income_guarantee );
         Amount_IO.Get( f, res.tarriff_income );
         Amount_IO.Get( f, res.highest_la_contribution );
         for s in Summary_Items_Type loop
            Amount_IO.Get( f, res.summary( s ));
         end loop;
         for c in Costs_Type loop
            Amount_IO.Get( f, res.costs_summary( c ));
         end loop;
         
         Means_Test_Result_IO.Get( f, res.passes_non_residential_income_test );
         Means_Test_Result_IO.Get( f, res.passes_non_residential_capital_test );
         Means_Test_Result_IO.Get( f, res.passes_residential_income_test );
         Means_Test_Result_IO.Get( f, res.passes_residential_capital_test );

         Means_Test_Result_IO.Get( f, res.passes_residential_means_test );
         Means_Test_Result_IO.Get( f, res.passes_non_residential_means_test );
         Amount_IO.Get( f, res.hours_of_care_la );
         Amount_IO.Get( f, res.hours_of_care_private );
         
         UAP_IO.Get( f, res.uap );
         
         map.Insert( res.sernum, res );         
         exit when End_Of_File( f );
      end loop;
      Close( f );
   end Restore;
   
end Model.WSC.Results.IO;
