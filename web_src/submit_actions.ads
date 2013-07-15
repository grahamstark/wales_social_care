package Submit_Actions is

   type Which_Apply is (
      this_year_only,
      this_and_subsequent_years,
      all_years );

   type Which_Reset is (
      this_year_these_items_only,
      this_year_all_items,
      all_years_these_items_only,
      all_years_all_items );

   type Which_Uprate is (
      this_year_these_items_only,
      this_year_all_items,
      this_and_subsequent_years_these_items_only,
      this_and_subsequent_years_all_items );

      
end Submit_Actions;

