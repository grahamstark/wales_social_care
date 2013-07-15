select 
  iteration, 
  wave, 
  passes_residential_means_test as means, 
  count( passes_residential_means_test) as res_mt_count,
  passes_residential_capital_test as capital, 
  count( passes_residential_capital_test) as res_capital_count,
  passes_residential_income_test as income, 
  count( passes_residential_income_test) as res_income_count 

from 
  personal_results 
where 
  username='test_user1' and run_id=2 and iteration=1 and 
        ( passes_residential_means_test > 0 or passes_residential_capital_test > 0 or passes_residential_income_test > 0 )
group by 
  iteration, passes_residential_means_test, passes_residential_capital_test,  passes_residential_income_test, wave   
order by 
  length( wave ), wave, passes_residential_means_test;
