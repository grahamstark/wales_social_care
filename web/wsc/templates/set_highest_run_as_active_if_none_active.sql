
update 
        run 
set 
        status=1 
where 
        username='@_USERNAME_@' 
and  
        run_id = 
               ( select 
                        max( run_id ) 
                 from 
                        run 
                 where 
                        username='@_USERNAME_@' 
                 and 
                        status=0 ) 
and 
       ( select 
                 run_id 
         from 
                 run 
         where 
                 username='@_USERNAME_@' 
         and status = 1 limit 1 ) is null;
