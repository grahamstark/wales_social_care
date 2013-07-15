--
-- set the highest non-displayed run to 1 (edited)
-- where there is no edited run
-- 
update run 
set 
        status = 1
where 
        username='@_USERNAME_@' 
and 
        status = 0 
and run_id = (
       select max( run_id ) 
       from run 
       where username='@_USERNAME_@' 
       and 
                ( select count( run_id ) 
                  from run 
                  where 
                       status = 1 
                  and 
                       username='@_USERNAME_@' ) = 0
   );
