select 
        personal_income_pre.username as username, 
        personal_income_pre.run_id as run_id,
        personal_income_pre.iteration as iteration,
        personal_income_pre.hid as hid, 
        personal_income_pre.pid as pid, 
        personal_income_pre.wave as wave,
        personal_income_pre.value as pre, 
        personal_income_post.value as post
from 
        personal_income_pre,personal_income_post 
where 
        personal_income_pre.username=personal_income_post.username and 
        personal_income_pre.run_id = personal_income_post.run_id and
        personal_income_pre.pid=personal_income_post.pid and 
        personal_income_pre.wave = personal_income_post.wave and 
        personal_income_pre.iteration = personal_income_post.iteration and 
        personal_income_post.income_type = personal_income_pre.income_type and
        
        personal_income_pre.value @_OP_@ personal_income_post.value and
        personal_income_pre.income_type = @_INCOME_TYPE_@ and
        personal_income_pre.iteration = @_ITERATION_@ and
        personal_income_pre.run_id=@_RUN-ID_@ and
        personal_income_pre.username='@_USERNAME_@' and
        personal_income_pre.wave = '@_WAVE_@' and
        ( personal_income_pre.value <> 0.0 or personal_income_post.value <> 0.0 )
        
limit @_LIMIT_@;


