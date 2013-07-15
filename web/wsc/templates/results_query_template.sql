select 
        personal_results_pre.username as username, 
        personal_results_pre.run_id as run_id,
        personal_results_pre.iteration as iteration,
        personal_results_pre.hid as hid, 
        personal_results_pre.pid as pid, 
        personal_results_pre.wave as wave,
        cast( personal_results_pre.@_FIELD-NAME_@ as float ) as pre, 
        cast( personal_results_post.@_FIELD-NAME_@ as float ) as post
from 
        personal_results_pre,personal_results_post 
where 
        personal_results_pre.username=personal_results_post.username and 
        personal_results_pre.run_id = personal_results_post.run_id and
        personal_results_pre.pid=personal_results_post.pid and 
        personal_results_pre.wave = personal_results_post.wave and 
        personal_results_pre.iteration = personal_results_post.iteration and 

        personal_results_pre.@_FIELD-NAME_@ @_OP_@ personal_results_post.@_FIELD-NAME_@ and
        personal_results_pre.iteration = @_ITERATION_@ and
        personal_results_pre.run_id= @_RUN-ID_@ and
        personal_results_pre.username='@_USERNAME_@' and
        personal_results_pre.wave = '@_WAVE_@' and
        ( cast( personal_results_pre.@_FIELD-NAME_@ as float ) <> 0.0 or 
          cast( personal_results_post.@_FIELD-NAME_@ as float ) <> 0.0 )
        
limit @_LIMIT_@;

