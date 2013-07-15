--
-- block copy associated parameters and run settings from a run (despite the title, not the run itself'
--
insert into key_value_parameter( username, run_id, key, val ) select '@_TO-USERNAME_@', @_TO-RUN-ID_@, key, val from key_value_parameter where username='@_FROM-USERNAME_@' and run_id=@_FROM-RUN-ID_@;
insert into uprate_assumption select '@_TO-USERNAME_@', @_TO-RUN-ID_@, percent_change, use_obr,  target, element from uprate_assumption where username='@_FROM-USERNAME_@' and run_id=@_FROM-RUN-ID_@;
insert into probit_threshold select '@_TO-USERNAME_@', @_TO-RUN-ID_@, element, threshold from probit_threshold where username='@_FROM-USERNAME_@' and run_id=@_FROM-RUN-ID_@;
     
