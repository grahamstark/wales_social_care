
create index pr_iter on personal_results( iteration );
create index pi_iter on personal_income( iteration );
create index pr_pid on personal_results( pid, iteration );
create index pi_pid on personal_income( pid, iteration );
create index pr_waves on personal_results( wave, iteration );
create index pi_waves on personal_income( wave, iteration );
create index pr_run on personal_results( username, run_id, sysno, iteration );
create index ps_run on personal_income( username, run_id, sysno, iteration );

create index pr_pk on personal_results( username, run_id, wave, pid, sysno, iteration );
create index pi_pk on personal_income( username, run_id, wave, pid, sysno, income_type, iteration );
create index pr_ak on personal_results( username, run_id, wave, buno, adno, sysno, iteration  );
create index pi_ak on personal_income( username, run_id, wave, hid, buno, adno, sysno, income_type, iteration  );
create index pi_all_inc on personal_income( username, run_id, wave, hid, buno, adno, sysno, iteration );
