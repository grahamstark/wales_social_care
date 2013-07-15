alter table run add column do_reweighting integer default 0;
alter table run alter column weighting_lower_bound set default 0.01;
alter table run alter column weighting_upper_bound set default 10.00;
alter table run alter column weighting_function set default 3;
update run set weighting_upper_bound = 10.0;
update run set weighting_lower_bound = 0.01;
update run set weighting_function = 0;


