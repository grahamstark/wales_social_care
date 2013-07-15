--
-- created on 24-07-2012 by Mill
--
drop database if exists wsc_db;
create database wsc_db with encoding 'UTF-8';

\c wsc_db;


CREATE TABLE user_type( 
       username VARCHAR(32) not null,
       password VARCHAR(60),
       title VARCHAR(60),
       description VARCHAR(60),
       email VARCHAR(60),
       work_dir VARCHAR(60),
       utype INTEGER,
       lang INTEGER,
       preferences VARCHAR(60),
       last_used TIMESTAMP,
       PRIMARY KEY( username )
);

CREATE TABLE dataset( 
       name VARCHAR(32) not null,
       creator VARCHAR(32),
       title VARCHAR(60),
       run_id INTEGER,
       PRIMARY KEY( name ),
       CONSTRAINT dataset_FK_0 FOREIGN KEY( creator) references user_type( username ) on delete SETNULL on update CASCADE
);

CREATE TABLE run( 
       run_id INTEGER not null,
       username VARCHAR(32) not null,
       comparison_username VARCHAR(60),
       comparison_run_id INTEGER,
       title VARCHAR(60),
       use_random_threshold INTEGER,
       num_iterations INTEGER default 1,
       interest_rate_pct DOUBLE PRECISION,
       real_terms INTEGER,
       is_null_settings INTEGER default 0,
       working_root VARCHAR(120),
       users_directory VARCHAR(120),
       output_directory VARCHAR(120),
       dir_separator VARCHAR(1),
       session_id VARCHAR(60),
       dataset_name VARCHAR(32),
       default_run_dir_id INTEGER,
       start_year INTEGER,
       end_year INTEGER,
       weighting_function INTEGER,
       weighting_lower_bound DOUBLE PRECISION,
       weighting_upper_bound DOUBLE PRECISION,
       status INTEGER,
       type_of_run INTEGER default 0,
       PRIMARY KEY( run_id, username ),
       CONSTRAINT run_FK_0 FOREIGN KEY( username) references user_type( username ) on delete CASCADE on update CASCADE,
       CONSTRAINT run_FK_1 FOREIGN KEY( dataset_name) references dataset( name ) on delete SETNULL on update CASCADE
);

CREATE TABLE state( 
       username VARCHAR(32) not null,
       run_id INTEGER not null,
       household INTEGER default 0,
       other_counter INTEGER default 0,
       year INTEGER default 0,
       phase INTEGER,
       health INTEGER,
       error_code INTEGER default 0,
       read_error INTEGER default 0,
       session_id VARCHAR(120),
       PRIMARY KEY( username, run_id ),
       CONSTRAINT state_FK_0 FOREIGN KEY( username, run_id) references run( username, run_id ) on delete cascade on update cascade
);

CREATE TABLE key_value_parameter( 
       username VARCHAR(32) not null,
       run_id INTEGER not null,
       key VARCHAR(250) not null,
       val VARCHAR(250),
       PRIMARY KEY( username, run_id, key ),
       CONSTRAINT key_value_parameter_FK_0 FOREIGN KEY( username, run_id) references run( username, run_id ) on delete cascade on update cascade
);

CREATE TABLE uprate_assumption( 
       username VARCHAR(32) not null,
       run_id INTEGER not null,
       percent_change DOUBLE PRECISION default 0.0,
       use_obr INTEGER default 0,
       target INTEGER not null,
       element INTEGER,
       PRIMARY KEY( username, run_id, target ),
       CONSTRAINT uprate_assumption_FK_0 FOREIGN KEY( username, run_id) references run( username, run_id ) on delete cascade on update cascade
);

CREATE TABLE probit_threshold( 
       username VARCHAR(32) not null,
       run_id INTEGER not null,
       element INTEGER not null,
       threshold DOUBLE PRECISION default 0.0,
       PRIMARY KEY( username, run_id, element ),
       CONSTRAINT probit_threshold_FK_0 FOREIGN KEY( username, run_id) references run( username, run_id ) on delete cascade on update cascade
);

CREATE TABLE personal_results( 
       username VARCHAR(32) not null,
       run_id INTEGER not null,
       sysno INTEGER not null,
       iteration INTEGER not null,
       pid BIGINT not null,
       wave VARCHAR(2) not null,
       hid BIGINT,
       buno INTEGER not null default 0,
       adno INTEGER not null default 0,
       passes_non_residential_capital_test INTEGER,
       passes_non_residential_income_test INTEGER,
       passes_residential_capital_test INTEGER,
       passes_residential_income_test INTEGER,
       passes_residential_means_test INTEGER,
       passes_non_residential_means_test INTEGER,
       la_contributions DOUBLE PRECISION default 0.0,
       client_contributions DOUBLE PRECISION default 0.0,
       gross_care_costs DOUBLE PRECISION default 0.0,
       total_payments_to_date DOUBLE PRECISION default 0.0,
       disposable_income DOUBLE PRECISION default 0.0,
       net_income DOUBLE PRECISION default 0.0,
       marginal_rate DOUBLE PRECISION default 0.0,
       capital_contribution DOUBLE PRECISION default 0.0,
       minimum_income_guarantee DOUBLE PRECISION default 0.0,
       hours_of_care_la DOUBLE PRECISION default 0.0,
       hours_of_care_private DOUBLE PRECISION default 0.0,
       uap INTEGER,
       remaining_capital_stock DOUBLE PRECISION default 0.0,
       PRIMARY KEY( username, run_id, sysno, iteration, pid, wave ),
       CONSTRAINT personal_results_FK_0 FOREIGN KEY( username, run_id) references run( username, run_id ) on delete cascade on update cascade
);

CREATE TABLE personal_income( 
       username VARCHAR(32) not null,
       run_id INTEGER not null,
       pid BIGINT not null,
       sysno INTEGER not null,
       iteration INTEGER not null,
       wave VARCHAR(2) not null,
       income_type INTEGER not null,
       hid BIGINT,
       buno INTEGER not null default 0,
       adno INTEGER not null default 0,
       value DOUBLE PRECISION default 0.0,
       PRIMARY KEY( username, run_id, pid, sysno, iteration, wave, income_type ),
       CONSTRAINT personal_income_FK_0 FOREIGN KEY( username, run_id) references run( username, run_id ) on delete cascade on update cascade
);

CREATE TABLE disaggregated_data_table_description( 
       model_table_name VARCHAR(40) not null,
       filename VARCHAR(80),
       has_totals INTEGER,
       table_type VARCHAR(60),
       table_subtype VARCHAR(60),
       PRIMARY KEY( model_table_name )
);

CREATE TABLE disaggregated_data_table_cell_description( 
       model_table_name VARCHAR(40) not null,
       cell_pos INTEGER not null,
       cell_label VARCHAR(80),
       cell_type VARCHAR(20),
       file_pos INTEGER,
       PRIMARY KEY( model_table_name, cell_pos ),
       CONSTRAINT disaggregated_data_table_cell_description_FK_0 FOREIGN KEY( model_table_name) references disaggregated_data_table_description( model_table_name ) on delete CASCADE on update cascade
);

CREATE TABLE disaggregated_data_table( 
       run_id INTEGER not null,
       username VARCHAR(32) not null,
       model_table_name VARCHAR(40) not null,
       iteration INTEGER not null default 1,
       PRIMARY KEY( run_id, username, model_table_name, iteration ),
       CONSTRAINT disaggregated_data_table_FK_0 FOREIGN KEY( model_table_name) references disaggregated_data_table_description( model_table_name ) on delete CASCADE on update cascade,
       CONSTRAINT disaggregated_data_table_FK_1 FOREIGN KEY( run_id, username) references run( run_id, username ) on delete CASCADE on update cascade
);

CREATE TABLE disaggregated_data_table_cell( 
       run_id INTEGER not null,
       username VARCHAR(32) not null,
       model_table_name VARCHAR(40) not null,
       row_num INTEGER not null,
       col_num INTEGER not null,
       wave VARCHAR(2) not null,
       iteration INTEGER not null,
       system_number INTEGER not null default 1,
       value1 DOUBLE PRECISION default 0.0,
       value2 DOUBLE PRECISION default 0.0,
       value3 DOUBLE PRECISION default 0.0,
       value4 DOUBLE PRECISION default 0.0,
       value5 DOUBLE PRECISION default 0.0,
       value6 DOUBLE PRECISION default 0.0,
       i BIGINT default 0,
       p1 INTEGER default 0,
       p2 INTEGER default 0,
       p3 INTEGER default 0,
       PRIMARY KEY( run_id, username, model_table_name, row_num, col_num, wave, iteration, system_number ),
       CONSTRAINT disaggregated_data_table_cell_FK_0 FOREIGN KEY( run_id, username, iteration, model_table_name) references disaggregated_data_table( run_id, username, iteration, model_table_name ) on delete CASCADE on update cascade
);

CREATE TABLE maxima_and_totals( 
       lifetime_la_contributions DOUBLE PRECISION default 0.0,
       lifetime_client_contributions DOUBLE PRECISION default 0.0,
       lifetime_gross_payments DOUBLE PRECISION default 0.0,
       lifetime_capital_contributions DOUBLE PRECISION default 0.0,
       highest_la_contribution DOUBLE PRECISION default 0.0
);

CREATE TABLE uap_threshold( 
       run_id INTEGER not null,
       username VARCHAR(32) not null,
       sysno INTEGER not null,
       iteration INTEGER not null,
       wave VARCHAR(2) not null,
       uap_level INTEGER not null,
       threshold DOUBLE PRECISION default 0.0,
       PRIMARY KEY( run_id, username, sysno, iteration, wave, uap_level ),
       CONSTRAINT uap_threshold_FK_0 FOREIGN KEY( username, run_id) references run( username, run_id ) on delete cascade on update cascade
);

CREATE TABLE table_stats( 
       run_id INTEGER not null,
       username VARCHAR(32) not null,
       model_table_name VARCHAR(40) not null,
       row_num INTEGER not null,
       col_num INTEGER not null,
       wave VARCHAR(2) not null,
       system_number INTEGER not null default 1,
       nvalues INTEGER default 0,
       rmean_1 DOUBLE PRECISION default 0.0,
       rmin_1 DOUBLE PRECISION default 0.0,
       rmax_1 DOUBLE PRECISION default 0.0,
       rmed_1 DOUBLE PRECISION default 0.0,
       sddev_1 DOUBLE PRECISION default 0.0,
       dec1_1 DOUBLE PRECISION default 0.0,
       dec10_1 DOUBLE PRECISION default 0.0,
       rmean_2 DOUBLE PRECISION default 0.0,
       rmin_2 DOUBLE PRECISION default 0.0,
       rmax_2 DOUBLE PRECISION default 0.0,
       rmed_2 DOUBLE PRECISION default 0.0,
       sddev_2 DOUBLE PRECISION default 0.0,
       dec1_2 DOUBLE PRECISION default 0.0,
       dec10_2 DOUBLE PRECISION default 0.0,
       rmean_3 DOUBLE PRECISION default 0.0,
       rmin_3 DOUBLE PRECISION default 0.0,
       rmax_3 DOUBLE PRECISION default 0.0,
       rmed_3 DOUBLE PRECISION default 0.0,
       sddev_3 DOUBLE PRECISION default 0.0,
       dec1_3 DOUBLE PRECISION default 0.0,
       dec10_3 DOUBLE PRECISION default 0.0,
       rmean_4 DOUBLE PRECISION default 0.0,
       rmin_4 DOUBLE PRECISION default 0.0,
       rmax_4 DOUBLE PRECISION default 0.0,
       rmed_4 DOUBLE PRECISION default 0.0,
       sddev_4 DOUBLE PRECISION default 0.0,
       dec1_4 DOUBLE PRECISION default 0.0,
       dec10_4 DOUBLE PRECISION default 0.0,
       rmean_5 DOUBLE PRECISION default 0.0,
       rmin_5 DOUBLE PRECISION default 0.0,
       rmax_5 DOUBLE PRECISION default 0.0,
       rmed_5 DOUBLE PRECISION default 0.0,
       sddev_5 DOUBLE PRECISION default 0.0,
       dec1_5 DOUBLE PRECISION default 0.0,
       dec10_5 DOUBLE PRECISION default 0.0,
       rmean_6 DOUBLE PRECISION default 0.0,
       rmin_6 DOUBLE PRECISION default 0.0,
       rmax_6 DOUBLE PRECISION default 0.0,
       rmed_6 DOUBLE PRECISION default 0.0,
       sddev_6 DOUBLE PRECISION default 0.0,
       dec1_6 DOUBLE PRECISION default 0.0,
       dec10_6 DOUBLE PRECISION default 0.0,
       PRIMARY KEY( run_id, username, model_table_name, row_num, col_num, wave, system_number )
);

CREATE TABLE household_capital( 
       username VARCHAR(32) not null,
       run_id INTEGER not null,
       sysno INTEGER not null,
       iteration INTEGER not null,
       hid BIGINT not null,
       wave VARCHAR(2) not null,
       capital_stock DOUBLE PRECISION default 0.0,
       PRIMARY KEY( username, run_id, sysno, iteration, hid, wave )
);

CREATE TABLE gain_lose( 
       username VARCHAR(32) not null,
       run_id INTEGER not null,
       iteration INTEGER not null,
       hid BIGINT,
       pid BIGINT not null,
       wave VARCHAR(2) not null,
       pre DOUBLE PRECISION default 0.0,
       post DOUBLE PRECISION default 0.0,
       PRIMARY KEY( username, run_id, iteration, pid, wave )
);

CREATE TABLE household_data( 
       dataset_name VARCHAR(32) not null,
       iteration INTEGER not null,
       wave VARCHAR(2) not null,
       interview_date TIMESTAMP default TIMESTAMP '1901-01-01 00:00:00.000000',
       current_simulated_date TIMESTAMP default TIMESTAMP '1901-01-01 00:00:00.000000',
       hid BIGINT not null,
       origin_hid BIGINT not null,
       tenure INTEGER,
       region INTEGER,
       gross_rent DOUBLE PRECISION,
       net_rent DOUBLE PRECISION,
       mortgage_outstanding DOUBLE PRECISION,
       gross_housing_costs DOUBLE PRECISION,
       net_housing_costs DOUBLE PRECISION,
       total_income DOUBLE PRECISION,
       house_value DOUBLE PRECISION,
       other_property_value DOUBLE PRECISION,
       mortgage_payment DOUBLE PRECISION,
       years_outstanding_on_mortgage INTEGER,
       years_outstanding_on_mortgage INTEGER,
       weight_basic DOUBLE PRECISION,
       weight_extended_1 DOUBLE PRECISION,
       weight_extended_2 DOUBLE PRECISION,
       ct_band INTEGER,
       has_full_sample INTEGER,
       PRIMARY KEY( dataset_name, iteration, wave, hid, origin_hid ),
       CONSTRAINT household_data_FK_0 FOREIGN KEY( dataset_name) references dataset( name ) on delete SETNULL on update CASCADE
);

CREATE TABLE person( 
       dataset_name VARCHAR(32) not null,
       iteration INTEGER not null,
       wave VARCHAR(2) not null,
       hid BIGINT not null,
       pid BIGINT not null,
       buno INTEGER default 0,
       chno INTEGER default 0,
       adno INTEGER default 0,
       pno INTEGER,
       age INTEGER,
       sex INTEGER,
       activities_of_daily_living_score DOUBLE PRECISION,
       health_score DOUBLE PRECISION,
       years_in_residential_care INTEGER,
       respondent_weight_basic DOUBLE PRECISION,
       respondent_weight_extended_1 DOUBLE PRECISION,
       respondent_weight_extended_2 DOUBLE PRECISION,
       enumeration_weight_basic DOUBLE PRECISION,
       enumeration_weight_extended_1 DOUBLE PRECISION,
       enumeration_weight_extended_2 DOUBLE PRECISION,
       hdsp INTEGER,
       has_full_sample INTEGER,
       receives_informal_care_from_household_member INTEGER,
       receives_informal_care_from_non_householder INTEGER,
       hours_of_care_recieved INTEGER,
       hours_of_care_given INTEGER,
       dies_this_period INTEGER,
       seperates_this_period INTEGER,
       manage_stairs_help INTEGER,
       get_around_house_help INTEGER,
       get_in_or_out_of_bed_help INTEGER,
       cut_toenails_help INTEGER,
       bathing_or_showering_help INTEGER,
       walk_down_road_help INTEGER,
       manage_stairs_difficulty INTEGER,
       get_around_house_difficulty INTEGER,
       get_in_or_out_of_bed_difficulty INTEGER,
       cut_toenails_difficulty INTEGER,
       bathing_or_showering_difficulty INTEGER,
       walk_down_road_difficulty INTEGER,
       employment_status INTEGER,
       usual_hours_worked_per_week INTEGER,
       highest_qualification INTEGER,
       is_disabled INTEGER,
       marital_status INTEGER,
       partner_status INTEGER,
       health_status INTEGER,
       base_personal_wealth DOUBLE PRECISION,
       personal_wealth DOUBLE PRECISION,
       disability_living_allowance_mobility_level INTEGER,
       disability_living_allowance_care_level INTEGER,
       attendance_allowance_level INTEGER,
       PRIMARY KEY( dataset_name, iteration, wave, hid, pid ),
       CONSTRAINT person_FK_0 FOREIGN KEY( dataset_name) references dataset( name ) on delete SETNULL on update CASCADE,
       CONSTRAINT person_FK_1 FOREIGN KEY( dataset_name, iteration, hid) references household_data( dataset_name, iteration, hid ) on delete SETNULL on update CASCADE
);

