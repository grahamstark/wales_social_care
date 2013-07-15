--
-- created on 26-11-2011 by Mill
--
drop database if exists wsc_db_mysql;
SET NAMES utf8;
create database wsc_db_mysql default charset=utf8;

use wsc_db_mysql;
SET FOREIGN_KEY_CHECKS = 0;

CREATE TABLE personal_results( 
       username VARCHAR(32) not null,
       run_id VARCHAR(60) not null,
       pid VARCHAR(60) not null,
       wave VARCHAR(2) not null,
       hid VARCHAR(60),
       buno INTEGER not null default 0,
       adno INTEGER not null default 0,
       passes_non_residential_capital_test INTEGER default 0,
       passes_non_residential_income_test INTEGER default 0,
       passes_residential_capital_test INTEGER default 0,
       passes_residential_income_test INTEGER default 0,
       la_contributions DOUBLE PRECISION default 0.0,
       client_contributions DOUBLE PRECISION default 0.0,
       gross_care_costs DOUBLE PRECISION default 0.0,
       total_payments_to_date DOUBLE PRECISION default 0.0,
       disposable_income DOUBLE PRECISION default 0.0,
       net_income DOUBLE PRECISION default 0.0,
       marginal_rate DOUBLE PRECISION default 0.0,
       capital_contribution DOUBLE PRECISION default 0.0,
       minimum_income_guarantee DOUBLE PRECISION default 0.0,
       passes_residential_means_test INTEGER default 0,
       passes_non_residential_means_test INTEGER default 0,
       hours_of_care_la DOUBLE PRECISION default 0.0,
       hours_of_care_private DOUBLE PRECISION default 0.0,
       uap INTEGER default 0,
       PRIMARY KEY( username, run_id, pid, wave )
) type = InnoDB;

CREATE TABLE personal_income( 
       username VARCHAR(32) not null,
       run_id VARCHAR(60) not null,
       pid VARCHAR(60) not null,
       wave VARCHAR(2) not null,
       hid VARCHAR(60),
       buno INTEGER not null default 0,
       adno INTEGER not null default 0,
       income_name VARCHAR(60) not null,
       value DOUBLE PRECISION default 0.0,
       PRIMARY KEY( username, run_id, pid, wave, income_name )
) type = InnoDB;

SET FOREIGN_KEY_CHECKS = 1;
