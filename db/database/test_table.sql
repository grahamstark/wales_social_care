create table available_items(
        name char( 120 ) not null primary key,
        description char( 120 ),
        display_order integer );
        
drop table summary_results_test;
CREATE TABLE summary_results_test( 
       username VARCHAR(32) not null,
       run_id VARCHAR(60) not null,
       sysno INTEGER not null,
       iteration INTEGER not null,
       wave VARCHAR(2) not null,
       item_name char( 120 ),
       value real,
       PRIMARY KEY( username, run_id, sysno, iteration, wave, item_name ),
       foreign Key( item_name ) references available_items( name ));
       


       
 select username,run_id, wave, pid, mode( value ), median( cast (value as numeric)), stddev( value ), sum( value ), count( value ), avg( value ),max( value ), min( value ), iteration from personal_income where run_id='run_2011-12-01_17_15_27_run' group by username,run_id, wave, pid,iteration;
       
 
CREATE OR REPLACE FUNCTION _final_2nd( anyarray )
  RETURNS anyelement AS
$BODY$
    SELECT a
    FROM unnest( $1 ) a
    ORDER BY a
    offset 2
    LIMIT 1;
$BODY$
LANGUAGE 'sql' IMMUTABLE;
 
-- Tell Postgres how to use our aggregate
CREATE AGGREGATE g2nd(anyelement) (
  SFUNC=array_append, --Function to call for each row. Just builds the array
  STYPE=anyarray,
  FINALFUNC=_final_2nd, --Function to call after everything has been added to array
  INITCOND='{}' --Initialize an empty array when starting
);

drop aggregate g2nd( anyelement );


CREATE OR REPLACE FUNCTION _final_nth( anyarray, integer )
  RETURNS anyelement AS
$BODY$
    SELECT a
    FROM unnest( $1 ) a
    ORDER BY a
    offset $2
    LIMIT 1;
$BODY$
LANGUAGE 'sql' IMMUTABLE;


-- Tell Postgres how to use our aggregate
CREATE AGGREGATE nth_element( anyelement, integer ) (
  SFUNC=array_append, --Function to call for each row. Just builds the array
  STYPE=anyarray,
  FINALFUNC=_final_nth, --Function to call after everything has been added to array
  INITCOND='{}' --Initialize an empty array when starting
);


CREATE OR REPLACE FUNCTION _final_1st_decile(numeric[])
   RETURNS numeric AS
$$
   SELECT AVG(val)
   FROM (
     SELECT val
     FROM unnest($1) val
     ORDER BY 1
     LIMIT  2 - MOD(array_upper($1, 1), 2)
     OFFSET CEIL(array_upper($1, 1) / 10.0) - 1
   ) sub;
$$
LANGUAGE 'sql' IMMUTABLE;
 
CREATE AGGREGATE first_decile(numeric) (
  SFUNC=array_append,
  STYPE=numeric[],
  FINALFUNC=_final_1st_decile,
  INITCOND='{}'
);


CREATE OR REPLACE FUNCTION _final_last_decile(numeric[])
   RETURNS numeric AS
$$
   SELECT AVG(val)
   FROM (
     SELECT val
     FROM unnest($1) val
     ORDER BY 1 desc
     LIMIT  2 - MOD(array_upper($1, 1), 2)
     OFFSET CEIL(array_upper($1, 1) / 10.0) - 1
   ) sub;
$$
LANGUAGE 'sql' IMMUTABLE;
 
CREATE AGGREGATE last_decile(numeric) (
  SFUNC=array_append,
  STYPE=numeric[],
  FINALFUNC=_final_last_decile,
  INITCOND='{}'
);


