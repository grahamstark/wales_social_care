--
--
CREATE OR REPLACE FUNCTION _final_range(numeric[])
   RETURNS numeric AS
$$
   SELECT MAX(val) - MIN(val)
   FROM unnest($1) val;
$$
LANGUAGE 'sql' IMMUTABLE;
 
-- Add aggregate
CREATE AGGREGATE range(numeric) (
  SFUNC=array_append, --Function to call for each row. Just builds the array
  STYPE=numeric[],
  FINALFUNC=_final_range, --Function to call after everything has been added to array
  INITCOND='{}' --Initialize an empty array when starting
);

--
-- median
--
CREATE OR REPLACE FUNCTION _final_median(numeric[])
   RETURNS numeric AS
$$
   SELECT AVG(val)
   FROM (
     SELECT val
     FROM unnest($1) val
     ORDER BY 1
     LIMIT  2 - MOD(array_upper($1, 1), 2)
     OFFSET CEIL(array_upper($1, 1) / 2.0) - 1
   ) sub;
$$
LANGUAGE 'sql' IMMUTABLE;
 
CREATE AGGREGATE median(numeric) (
  SFUNC=array_append,
  STYPE=numeric[],
  FINALFUNC=_final_median,
  INITCOND='{}'
);

--
-- range
--
CREATE OR REPLACE FUNCTION _final_range(numeric[])
   RETURNS numeric AS
$$
   SELECT MAX(val) - MIN(val)
   FROM unnest($1) val;
$$
LANGUAGE 'sql' IMMUTABLE;
 
-- Add aggregate
CREATE AGGREGATE range(numeric) (
  SFUNC=array_append, --Function to call for each row. Just builds the array
  STYPE=numeric[],
  FINALFUNC=_final_range, --Function to call after everything has been added to array
  INITCOND='{}' --Initialize an empty array when starting
);

create rule "replace_key_value" as
    on insert to "key_value_parameter"
    where
      exists( select 1 from key_value_parameter where username=new.username and run_id = new.run_id and key = new.key )
    do instead
       ( update key_value_parameter set val=new.val where username=new.username and run_id = new.run_id and key=new.key );
       
CREATE OR REPLACE FUNCTION _final_nth( an anyarray, pos integer )
  RETURNS anyelement AS
$BODY$                                                                      
    SELECT a
    FROM unnest( $1 ) a                                                            
    ORDER BY a                                           
    offset $2
    LIMIT 1;
$BODY$
LANGUAGE 'sql' IMMUTABLE;

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

drop table table_stats;
drop view table_stats;
create or replace view table_stats
as
select run_id,
       username,
       model_table_name,
       row_num,
       col_num,
       wave,
       system_number,
       count( 1 ) as nvalues, 
       avg( value1 ) as rmean_1,
       min( value1 ) as rmin_1,
       max( value1 ) as rmax_1, 
       median( cast ( value1 as numeric )) as rmed_1,
       range( cast ( value1 as numeric )) as rranage_1,
       stddev( cast ( value1 as numeric )) as sddev_1,
       first_decile( cast ( value1 as numeric )) as dec1_1,
       last_decile( cast ( value1 as numeric )) as dec10_1,

       avg( value2 ) as rmean_2,
       min( value2 ) as rmin_2,
       max( value2 ) as rmax_2, 
       median( cast ( value2 as numeric )) as rmed_2,
       range( cast ( value2 as numeric )) as rranage_2,
       stddev( cast ( value2 as numeric )) as sddev_2,
       first_decile( cast ( value2 as numeric )) as dec1_2,
       last_decile( cast ( value2 as numeric )) as dec10_2,

       avg( value3 ) as rmean_3,
       min( value3 ) as rmin_3,
       max( value3 ) as rmax_3, 
       median( cast ( value3 as numeric )) as rmed_3,
       range( cast ( value3 as numeric )) as rranage_3,
       stddev( cast ( value3 as numeric )) as sddev_3,
       first_decile( cast ( value3 as numeric )) as dec1_3,
       last_decile( cast ( value3 as numeric )) as dec10_3,
       
       avg( value4 ) as rmean_4,
       min( value4 ) as rmin_4,
       max( value4 ) as rmax_4, 
       median( cast ( value4 as numeric )) as rmed_4,
       range( cast ( value4 as numeric )) as rranage_4,
       stddev( cast ( value4 as numeric )) as sddev_4,
       first_decile( cast ( value4 as numeric )) as dec1_4,
       last_decile( cast ( value4 as numeric )) as dec10_4,
       
       avg( value5 ) as rmean_5,
       min( value5 ) as rmin_5,
       max( value5 ) as rmax_5, 
       median( cast ( value5 as numeric )) as rmed_5,
       range( cast ( value5 as numeric )) as rranage_5,
       stddev( cast ( value5 as numeric )) as sddev_5,
       first_decile( cast ( value5 as numeric )) as dec1_5,
       last_decile( cast ( value5 as numeric )) as dec10_5,
       
       avg( value6 ) as rmean_6,
       min( value6 ) as rmin_6,
       max( value6 ) as rmax_6, 
       median( cast ( value6 as numeric )) as rmed_6,
       range( cast ( value6 as numeric )) as rranage_6,
       stddev( cast ( value6 as numeric )) as sddev_6,
       first_decile( cast ( value6 as numeric )) as dec1_6,
       last_decile( cast ( value6 as numeric )) as dec10_6
       
from 
       disaggregated_data_table_cell 
group by 
       username, run_id, model_table_name, wave, row_num, col_num, system_number 
order by 
       username,run_id,model_table_name,length( wave ),wave,system_number,row_num,col_num;

