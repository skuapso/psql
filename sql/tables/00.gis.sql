create sequence gis.seq_data;

create table gis.data(
  id bigint
    constraint "data(id)" primary key
    default nextval('gis.seq_data')
  ,osm_id bigint
    not null
  ,name varchar
  ,geography geography
    not null
  ,class gis.types
    not null
);
create index "data(geography)" on gis.data using gist (geography);
