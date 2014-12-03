create sequence gis.seq_data;

create table gis._geography(
  data_id bigint
    constraint "_geography(id)" primary key
    default nextval('gis.seq_data')
  ,data geography
    not null
  ,osm_id bigint
);
create unique index "_geography uniq(geography)" on gis._geography(md5(data::bytea));
create index "_geography(data)" on gis._geography using gist(data);
create index "_geography(osm_id)" on gis._geography(osm_id);

create table gis._parents(
  data_id bigint
    constraint "_parents(data_id)->_geography" references gis._geography(data_id)
    not null
  ,parent_id bigint
    constraint "_parents(parent_id)->_geography(data_id)" references gis._geography(data_id)
    not null
);

create table gis._data(
  id bigint
    constraint "data pk(id)" primary key
    default nextval('gis.seq_data')
  ,name varchar
    not null
  ,class gis.types
    not null
);
create table gis._rels(
  id bigint
    not null
    constraint "_rel(id)->_data" references gis._data(id)
  ,data_id bigint
    not null
    constraint "_rel(data_id)->_geography" references gis._geography(data_id)
);
create unique index "_rel unique(class(id), data_id)" on gis._rels(gis.class(id), data_id);
create index "_rel(id)" on gis._rels(id);
create index "_rel(data_id)" on gis._rels(data_id);

create view gis.data as
select *
from gis._data D
join gis._rels R using(id)
join gis._geography G using(data_id);
