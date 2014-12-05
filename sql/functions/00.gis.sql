create type gis.types as enum(
  'milestone', 'park', 'road',
  'village', 'town', 'city',
  'county', 'region', 'country');

create function gis.osm(_id bigint)
returns setof jsonb
as $$
begin
  return query
  select jsonb.extend(
    st_asGeoJson(way)::jsonb,
    json_build_object('name', name)
  )
  from gis.data
  where osm_id=$1;
end $$ language plpgsql stable;

create function gis.intersects(geo geography)
returns setof varchar
as $$
begin
  return query
  select name
  from gis.data
  where st_intersects(geography, $1);
end $$ language plpgsql immutable;

create function gis.class(_id bigint)
returns gis.types
as $$
begin
  return (select class from gis._data where id=$1 limit 1);
end $$ language plpgsql immutable;

create function gis.geography_id(_data geography)
returns bigint
as $$
declare
  i bigint;
begin
  loop
    select data_id into i from gis._geography where md5(data)=md5($1::bytea);
    if found then
      return i;
    end if;
    begin
      insert into gis._geography(data) values($1);
    exception when unique_violation then
    end;
  end loop;
end $$ language plpgsql strict;

create function gis.geography_id(_osm_id bigint)
returns bigint[]
as $$
begin
  insert into gis._geography(data, osm_id)
    select
      way,$1
    from
      (
        select way from (
          select distinct
            case
              when geometryType(way) = 'LINESTRING' and st_isClosed(way)
                then st_makePolygon(way)
              else
                way
            end as way
          from (
            select way
            from ru_polygon
            where osm_id=$1
            union all
            select way
            from ru_line
            where osm_id=$1
          ) S1
        ) O
        left join lateral (
          select true as present
          from gis._geography G
          where md5(g.data::bytea)=md5(O.way::bytea)
        ) S on true
        where
          present is null
      ) S2
    where way is not null;
  return (select array_agg(data_id) from gis._geography where osm_id=$1);
end $$ language plpgsql strict;

do $$begin raise warning 'some roads in two counties => no county'; end$$;
create or replace function gis.places(_point geography)
returns bigint[]
as $$
begin
  return array(
    with
      g as (select $1 as point, st_buffer($1, 50) as circle),
      pp as (
        select data_id,data,class from gis.data join g on (g.circle && data) order by class
      ),
      place as (
        select pp.data_id
        from pp,g
        where (geometryType(pp.data)='LINESTRING' and st_intersects(pp.data, g.circle))
          or (geometrytype(pp.data)='POLYGON' and st_covers(pp.data, g.point))
        limit 1
      )
    select id from
    gis.data
    join (
      select data_id from place
      union all
      select parent_id from gis._parents join place using(data_id)
    ) S using(data_id)
    group by id,class
    order by class
  );
end $$ language plpgsql stable strict;
