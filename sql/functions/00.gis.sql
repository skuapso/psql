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
