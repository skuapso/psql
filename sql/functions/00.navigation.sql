do $$begin raise warning 'navigation.coords_gm needed for m2m replication'; end$$;
create type navigation.coords_gm as(
  degrees smallint,
  minutes float8
);

create function navigation.coords2float4(navigation.coords_gm) returns real as $$
begin
  return (-1)^(($1.degrees<0)::int) * (@ $1.degrees + $1.minutes/60.0);
end $$ language plpgsql immutable;

create function navigation.coords2float8(navigation.coords_gm) returns float8 as $$
begin
  return (-1)^(($1.degrees<0)::int) * (@ $1.degrees + $1.minutes/60.0);
end $$ language plpgsql immutable;

create cast(navigation.coords_gm as real) with function
  navigation.coords2float4(navigation.coords_gm);

create cast(navigation.coords_gm as float8) with function
  navigation.coords2float8(navigation.coords_gm);

create function navigation.distance(geography, geography) returns float as $$
begin
  return (6371008.77141506 + (navigation.z($1) + navigation.z($2))/2) / 6371008.77141506
          * st_distance($1, $2, false);
end $$ language plpgsql immutable;

create function navigation.distance(x1 float, y1 float, x2 float, y2 float) returns float as $$
begin
  return navigation.distance(
    ('point(' || x1 || ' ' || y1 || ')')::geography,
    ('point(' || x2 || ' ' || y2 || ')')::geography
  );
end $$ language plpgsql immutable;

create function navigation.distance(lon1 float, lat1 float, alt1 float, lon2 float, lat2 float, alt2 float) returns float as $$
begin
  return navigation.distance(
    ('point(' || lon1 || ' ' || lat1 || ' ' || alt1 || ')')::geography,
    ('point(' || lon2 || ' ' || lat2 || ' ' || alt2 || ')')::geography
  );
end $$ language plpgsql immutable;

create function navigation.distance(x1 navigation.coords_gm, y1 navigation.coords_gm, x2 navigation.coords_gm, y2 navigation.coords_gm) returns float as $$
begin
  return navigation.distance($1::float, $2::float, $3::float, $4::float);
end $$ language plpgsql immutable;

create function navigation.distance(jsonb, jsonb) returns float as $$
begin
  return navigation.distance($1::geography, $2::geography);
end $$ language plpgsql immutable;

create function navigation.x(geography) returns float8 as $$
begin
  return st_x($1::geometry);
end $$ language plpgsql immutable strict;

create function navigation.y(geography) returns float8 as $$
begin
  return st_y($1::geometry);
end $$ language plpgsql immutable strict;

create function navigation.z(geography) returns float8 as $$
begin
  return case when st_z($1::geometry) is null then 0
              else st_z($1::geometry)
          end;
end $$ language plpgsql immutable strict;

create type navigation.part_type as(
  current bool
  ,part_no bigint
);

create function navigation.part_state(navigation.part_type, bool) returns navigation.part_type as $$
begin
  if $2 is not null and $1.current<>$2 then
    $1.part_no = $1.part_no + 1;
    $1.current = $2;
  end if;
  return $1;
end $$ language plpgsql immutable;

create function navigation.part_fin(navigation.part_type) returns bigint as $$
begin
  return $1.part_no;
end $$ language plpgsql immutable;

create aggregate navigation.part(bool)(
  sfunc = navigation.part_state
  ,stype = navigation.part_type
  ,finalfunc = navigation.part_fin
  ,initcond = '(false,0)'
);

create function navigation.to_geography(_location jsonb) returns geography as $$
declare
  alt text := $1->>'altitude';
begin
  if alt is null then
    alt = '0';
  end if;
  return (
    'POINTZ('
      || ($1->>'longitude')
      || ' '
      || ($1->>'latitude')
      || ' '
      || alt
      || ')'
  )::geography;
end $$ language plpgsql immutable strict;

create function navigation.get(location jsonb, param varchar) returns float as $$
  var o = JSON.parse(location);
  return plv8.ll_convert(o, param);
$$ language plv8 immutable strict;

create function navigation.get(location json, param varchar) returns float as $$
  select navigation.get($1::jsonb, $2);
$$ language sql immutable strict;

create function navigation.to_jsonb(_location geography) returns jsonb as $$
begin
  return
    case
      when navigation.x($1) then
        null
    else
      json_build_object(
        'longitude', navigation.x($1),
        'latitude', navigation.y($1),
        'altitude', navigation.z($1)
      )
    end;
end $$ language plpgsql immutable;

drop cast if exists (jsonb as geography);
drop cast if exists (geography as jsonb);
create cast (jsonb as geography) with function navigation.to_geography(jsonb) as implicit;
create cast (geography as jsonb) with function navigation.to_jsonb(geography);
