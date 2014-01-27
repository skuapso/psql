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

create function navigation.distance(lon1 float, lat1 float, alt1 float, lon2 float, lat2 float, alt2 float) returns float as $$
declare
  pi float;
  earth_radius float;
  x1 float;
  y1 float;
  x2 float;
  y2 float;
  radius float;
begin
  pi = 3.14159265358979;
  earth_radius = 6371302;
  x1 = lon1 * pi/180;
  y1 = lat1 * pi/180;
  x2 = lon2 * pi/180;
  y2 = lat2 * pi/180;
  radius = earth_radius + (lat1 + lat2)/2;
  return sqrt(
    (x2-x1)*(x2-x1)*radius*radius*cos(y2/2+y1/2)*cos(y2/2+y1/2)
    + (y2-y1)*(y2-y1)*radius*radius
    + (alt2-alt1)*(alt2-alt1)
  );
end $$ language plpgsql immutable;
create function navigation.distance(x1 float, y1 float, x2 float, y2 float) returns float as $$
begin
  return st_distance(
    ('pointz(' || x1 || ' ' || y1 || ' 0)')::geography,
    ('pointz(' || x2 || ' ' || y2 || ' 0)')::geography
  );
end $$ language plpgsql immutable;
create function navigation.distance(x1 navigation.coords_gm, y1 navigation.coords_gm, x2 navigation.coords_gm, y2 navigation.coords_gm) returns float as $$
begin
  return navigation.distance($1::float, $2::float, $3::float, $4::float);
end $$ language plpgsql immutable;

create function navigation.distance(geography, geography) returns float as $$
begin
  return st_distance($1, $2);
end $$ language plpgsql immutable;
