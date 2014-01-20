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

create table navigation.test(
  id navigation.coords_gm
);
