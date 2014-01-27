create function checking.is_value_presents(_schema varchar, _table varchar, _column varchar, _value bigint) returns bool as $$
declare
  c bigint;
begin
  if $4 is null then
    return true;
  end if;
  execute 'select ' || $3 || ' from ' || $1 || '.' || $2 || ' where ' || $3 || '=' || $4 || ' limit 1' into c;
  if c=$4 then
    return true;
  end if;
  return false;
end $$ language plpgsql stable;
create function checking.is_value_presents(_schema varchar, _table varchar, _column varchar, _value bytea) returns bool as $$
declare
  s varchar;
  c bytea;
begin
  if $4 is null then
    return true;
  end if;
  s = md5($4);
  execute 'select ' || $3 || ' from ' || $1 || '.' || $2 || ' where md5(' || $3 || E')=\'' || s || E'\' limit 1' into c;
  if c is not null then
    return true;
  end if;
  return false;
end $$ language plpgsql stable;
create function checking.is_value_presents(_schema varchar, _table varchar, _column varchar, _value timestamptz) returns bool as $$
declare
  c timestamptz;
begin
  if $4 is null then
    return true;
  end if;
  execute 'select ' || $3 || ' from ' || $1 || '.' || $2 || ' where ' || $3 || E'=\'' || $4 || E'\' limit 1' into c;
  if c=$4 then
    return true;
  end if;
  return false;
end $$ language plpgsql stable;
create function checking.is_value_presents(_schema varchar, _table varchar, _columns varchar[], _values varchar[]) returns bool as $$
declare
  condition varchar;
  str varchar;
  i bigint;
begin
  for i in array_lower($3, 1) .. array_upper($3, 1) loop
    str = $3[i] || '=' || $4[i];
    if condition is not null then
      condition = condition || ' and ' || str;
    else
      condition = str;
    end if;
  end loop;
  execute 'select 1 from ' || $1 || '.' || $2 || ' where ' || condition || ' limit 1' into i;
  if i is not null then
    return true;
  end if;
  return false;
end $$ language plpgsql stable;

create function checking.is_data_exists(_object_id bigint, _eventtime timestamp with time zone) returns bool as $$
begin
  perform id from data.navigation where object_id=$1 and eventtime=$2;
  if found then
    return true;
  else
    return false;
  end if;
end $$ language plpgsql stable;
