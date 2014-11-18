create function checking.is_value_presents(_schema varchar, _table varchar, _column varchar, _value bigint) returns bool as $$
declare
  c bool;
begin
  if $4 is null then
    return true;
  end if;
  execute 'select true from ' || $1 || '.' || $2 || ' where ' || $3 || '=$1 limit 1' using $4 into c;
  return c is not null and c;
end $$ language plpgsql stable;
create function checking.is_value_presents(_schema varchar, _table varchar, _column varchar, _value bytea) returns bool as $$
declare
  c bool;
begin
  if $4 is null then
    return true;
  end if;
  execute 'select 1 from ' || $1 || '.' || $2 || ' where md5(' || $3 || ')=$1 limit 1' using md5($4) into c;
  return c is not null and c;
end $$ language plpgsql stable;
create function checking.is_value_presents(_schema varchar, _table varchar, _column varchar, _value varchar) returns bool as $$
declare
  c bool;
begin
  if $4 is null then
    return true;
  end if;
  execute 'select 1 from ' || $1 || '.' || $2 || ' where ' || $3 || '::varchar=$1 limit 1' using $4 into c;
  return c is not null and c;
end $$ language plpgsql stable;
create function checking.is_value_presents(_schema varchar, _table varchar, _column varchar, _value jsonb) returns bool as $$
declare
  c bool;
begin
  if $4 is null then
    return true;
  end if;
  execute 'select 1 from ' || $1 || '.' || $2 || ' where ' || $3 || '::jsonb=$1 limit 1' using $4 into c;
  return c is not null and c;
end $$ language plpgsql stable;
create function checking.is_value_presents(_schema varchar, _table varchar, _column varchar, _value timestamptz) returns bool as $$
declare
  c bool;
begin
  if $4 is null then
    return true;
  end if;
  execute 'select true from ' || $1 || '.' || $2 || ' where ' || $3 || '=$1 limit 1' using $4 into c;
  return c is not null and c;
end $$ language plpgsql stable;
create function checking.is_value_presents(_schema varchar, _table varchar, _columns varchar[], _values varchar[]) returns bool as $$
declare
  condition varchar;
  str varchar;
  c bool;
begin
  for i in array_lower($3, 1) .. array_upper($3, 1) loop
    str = $3[i] || '=' || $4[i];
    if condition is not null then
      condition = condition || ' and ' || str;
    else
      condition = str;
    end if;
  end loop;
  execute 'select true from ' || $1 || '.' || $2 || ' where ' || condition || ' limit 1' into c;
  return c is not null and c;
end $$ language plpgsql stable;
create function checking.is_array_in(_schema varchar, _table varchar, _column varchar, _array bigint[]) returns bool as $$
declare
  ar bigint[];
  c bool;
begin
  execute 'select array_agg(' || $3 || ') from ' || $1 || '.' || $2 || into ar;
  return $4<@ar;
end $$ language plpgsql stable;
