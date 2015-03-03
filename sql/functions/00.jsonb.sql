create function jsonb.extend(jsonb, jsonb, varchar[]) returns jsonb as $$
  select jsonb_deep_extend($1, $2);
$$ language sql immutable strict;

create function jsonb.extend(anyelement, anyelement, varchar[] default '{}') returns jsonb as $$
  select jsonb.extend($1::jsonb, $2::jsonb, $3)
$$ language sql immutable strict;

create function jsonb.extend(jsonb, anyelement, varchar[] default '{}') returns jsonb as $$
  select jsonb.extend($1, $2::jsonb, $3)
$$ language sql immutable strict;

create function jsonb.extend(anyelement, jsonb, varchar[] default '{}') returns jsonb as $$
  select jsonb.extend($1::jsonb, $2, $3)
$$ language sql immutable strict;

create function jsonb.extend(jsonb, jsonb) returns jsonb as $$
  select jsonb.extend($1, $2, '{}'::varchar[]);
$$ language sql immutable strict;

create function jsonb.path(_path jsonb, text[] default '{}') returns text[] as $$
declare
  k text;
  v jsonb;
begin
  k = jsonb_typeof($1);
  if k = 'object' then
    select key,value into k,v from jsonb_each($1) limit 1;
    return jsonb.path(v, $2 || k);
  else
    return $2 || ($1->>0);
  end if;
end $$ language plpgsql immutable strict;
