create function object.terminal(_object_id bigint, _time timestamptz) returns bigint as $$
declare
  i bigint;
begin
  select terminal_id
  into i
  from objects.data
  where id=$1
  limit 1;
  return i;
end $$ language plpgsql immutable;

create function object.terminal(_object_id bigint) returns bigint as $$
begin
  return object.terminal(_object_id, current_timestamp);
end $$ language plpgsql stable;

create function object.model_title(_model_id bigint) returns varchar as $$
declare
  t varchar;
begin
  select title into t from objects.models where id=$1;
  return t;
end $$ language plpgsql stable;

create function object.model(_object_id bigint) returns bigint as $$
declare
  m bigint;
begin
  select model_id into m from objects.data where id=$1;
  return m;
end $$ language plpgsql stable;

create function object.title(_object_id bigint) returns varchar as $$
declare
  t varchar;
begin
  select object.model_title(object.model(id)) || ' ' || no into t from objects.data where id=$1;
  return t;
end $$ language plpgsql stable;

create function object.specialization(_object_id bigint) returns bigint as $$
declare
  s bigint;
begin
  select specialization_id into s from objects.data where id=$1;
  return s;
end $$ language plpgsql stable;

create function object.location(_object_id bigint, _eventtime timestamp with time zone) returns table(
  id bigint,
  eventtime timestamp with time zone,
  latitude float,
  longitude float,
  altitude float) as $$
begin
  return query
    select Q.id,Q.eventtime,Q.latitude,Q.longitude,Q.altitude from data.navigation
    where object_id=$1 and eventtime<=$2 order by eventtime desc limit 1;
end $$ language plpgsql stable;

create function object.group(_object_id bigint) returns bigint as $$
declare
  g bigint;
begin
  select group_id into g from objects.data where id=$1;
  return g;
end $$ language plpgsql stable;

create function object.last_event_id(_object_id bigint) returns timestamptz as $$
declare
  e timestamptz;
begin
  select last_event_id into e from objects.data where id=$1;
  return e;
end $$ language plpgsql stable;

create function object.json(_group_id bigint) returns json as $$
declare
  r record;
begin
  select
    'object' as type,
    id,
    terminal_id,
    no,
    specialization_id,
    model_id,
    group_id
  into r
  from objects.data where id=$1;
  return row_to_json(r);
end $$ language plpgsql stable;
