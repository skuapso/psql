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

create function object.sensor(_object_id bigint, _port_type terminal.port_types, _port_id varchar)
returns bigint as $$
declare
  sid bigint;
begin
  select id into sid from objects.sensors
  where object_id=$1
  and sensor.port_type(sensor_id)=$2
  and port_id=$3;
  return sid;
end $$ language plpgsql stable;

create function object.sensor(_object_id bigint, _object_sensor_id bigint) returns bigint as $$
declare
  sid bigint;
begin
  select sensor_id into sid from objects.sensors where object_id=$1 and id=$2;
  return sid;
end $$ language plpgsql stable;

create function object.sensor(_object_sensor_id bigint) returns bigint as $$
declare
  sid bigint;
begin
  select sensor_id into sid from objects.sensors where id=$1;
  return sid;
end $$ language plpgsql stable;

create function object.notify_create() returns trigger as $$
begin
  perform triggers.notify('ui',
    'create group ' || new.group_id || ' '
    || tg_table_schema || ' ' || tg_table_name || ' ' || new.id);
  return new;
end $$ language plpgsql;

create function object.notify_delete() returns trigger as $$
begin
  perform triggers.notify('ui',
    'delete object ' || old.id || ' '
    || tg_table_schema || ' ' || tg_table_name);
  return new;
end $$ language plpgsql;

create function object.notify_update() returns trigger as $$
begin
  perform
    triggers.notify('ui', 'delete user ' || user_name || ' object ' || new.id),
    triggers.notify('ui', 'delete user ' || user_name || ' terminal ' || new.terminal_id)
  from _users.data
  where uac.can_read_group(user_name, old.group_id)
  and not uac.can_read_group(user_name, new.group_id);

  perform triggers.notify('ui', 'update object ' || new.id || ' '
    || tg_table_schema || ' ' || tg_table_name)
  from _users.data
  where uac.can_read_group(user_name, old.group_id)
  and uac.can_read_group(user_name, new.group_id)
  limit 1;

  perform triggers.notify('ui', 'create user ' || user_name || ' '
    || tg_table_schema || ' ' || tg_table_name || ' ' || new.id)
  from _users.data
  where not uac.can_read_group(user_name, old.group_id)
  and uac.can_read_group(user_name, new.group_id);
  return new;
end $$ language plpgsql;

create function object.track(
  _object_id bigint,
  _from timestamptz,
  _to timestamptz
) returns setof jsonb
as $$
begin
  return query
  select row_to_json(S1.*)::jsonb as jsons from (
    select
      array_to_json(array_agg(loc_json)) as track,
      object_id,
      min(time),
      max(time)
    from (
      select
        json_build_object('location', data->'location', 'eventtime', time) loc_json,
        time,
        object_id
       from events.data as ev
       where valid
       and object_id=$1 and time>=$2 and time<=$3
       order by time
    ) S3
    group by object_id
  ) S1;
end $$ language plpgsql stable;
