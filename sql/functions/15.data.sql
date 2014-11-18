create type data.types as enum(
  'unknown',
  'broken',
  'ping',
  'low',
  'offline',
  'online',
  'high',
  'panic',
  'security',
  'authentication',
  'command'
);

create type connections.types as enum(
  'ip'
);

create function data.binary_id(
  _id timestamptz,
  _data bytea)
returns timestamptz
as $$
declare
  i timestamptz;
begin
  loop
    select data_id::timestamptz into i from data.binary where md5(data)=md5($2) limit 1;
    if found then
      return i;
    end if;
    begin
      insert into data.binary values ($1::bigint, $2) returning data_id::timestamptz into i;
    exception when unique_violation then
    end;
  end loop;
  return i;
end $$ language plpgsql;

create function data.add_broken(
  _id timestamptz,
  _data bytea,
  _connection_id timestamptz)
returns setof timestamptz
as $$
begin
  return query
  insert into data.broken
  select $1, data.binary_id($1, $2), $3, false
  returning id::bigint;
end $$ language plpgsql;

create function data.add_raw(
  _id timestamptz,
  _data bytea,
  _connection_id timestamptz)
returns setof timestamptz
as $$
begin
  return query
  with
    data as (
      select data.binary_id($1, $2) as id),
    br as (
      insert into data.broken
      select $1, id, $3, true
      from data
      where id<>$1)
  insert into data.raws
  select $1, id, $3
  from data
  where id=$1
  returning id::timestamptz;
end $$ language plpgsql;

create function data.add_packet(
  _id timestamptz,
  _data bytea,
  _raw_id timestamptz,
  _type data.types,
  _eventtime timestamptz,
  _desc jsonb)
returns setof timestamptz
as $$
begin
  return query
  with
    raw as (
      select id
      from data.raws
      where id=$3::bigint),
    craw as (
      select count(*) from raw),
    data as (
      select data.binary_id($1, $2) as id
      from craw
      where craw.count>0)
    insert into data.packets
    select $1, data.id, raw.id, $4, $5, $6
    from data
    inner join raw on(true)
    inner join craw on(true)
    where craw.count>0
    returning id::timestamptz;
end $$ language plpgsql;

create function data.set_answer(
  _id timestamptz,
  _data bytea,
  _answered varchar
) returns setof timestamptz
as $$
begin
  return query
  with
    data as (
      select data.binary_id(now(), $2) as id)
  update data.raws r
  set (answer_id, answered)=(d.id, $3)
  from data d
  where r.id=$1::bigint
  returning r.id::timestamptz;
end $$ language plpgsql;

create function data.merge(
  a1 jsonb,
  a2 jsonb
) returns jsonb
as $$
  return plv8.extend(a1, a2);
$$ language plv8;

create function data.compute(
  object_id bigint,
  sensor_id bigint,
  value jsonb
) returns jsonb
as $$
declare
  val jsonb;
  fun varchar;
begin
  fun = sensor.compute_fun($2);
  if fun is null then
    return $3;
  end if;
  if $3 is null then
    return null;
  end if;
  execute 'select ' || fun || '($1, $2, $3)' using $1, $2, $3 into val;
  return val;
end $$ language plpgsql stable strict;
