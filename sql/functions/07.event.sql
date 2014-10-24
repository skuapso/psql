create function event.set_neighbours() returns trigger as $$
declare
  e bigint;
begin
  e = object.last_event_id(new.object_id);
  if e is null then
    return new;
  end if;
  if new.time < event.time(e) then
    select id,prev into new.next,new.prev from events.data D
      where object_id=new.object_id
      and valid
      and D.time > new.time
      order by time
      limit 1;
  else
    new.prev = e;
  end if;
  update events.data set next=new.id where id=new.prev;
  update events.data set prev=new.id where id=new.next;
  return new;
end $$ language plpgsql;

create function event.update_object() returns trigger as $$
begin
  update objects._data set last_event_id=new.id where id=new.object_id;
  return new;
end $$ language plpgsql;

create function event.time(_event_id bigint) returns timestamptz as $$
declare
  t timestamptz;
begin
  select time into t from events.data where id=$1::bigint;
  return t;
end $$ language plpgsql stable;

create function event.location(_event_id timestamptz) returns geography as $$
declare
  p geography;
begin
  select location into p from events.data where id=$1;
  return p;
end $$ language plpgsql stable;

create function event.set_not_valid() returns trigger as $$
begin
  new.valid = false;
  return new;
end $$ language plpgsql;

create function event.upload()
returns trigger
as $$
begin
  insert into events._data (
    id,
    type,
    object_id,
    terminal_id,
    time,
    data)
  select
    new.id,
    new.type,
    packet.object(new.raw_id, true),
    packet.terminal(new.raw_id, true),
    new.eventtime,
    event.prepare_data(new.data);
  return new;
end $$ language plpgsql;

create function event.merge_data()
returns trigger
as $$
begin
  if new.prev is not null then
    new.data = jsonb.extend(event.data(new.prev), new.data, array['used']::varchar[]);
  end if;
  if new.next is not null then
    update events._data
    set data=jsonb.extend(new.data, event.data(new.next), array['used']::varchar[])
    where id=new.next;
  end if;
  return new;
end $$ language plpgsql;

create function event.data(_id bigint) returns setof jsonb as $$
begin
  return query
  select data
  from events._data
  where id=$1;
end $$ language plpgsql stable strict;

create function event.prepare_data(data jsonb) returns jsonb as $$
  data = JSON.parse(data);
  var loc;
  if (data.used < 4) {
    data = plv8.extend({}, data, ['location']);
  } else {
    loc = data.location;
    if (loc) {
      loc.latitude = plv8.ll_convert(loc, 'latitude');
      loc.longitude = plv8.ll_convert(loc, 'longitude');
    }
  }
  return JSON.stringify(data);
$$ language plv8 stable strict;

create function event.delete(_id bigint)
returns bool
as $$
declare
  p bigint;
  n bigint;
begin
  select prev,next into p,n from events.data where id=$1;

  update events.data set valid=false, prev=null, next=null where id=$1;
  update events.data set next=n where id=p;
  update events.data set prev=p where id=n;
  return true;
end $$ language plpgsql;

create function event.delete(_object_id bigint, _time timestamp)
returns bool
as $$
declare
  i bigint;
begin
  select id into i from events.data where object_id=$1 and time=$2 and valid;
  return event.delete(i);
end $$ language plpgsql;
