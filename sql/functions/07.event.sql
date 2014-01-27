create function event.set_neighbours() returns trigger as $$
declare
  e timestamptz;
begin
  e = object.last_event_id(new.object_id);
  if e is null then
    return new;
  end if;
  if new.time > event.time(e) then
    new.prev = e;
  else
    select id into new.next from events.data
      where object_id=new.object_id
      and time>new.time
      order by time
      limit 1;
    select prev into new.prev from events.data
      where id=new.next;
  end if;
  return new;
end $$ language plpgsql;

create function event.update_prev() returns trigger as $$
begin
  update events.data set next=new.id where id=new.prev;
  return new;
end $$ language plpgsql;

create function event.update_next() returns trigger as $$
begin
  update events.data set prev=new.id where id=new.next;
  return new;
end $$ language plpgsql;

create function event.update_object() returns trigger as $$
begin
  update objects.data set last_event_id=new.id where id=new.object_id;
  return new;
end $$ language plpgsql;

create function event.time(_event_id timestamptz) returns timestamptz as $$
declare
  t timestamptz;
begin
  select time into t from events.data where id=$1;
  return t;
end $$ language plpgsql stable;

create function event.location(_event_id timestamptz) returns geography(pointz, 4326) as $$
declare
  p geometry(pointz, 4326);
begin
  select location into p from events.data where id=$1;
  return p;
end $$ language plpgsql stable;
