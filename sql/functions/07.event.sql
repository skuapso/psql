create function event.set_neighbours() returns trigger as $$
declare
  e timestamptz;
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
