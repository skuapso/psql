create type navigation.coords_gm as(
  degrees smallint,
  minutes float8
);

create function navigation.set_neighboring_events() returns trigger as $$
declare
  e bigint;
begin
  new.prev=event.search_prev(new.object_id, new.eventtime);
  if new.prev is null then
    new.next=event.search_next(new.object_id, new.eventtime);
  else
    new.next=event.next(new.prev);
  end if;
  return new;
end $$ language plpgsql;

create function navigation.set_prev_next_event2current() returns trigger as $$
begin
  update data.navigation set next=new.id where id=new.prev;
  return new;
end $$ language plpgsql;

create function navigation.set_next_prev_event2current() returns trigger as $$
begin
  update data.navigation set prev=new.id where id=new.next;
  return new;
end $$ language plpgsql;

create or replace function navigation.set_distance() returns trigger as $$
declare
  prev record;
begin
  if new.prev is null then
    new.distance = 0;
  else
    select * into prev from data.navigation where id=new.prev;
    new.distance = navigation.distance(
      new.latitude,  new.longitude,  new.altitude,
      prev.latitude, prev.longitude, prev.altitude);
  end if;
  return new;
end $$ language plpgsql;

create or replace function navigation.set_timediff() returns trigger as $$
declare
  prev record;
begin
  if new.prev is null then
    new.timediff = '0:0:0';
  else
    select * into prev from data.navigation where id=new.prev;
    new.timediff = navigation.timediff(
      new.eventtime, prev.eventtime);
  end if;
  return new;
end $$ language plpgsql;

create function navigation.timediff(t1 timestamp with time zone, t2 timestamp with time zone) returns interval as $$
begin
  return t1 - t2;
end $$ language plpgsql immutable;

create function navigation.distance(x1 float, y1 float, z1 float, x2 float, y2 float, z2 float) returns float as $$
declare
  pi float;
  r float;
  x1r float;
  y1r float;
  x2r float;
  y2r float;
  rad float;
begin
  pi = 3.14159265358979;
  r = 6371302;
  x1r = x1 * pi/180;
  y1r = y1 * pi/180;
  x2r = x2 * pi/180;
  y2r = y2 * pi/180;
  rad = r + (z1 + z2)/2;
  return sqrt(
    (y2r-y1r)*(y2r-y1r)*rad*rad*cos(x2r/2+x1r/2)*cos(x2r/2+x1r/2)
    + (x2r-x1r)*(x2r-x1r)*rad*rad
    + (z2-z1)*(z2-z1)
  );
end $$ language plpgsql immutable;
create function navigation.distance(x1 float, y1 float, x2 float, y2 float) returns float as $$
begin
  return navigation.distance(x1, y1, 0, x2, y2, 0);
end $$ language plpgsql immutable;
create function navigation.distance(x1 navigation.coords_gm, y1 navigation.coords_gm,
  x2 navigation.coords_gm, y2 navigation.coords_gm) returns float as $$
begin
  return navigation.distance(x1::float, y1::float, 0, x2::float, y2::float, 0);
end $$ language plpgsql immutable;
create function navigation.distance(x1 navigation.coords_gm, y1 navigation.coords_gm, z1 float,
  x2 navigation.coords_gm, y2 navigation.coords_gm, z2 float) returns float as $$
begin
  return navigation.distance(x1::float, y1::float, z1, x2::float, y2::float, z2);
end $$ language plpgsql immutable;
