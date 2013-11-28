create function event.prev(_event_id bigint) returns bigint as $$
declare
  e bigint;
begin
  select prev into e from data.navigation where id=$1;
  return e;
end $$ language plpgsql stable;

create function event.next(_event_id bigint) returns bigint as $$
declare
  e bigint;
begin
  select next into e from data.navigation where id=$1;
  return e;
end $$ language plpgsql stable;

create function event.search_prev(_object_id bigint, _eventtime timestamp with time zone) returns bigint as $$
declare
  e bigint;
begin
  select id into e
  from data.navigation
  where eventtime<=$2 and object_id=$1
  order by eventtime desc limit 1;
  return e;
end $$ language plpgsql stable;

create function event.search_next(_object_id bigint, _eventtime timestamp with time zone) returns bigint as $$
declare
  e bigint;
begin
  select id into e
  from data.navigation
  where eventtime>=$2 and object_id=$1
  order by eventtime asc limit 1;
  return e;
end $$ language plpgsql stable;
