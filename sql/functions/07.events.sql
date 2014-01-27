create function events.timediff(t1 timestamp with time zone, t2 timestamp with time zone)
returns interval as $$
begin
  return t1 - t2;
end $$ language plpgsql immutable;

create function event.set_not_valid() returns trigger as $$
begin
  new.valid = false;
  return new;
end $$ language plpgsql;
