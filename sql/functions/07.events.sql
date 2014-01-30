create function events.timediff(e1 timestamptz, e2 timestamptz)
returns interval as $$
begin
  return event.time($1) - event.time($2);
end $$ language plpgsql stable;
