create function "option".get(_name varchar) returns varchar as $$
declare
  v varchar;
begin
  select value into v from options.data where name=$1;
  return v;
end $$ language plpgsql stable;
