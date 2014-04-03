create function _user.exists(name text) returns boolean as $$
begin
  select 1 from pg_catalog.pg_user where usename=$1;
  return found;
end $$ language plpgsql stable;
