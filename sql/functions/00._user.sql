create function _user.exists(name text, can_login boolean = true) returns boolean as $$
begin
  if $2 then
    perform 1 from pg_catalog.pg_user where usename=$1;
  else
    perform 1 from pg_catalog.pg_roles where rolname=$1;
  end if;
  return found;
end $$ language plpgsql stable;
