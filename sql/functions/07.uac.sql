create function uac.check_group_parents() returns trigger as $$
begin
  select id from uac.groups where array[group_id]<@"group".parents(new.group_id);
  if found then
    return null;
  end if;
  return new;
end $$ language plpgsql stable;

create function uac.role_id(name) returns oid as $$
declare
  i oid;
begin
  select oid into i from pg_roles where rolname=$1;
  return i;
end $$ language plpgsql stable;

create function uac.roles(name) returns name[] as $$
begin
  return $1 || array(select b.rolname
                from pg_catalog.pg_auth_members m
                join pg_catalog.pg_roles b on (m.roleid = b.oid)
                where m.member = uac.role_id($1)
              );
end $$ language plpgsql stable;
