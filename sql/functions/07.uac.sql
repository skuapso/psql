create function uac.check_group_parents() returns trigger as $$
begin
  perform 1 from uac.groups
  where array[group_id]<@(new.group_id || "group".parents(new.group_id))
  and user_name=new.user_name;
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

create function uac.can_read_group(_group_id bigint) returns boolean as $$
begin
  return uac.can_read_group(current_user, $1);
end $$ language plpgsql stable;

create function uac.can_read_group(_user name, _group_id bigint, _root bool=true)
returns boolean
as $$
begin
  perform true from uac.groups
  where array[user_name]<@uac.roles($1)
  and (
    ($3 and group_id is null)
    or group_id=$2
    or array[group_id]<@"group".parents($2)
  )
  limit 1;
  return found;
end $$ language plpgsql stable security definer;
