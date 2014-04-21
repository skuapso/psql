create function "group".title(_group_id bigint) returns varchar as $$
declare
  t varchar;
begin
  select title into t from objects.groups where id=$1;
  return t;
end $$ language plpgsql stable;

create or replace function "group".childs(_start_group_id bigint) returns bigint[] as $$
declare
  children bigint[];
  parents bigint[];
  child bigint[];
  i bigint;
begin
  children := array[]::bigint[];
  parents := $1 || array[]::bigint[];

  loop
    child := array[]::bigint[];
    for i in select unnest (parents) loop
      if i is null then
        child := child || array (select id from objects.groups where parent_id is null);
      else
        child := child || array (select id from objects.groups where parent_id=i);
      end if;
    end loop;
    exit when child = array[]::bigint[];
    parents := child;
    children := children || child;
  end loop;
  return children;
end $$ language plpgsql stable;

create or replace function "group".count_objects(_group_id bigint) returns bigint as $$
declare
  c bigint;
begin
  select count(*) into c from objects.data where group_id=$1;
  return c;
end $$ language plpgsql stable;

create or replace function "group".parent(_group_id bigint) returns bigint as $$
declare
  p bigint;
begin
  select id into p
  from objects._groups
  where id in (
    select parent_id
    from objects._groups
    where id=$1
  );
  return p;
end $$ language plpgsql stable security definer;

create or replace function "group".parents(_group_id bigint) returns bigint[] as $$
declare
  parents bigint[];
  current_group bigint;
begin
  parents = array[]::bigint[];
  current_group = "group".parent($1);
  loop
    exit when current_group is null;
    parents = current_group || parents;
    current_group = "group".parent(current_group);
  end loop;
  return parents;
end $$ language plpgsql stable;

create or replace function "group".json(_group_id bigint) returns json as $$
declare
  r record;
begin
  select 'group' as type, id, title, parent_id, owner_id into r from objects.groups where id=$1;
  return row_to_json(r);
end $$ language plpgsql stable;

create or replace function "group".notify_update() returns trigger as $$
begin
  perform triggers.notify('ui',
    'create user ' || user_name || ' '
    || tg_table_schema || ' ' || tg_table_name || ' ' || new.id)
  from _users.data
  where not uac.can_read_group(user_name, old.parent_id)
  and uac.can_read_group(user_name, new.parent_id);

  perform triggers.notify('ui',
    'delete user ' || user_name || ' group ' || old.id)
  from _users.data
  where uac.can_read_group(user_name, old.parent_id)
  and not uac.can_read_group(user_name, new.parent_id);

  perform triggers.notify('ui',
    'update group ' || new.id || ' ' || tg_table_schema || ' ' || tg_table_name)
  from _users.data
  where uac.can_read_group(user_name, old.parent_id)
  and uac.can_read_group(user_name, new.parent_id)
  limit 1;

  return new;
end $$ language plpgsql;

create or replace function "group".notify_create() returns trigger as $$
begin
  perform triggers.notify('ui',
    'create group ' || new.parent_id || ' '
    || tg_table_schema || ' ' || tg_table_name || ' ' || new.id);
  return new;
end $$ language plpgsql;

create or replace function "group".notify_delete() returns trigger as $$
begin
  perform triggers.notify('ui',
    'delete group ' || old.id)
  from _users.data
  where uac.can_read_group(user_name, old.parent_id)
  limit 1;
  return new;
end $$ language plpgsql;
