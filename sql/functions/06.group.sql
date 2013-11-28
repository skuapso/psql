create function "group".can_view(_group_id bigint) returns bool as $$
begin
  return true;
end $$ language plpgsql stable;

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
  select parent_id into p from objects.groups where id=$1;
  return p;
end $$ language plpgsql stable;

create or replace function "group".owner(_group_id bigint) returns bigint as $$
declare
  o bigint;
begin
  select owner_id into o from objects.groups where id=$1;
  return o;
end $$ language plpgsql stable;

create or replace function "group".json(_group_id bigint) returns json as $$
declare
  r record;
begin
  select 'group' as type, id, title, parent_id, owner_id into r from objects.groups where id=$1;
  return row_to_json(r);
end $$ language plpgsql stable;
