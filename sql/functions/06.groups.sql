create function groups.get() returns table (group_id bigint, group_name varchar, group_description varchar) as $$
begin
  return query select id,name,description from objects.groups where "group".can_view(id) order by name;
end $$ language plpgsql stable;

create or replace function groups.tree() returns table (
  group_id bigint,
  lvl bigint,
  weight bigint,
  name varchar,
  objects_count bigint) as $$
declare
  l bigint;
  w bigint;
  current_parent bigint;
  i bigint;
begin
  create temp table tmptree (id bigint, lvl bigint, weight bigint) on commit drop;
  l := 0;
  w := 0;
  current_parent := 0;
  loop
    if current_parent is null then
      select G.id into i
      from objects.groups G
      where G.parent_id is null
      and G.id not in (select T.id from tmptree T)
      order by G.title limit 1;
    else
      select G.id into i
      from objects.groups G
      where G.parent_id=current_parent
      and G.id not in (select T.id from tmptree T)
      order by G.title limit 1;
    end if;
    if not i is null then
      insert into tmptree values (i, l, w);
      l := l + 1;
      w := w + 1;
    else
      l := l - 1;
      loop
        if l < 0 or not i is null then
          exit;
        end if;
        l := l - 1;
        select id into i from tmptree where tmptree.lvl=l order by tmptree.weight desc limit 1;
      end loop;
      l := l + 1;
    end if;
    if i is null then
      exit;
    else
      current_parent := i;
    end if;
  end loop;
  return query
    select *, "group".title (tmptree.id) as title,
    groups.count_objects (array_append ("group".childs(tmptree.id), tmptree.id))  as obj_count
    from tmptree
    where "group".can_view (tmptree.id)
    order by tmptree.weight;
end $$ language plpgsql;

create function groups.count_objects(_groups_id bigint[]) returns bigint as $$
declare
  c bigint;
begin
  select sum(count) into c
  from (
    select "group".count_objects(id) as count
    from objects.groups
    where id in (select unnest($1))
  ) s;
  return c;
end $$ language plpgsql stable;
