create sequence uac.seq_groups;
create table uac.groups(
  id bigint
    default nextval('uac.seq_groups')
    constraint zidx_groups_pk primary key,
  group_id bigint
    not null
    constraint zidx_groups_fk_group references objects._groups(id),
  user_name name
    not null
);

create trigger insertb_00_check_parent before insert on uac.groups
for each row
when (not _user.exists(new.user_name))
execute procedure triggers.reject();

create trigger insertb_00_check_group_parents before insert on uac.groups
for each row
execute procedure uac.check_group_parents();

create rule delete_childs as on insert to uac.groups
do also
  delete from uac.groups
  where user_name=new.user_name
  and array[group_id]<@"group".childs(new.group_id);

raise warning 'should use "with (security_barrier)"';
create view objects.groups as
  select
    id,
    title,
    case
      when uac.can_read_group(parent_id) then parent_id
      else null
    end as parent_id from objects._groups
  where uac.can_read_group(id);

create view objects.data as
  select O.* from objects.groups G
  inner join objects._data O on(G.id=O.group_id);

create view terminals.data as
  select T.* from objects.data O
  inner join terminals._data T on (O.terminal_id=T.id);

create view objects.get as select *,object.title(id) as title from objects.data;
