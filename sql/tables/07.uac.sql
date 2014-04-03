create sequence uac.seq_uac;
create table uac.groups(
  id bigint
    default nextval('uac.seq_uac')
    constraint zidx_groups_pk primary key,
  group_id bigint
    not null
    constraint zidx_groups_fk_group references objects.groups(id),
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
