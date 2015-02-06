create sequence uac.seq_groups;
create table uac.groups(
  id bigint
    default nextval('uac.seq_groups')
    constraint zidx_groups_pk primary key,
  group_id bigint
    constraint zidx_groups_fk_group references objects._groups(id),
  user_name name
    not null,

  constraint zidx_groups_cu_group_user unique(group_id, user_name)
);

create trigger insertb_00_check_user_exists before insert on uac.groups
for each row
when (not _user.exists(new.user_name, true))
execute procedure triggers.reject();

create trigger insertb_10_check_group_parents before insert on uac.groups
for each row
execute procedure uac.check_group_parents();

create rule delete_childs as on insert to uac.groups
do also
  delete from uac.groups
  where user_name=new.user_name
  and array[group_id]<@"group".childs(new.group_id);

create trigger insertb_00_access_denied before insert on objects._groups
for each row
when (not uac.can_read_group(new.parent_id))
execute procedure triggers.reject();

create view objects.groups
with (security_barrier)
as select
    id,
    title,
    case
      when uac.can_read_group(parent_id) then parent_id
      else null
    end as parent_id,
    'group'::varchar as type
  from objects._groups
  where uac.can_read_group(id);

create view objects.viewable
as select O.*
  from objects.groups G
  join objects._data O on (G.id = O.group_id);

create view terminals.data as
  select T.*,'terminal'::varchar as type from objects.viewable O
  inner join terminals._data T on (O.terminal_id=T.id)
  where not T.deleted;

create view objects.sensors as
  select S.*,'object_sensor'::varchar as type from objects._sensors S
  inner join objects.viewable O on (S.object_id = O.id)
  union all
  select *,'object_sensor'::varchar as type
  from (
    select
      OT.type_id,
      O.id,
      OT.type_id,
      ports,
      provides
    from objects.viewable O
    inner join terminals.data T on (O.terminal_id=T.id)
    inner join terminals.ports P on (T.model_id=P.model_id)
    inner join objects.tools OT on (P.provides=OT.id)
    where
    provides is not null
  ) S1;

create view objects.data as
  select
    O.*,
    array_to_json(
      array(
        select row_to_json(s.*)
        from objects.sensors s where object_id=o.id
      )
    )::jsonb as sensors,
    jsonb.extend(E.data, json_build_object('eventtime', time)) as data,
    'object'::varchar as type
  from objects.viewable O
  left join events._data E on (O.last_event_id = E.id)
  where not O.deleted
  group by O.id,O.no,O.model_id,
          O.specialization_id,O.group_id,
          O.terminal_id,O.deleted,O.last_event_id,
          E.data,time;

create view objects.get as select *,object.title(id) as title from objects.data;

create view events.data as
  select *
  from events._data
  where object_id in (select id from objects.viewable);

create rule "create" as on insert to objects.groups
do instead
  insert into objects._groups (title, parent_id)
  values (new.title, new.parent_id)
  returning *,'group'::varchar;

create rule "update" as on update to objects.groups
do instead
  update objects._groups set title=new.title,parent_id=new.parent_id
  where id=new.id
  and uac.can_read_group(id)
  and uac.can_read_group(new.parent_id)
  and uac.can_read_group(old.parent_id)
  returning *,'group'::varchar;

create rule "delete" as on delete to objects.groups
do instead
  delete from objects._groups
  where id=old.id
  and uac.can_read_group(id)
  returning *,'group'::varchar;

create rule "create" as on insert to objects.data
do instead
  insert into objects._data (no, model_id, specialization_id, group_id, terminal_id)
  select new.no, new.model_id, new.specialization_id, new.group_id, new.terminal_id
  from objects.groups
  where id=new.group_id
  returning *, null::jsonb, null::jsonb, 'object'::varchar;

create rule "update" as on update to objects.data
do instead
  update objects._data
  set
    no = new.no,
    model_id = new.model_id,
    specialization_id = new.specialization_id,
    group_id = new.group_id,
    terminal_id = new.terminal_id
  where
    id = new.id
    and id in (select id from objects.data where id=new.id)
    and uac.can_read_group(new.group_id)
    and uac.can_read_group(old.group_id)
  returning *, null::jsonb, null::jsonb, 'object'::varchar;

create rule "delete" as on delete to objects.data
do instead
  update objects._data set deleted=true
  where id = old.id
  and id in (select id from objects.data where id=old.id)
  returning *, null::jsonb, null::jsonb, 'object'::varchar;

create rule r_insert as on insert to terminals.data
do instead insert into terminals._data
  select new.id,
          new.uin,
          new.serial_no,
          new.period,
          new.model_id
  returning *,'terminal'::varchar;
alter table terminals.data alter column period set default '0:3:0';

create rule "create" as on insert to objects.sensors
do instead
  insert into objects._sensors (object_id, sensor_id, port_id, provides)
  select new.object_id, new.sensor_id, new.port_id, new.provides
  from objects.data
  where id=new.object_id
  returning *,'object_sensor'::varchar;

create rule "update" as on update to objects.sensors
do instead
  update objects._sensors
  set
    object_id=new.object_id,
    sensor_id=new.sensor_id,
    port_id=new.port_id
  where id in (select id from objects.sensors where id=new.id and old.id=new.id)
  returning *,'object_sensor'::varchar;

create rule "delete" as on delete to objects.sensors
do instead
  delete from objects._sensors
  where id in (select id from objects.sensors where id=old.id)
  returning *,'object_sensor'::varchar;
