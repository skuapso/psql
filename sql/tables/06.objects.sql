create sequence objects.seq_specializations;
create table objects.specializations(
  id bigint,
  title varchar not null,

  constraint zidx_specializations_pk primary key(id),
  constraint zidx_specializations_uk_title unique(title)
);

create trigger insertb_00_set_id
  before insert
  on objects.specializations
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();

create sequence objects.seq_models;
create table objects.models(
  id bigint
    constraint zidx_models_pk primary key,
  title varchar
    not null
    constraint zidx_models_uk_title unique
);

create trigger insertb_00_set_id
  before insert
  on objects.models
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();

create sequence objects.seq__groups;
create table objects._groups(
  id bigint
    constraint zidx_groups_pk primary key,
  title varchar
    not null
    constraint zidx_groups_uk_title unique,
  parent_id bigint
    constraint zidx_groups_fk_parent references objects._groups(id)
      on delete set default
    constraint zidx_groups_ck_parent check(not(array[parent_id] <@ "group".childs(id)))
    constraint zidx_groups_ck_self_parent check(not parent_id=id)
);

create trigger insertb_00_set_id
  before insert
  on objects._groups
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();

create trigger updateb_00_check_parenting
  before update
  on objects._groups
  for each row
  when (array[new.parent_id] <@ "group".childs(new.id))
    execute procedure triggers.reject();

create trigger updatea_zz_notify_update
  after update
  on objects._groups
  for each row
  when (new <> old)
  execute procedure "group".notify_update();

create trigger updatea_zz_notify_create
  after insert
  on objects._groups
  for each row
  execute procedure "group".notify_create();

create trigger updatea_zz_notify_delete
  after delete
  on objects._groups
  for each row
  execute procedure "group".notify_delete();

create sequence objects.seq__data;
create table objects._data(
  id bigint
    constraint zidx_data_pk primary key,
  no varchar
    not null
    constraint zidx_data_uk_no unique,
  model_id bigint
    not null
    constraint zidx_data_fk_model references objects.models(id),
  specialization_id bigint
    constraint zidx_data_fk_specialization references objects.specializations(id),
  group_id bigint
    not null
    constraint zidx_data_fk_group references objects._groups(id),
  terminal_id bigint
    constraint zidx_data_uk_terminal unique
    constraint zidx_data_fk_terminal references terminals._data(id)
      on delete set null,
  deleted boolean
    not null
    default false
);

create sequence objects.seq_tools;
create table objects.tools(
  id bigint
    constraint zidx_purposes_pk primary key
    default nextval('objects.seq_tools')
  ,title varchar
    not null
  ,type_id bigint
    not null
    constraint zidx_purposes_fk_type references sensors.types(id)
);

create table objects._sensors(
  id bigint
    constraint zidx_sensors_pk primary key
    default nextval('sensors.seq_ids')
  ,object_id bigint
    not null
    constraint zidx_sensors_fk_object references objects._data(id)
  ,sensor_id bigint
    not null
  ,port_id jsonb
  ,provides bigint
    constraint zidx_sensors_fk_purpose references objects.tools(id)

  ,constraint zidx_sensors_ck_type
    check(port_id is null or (port_id<@any(terminal.ports(object.terminal(object_id), false))))
  ,constraint zidx_sensors_fk_sensor
    check(checking.is_value_presents('sensors', 'data', 'id', sensor_id))
);
create unique index zidx_sensors_uk_object_sensor_port
on objects._sensors(object_id, port_id)
where (port_id is not null);
create unique index zidx_sensors_uk_sensor_not_virtual
on objects._sensors(id) where (sensor_id>0);

create trigger insertb_50_set_id
  before insert
  on objects._sensors
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();

create trigger insertb_00_set_id
  before insert
  on objects._data
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();

create trigger inserta_00_notify_create
  after insert
  on objects._data
  for each row
  when (not new.deleted)
  execute procedure object.notify_create();

create trigger inserta_00_notify_update
  after update
  on objects._data
  for each row
  when (not new.deleted and not old.deleted)
  execute procedure object.notify_update();

create trigger inserta_00_notify_delete
  after update
  on objects._data
  for each row
  when (not old.deleted and new.deleted)
  execute procedure object.notify_delete();

alter table terminals.ports
add column provides bigint
  constraint zidx_ports_fk_provides references objects.tools(id)
  on delete restrict
  on update restrict;
