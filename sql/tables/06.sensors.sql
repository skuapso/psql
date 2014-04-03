create sequence sensors.seq_types;
create table sensors.types(
  id bigint
    constraint zidx_types_pk primary key
  ,port_type terminal.port_types
    not null
  ,data_type varchar
    not null
);
create trigger insertb_50_set_id
  before insert
  on sensors.types
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();

create sequence sensors.seq_models;
create table sensors.models(
  id bigint
    constraint zidx_models_pk primary key
  ,type_id bigint
    not null
    constraint zidx_models_fk_type references sensors.types(id)
  ,virtual boolean
    not null
  ,title varchar
    not null
    default ''
);

create trigger insertb_50_set_id
  before insert
  on sensors.models
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();

create sequence sensors.seq_data;
create table sensors.data(
  id bigint
    constraint zidx_data_pk primary key
  ,model_id bigint
    constraint zidx_data_fk_model references sensors.models(id)
  ,serial varchar
);
create trigger insertb_50_set_id
  before insert
  on sensors.data
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();

create sequence objects.seq_sensors;
create table objects.sensors(
  id bigint
    constraint zidx_sensors_pk primary key
  ,object_id bigint
    not null
    constraint zidx_sensors_fk_object references objects._data(id)
  ,sensor_id bigint
    not null
    constraint zidx_sensors_fk_sensor references sensors.data(id)
  ,port_id varchar

  ,constraint zidx_sensors_uk_object_sensor_port unique(object_id, sensor_id, port_id)
);
create unique index zidx_sensors_uk_sensor
on objects.sensors(sensor_id)
where not sensor.virtual(sensor_id);

create trigger insertb_50_set_id
  before insert
  on objects.sensors
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();
