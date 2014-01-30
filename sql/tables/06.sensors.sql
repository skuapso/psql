create table sensors.types(
  id bigint
    not null
    constraint zidx_types_pk primary key
  ,purpose sensor.purposes
    not null
  ,virtual boolean
    not null
  ,title varchar
    not null
    constraint zidx_types_uk_title unique
);
create unique index zidx_types_uk_purpose
on sensors.types(purpose)
where virtual;

create sequence sensors.seq_models;
create table sensors.models(
  id bigint
    constraint zidx_models_pk primary key
  ,type_id bigint
    not null
    constraint zidx_models_fk_type references sensors.types(id)
  ,title varchar
    not null
);

create table sensors.data(
  id bigint
    constraint zidx_data_pk primary key
  ,model_id bigint
    constraint zidx_data_fk_model references sensors.models(id)
  ,serial varchar
);

create table objects.sensors(
  object_id bigint
    not null
    constraint zidx_sensors_fk_object references objects.data(id)
  ,sensor_id bigint
    not null
    constraint zidx_sensors_fk_sensor references sensors.data(id)

  ,constraint zidx_sensors_pk primary key(object_id, sensor_id)
);
create unique index zidx_sensors_uk_sensor
on objects.sensors(sensor_id)
where not sensor.virtual(sensor_id);
