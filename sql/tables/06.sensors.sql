create type sensors.types as enum ('digital', 'analog', 'counter', 'boolean');

create sequence sensors.seq_models;
create table sensors.models(
  id bigint constraint zidx_models_pk primary key
  ,type sensors.types not null
  ,title varchar not null
);

create table sensors.data(
  id bigint constraint zidx_data_pk primary key
  ,model_id bigint
    constraint zidx_data_fk_model
    foreign key sensors.models
  ,serial varchar
);
