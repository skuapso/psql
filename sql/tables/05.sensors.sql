create sequence sensors.seq_ids;
create table sensors.types(
  id bigint
    constraint zidx_types_pk primary key
    default -nextval('sensors.seq_ids')
  ,title varchar
    not null
    constraint zidx_types_uk_title unique
);

create table sensors.type_ports(
  id bigint
    constraint zidx_type_ports_fk references sensors.types(id)
  ,port_type terminal.port_types
  ,compute_fun varchar

  ,constraint zidx_type_ports_pk primary key(id, port_type)
);
create unique index zidx_type_ports_uk_port_without_fun
on sensors.type_ports(port_type)
where compute_fun is null;
create unique index zidx_type_ports_uk_port_fun
on sensors.type_ports(port_type, compute_fun)
where compute_fun is not null;

create table sensors.models(
  id bigint
    constraint zidx_models_pk primary key
    default -nextval('sensors.seq_ids')
  ,type_id bigint
    not null
    constraint zidx_models_fk_type references sensors.types(id)
  ,title varchar
    not null
    constraint zidx_models_uk_title unique
);

create table sensors._data(
  id bigint
    constraint zidx_data_pk primary key
    default nextval('sensors.seq_ids')
  ,model_id bigint
    constraint zidx_data_fk_model references sensors.models(id)
  ,serial varchar not null

  ,constraint zidx_data_uk_serial_model unique(serial, model_id)
);

create view sensors.data as
select * from sensors._data
union all select id,id,title from sensors.models
union all select id,id,title from sensors.types;

create rule on_insert as on insert to sensors.data
do instead
insert into sensors._data
values (new.id, new.model_id, new.serial)
returning *;

create rule on_update as on update to sensors.data
do instead
update sensors._data
set id=new.id, model_id=new.model_id, serial=new.serial
where id=old.id
returning *;

create rule on_delete as on delete to sensors.data do instead
delete from sensors._data where id=old.id;
