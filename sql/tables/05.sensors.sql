create sequence sensors.seq_ids;
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
  execute procedure triggers.set_id('sensors.seq_ids');

create table sensors._models(
  id bigint
    constraint zidx_models_pk primary key
  ,type_id bigint
    not null
    constraint zidx_models_fk_type references sensors.types(id)
  ,title varchar
    not null
    constraint zidx_models_uk_title unique
);

create view sensors.models as
select *,false as virtual from sensors._models
union all select id,id,port_type || '-' || data_type,true as virtual from sensors.types;

create rule on_insert as on insert to sensors.models
do instead
insert into sensors._models
values (new.id, new.type_id, new.title)
returning *,false;

create rule on_udpate as on update to sensors.models
do instead
update sensors._models
set id=new.id,type_id=new.type_id,title=new.title
where id=old.id returning *,false;

create rule on_delete as on delete to sensors.models
do instead
delete from sensors._models
where id=old.id;

create trigger insertb_50_set_id
  before insert
  on sensors._models
  for each row
  when (new.id is null)
  execute procedure triggers.set_id('sensors.seq_ids');

create sequence sensors.seq_data;
create table sensors._data(
  id bigint
    constraint zidx_data_pk primary key
  ,model_id bigint
    constraint zidx_data_fk_model references sensors._models(id)
  ,serial varchar not null

  ,constraint zidx_data_uk_serial_model unique(serial, model_id)
);
create trigger insertb_50_set_id
  before insert
  on sensors._data
  for each row
  when (new.id is null)
  execute procedure triggers.set_id('sensors.seq_ids');

create view sensors.data as
select * from sensors._data
union all select id,id,'' from sensors.models;

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

create rule on_delte as on delete to sensors.data do instead
delete from sensors._data where id=old.id;
