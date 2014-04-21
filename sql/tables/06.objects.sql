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
  id bigint,
  title varchar not null,

  constraint zidx_models_pk primary key(id),
  constraint zidx_models_uk_title unique(title)
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

create trigger updatea_zz_notify
  after update
  on objects._groups
  for each row
  when (new <> old)
  execute procedure "group".notify_update();

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

create trigger insertb_00_set_id
  before insert
  on objects._data
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();
