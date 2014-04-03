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
  execute procedure triggers.notify_update('ui', 'group');

create sequence objects.seq__data;
create table objects._data(
  id bigint,
  no varchar not null,
  specialization_id bigint,
  model_id bigint not null,
  terminal_id bigint,
  group_id bigint not null,

  constraint zidx_data_pk primary key(id),
  constraint zidx_data_uk_no unique(no),
  constraint zidx_data_uk_terminal unique(terminal_id),
  constraint zidx_data_fk_specialization foreign key(specialization_id) references objects.specializations(id),
  constraint zidx_data_fk_model foreign key(model_id) references objects.models(id),
  constraint zidx_data_fk_terminal foreign key(terminal_id) references terminals._data(id),
  constraint zidx_data_fk_group foreign key(group_id) references objects._groups(id)
);

create trigger updatea_zz_notify
  after update
  on objects._data
  for each row
  when (new <> old)
  execute procedure triggers.notify_update('ui', 'object');

create trigger insertb_00_set_id
  before insert
  on objects._data
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();
