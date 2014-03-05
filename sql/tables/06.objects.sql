create sequence objects.seq_specialization;
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

create sequence objects.seq_groups;
create table objects.groups(
  id bigint,
  title varchar not null,
  description varchar,
  owner_id bigint not null,
  parent_id bigint,

  constraint zidx_groups_pk primary key(id),
  constraint zidx_groups_uk_title unique(title),
  constraint zidx_groups_fk_owner foreign key(owner_id) references owners.data(id),
  constraint zidx_groups_fk_parent foreign key(parent_id) references objects.groups(id),
  constraint zidx_groups_ck_parent check(not(array[parent_id] <@ "group".childs(id))),
  constraint zidx_groups_ck_owner check("group".owner(id) = "group".owner(parent_id))
);

create trigger insertb_00_set_id
  before insert
  on objects.groups
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();

create trigger updateb_00_check_parenting
  before update
  on objects.groups
  for each row
  when (array[new.parent_id] <@ "group".childs(new.id))
    execute procedure triggers.reject();

create sequence objects.seq_data;
create table objects.data(
  id bigint,
  no varchar not null,
  specialization_id bigint,
  model_id bigint not null,
  terminal_id bigint,
  group_id bigint not null,
  owner_id bigint not null,

  constraint zidx_data_pk primary key(id),
  constraint zidx_data_uk_no unique(no),
  constraint zidx_data_uk_terminal unique(terminal_id),
  constraint zidx_data_fk_specialization foreign key(specialization_id) references objects.specializations(id),
  constraint zidx_data_fk_model foreign key(model_id) references objects.models(id),
  constraint zidx_data_fk_terminal foreign key(terminal_id) references terminals.data(id),
  constraint zidx_data_fk_group foreign key(group_id) references objects.groups(id),
  constraint zidx_data_fk_owner foreign key(owner_id) references owners.data(id)
);

create trigger insertb_00_set_id
  before insert
  on objects.data
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();

create view groups.tree as select id, parent_id as parent_id, title, owner_id
  from objects.groups
  order by parent_id nulls first;

create view objects.get as select *,object.title(id) as title from objects.data;
