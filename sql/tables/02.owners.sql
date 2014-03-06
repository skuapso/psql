create sequence owners.seq_data;

create table owners.data(
  id bigint,
  parent_id bigint,
  title varchar not null,
  description varchar not null default '',

  constraint zidx_data_pk primary key(id),
  constraint zidx_data_uk_title unique(title),
  constraint zids_data_fk_parent foreign key(parent_id) references owners.data(id)
);

create trigger insertb_00_set_id
  before insert
  on owners.data
  for each row
  execute procedure triggers.set_id();
