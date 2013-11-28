create sequence options.seq_data;
create table options.data(
  id bigint,
  name varchar not null,
  value varchar not null,

  constraint zidx_data_pk primary key(id)
);

create trigger insertb_00_set_id
  before insert
  on options.data
  for each row when (new.id is null)
  execute procedure triggers.set_id();
