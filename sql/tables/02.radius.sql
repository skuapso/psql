create sequence radius.seq_nas;
create table radius.nas(
  id bigint,
  identifier varchar not null,

  constraint zidx_nas_pk primary key(id)
);
create trigger insertb_00_set_id
  before insert on radius.nas
  for each row
  when (new.id is null)
  execute procedure triggers.set_id();

create table radius.nas_ips(
  id bigint,
  address inet,

  constraint zidx_nas_ips_pk primary key(id, address),
  constraint zidx_nas_ips_fk_id foreign key(id) references radius.nas(id) on delete cascade
);

create table radius.nas_apns(
  id bigint,
  apn varchar,

  constraint zidx_nas_apns_pk primary key(id, apn),
  constraint zidx_nas_apns_fk_id foreign key(id) references radius.nas(id) on delete cascade
);
