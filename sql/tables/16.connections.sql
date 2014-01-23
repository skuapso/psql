create table connections.ip_data(
  id timestamptz not null,
  local_ip inet not null,
  local_port bigint not null,
  remote_ip inet not null,
  remote_port bigint not null,

  constraint zidx_ip_pk primary key(id),
  constraint zidx_ip_fk_id foreign key(id) references data.connections(id) on delete cascade
);

create view connections.ip as
  select D.*,I.local_ip,I.local_port,I.remote_ip,I.remote_port
  from connections.ip_data I
  inner join data.connections D using (id);
