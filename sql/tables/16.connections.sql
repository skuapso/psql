create table connections.ip_data(
  id bigint
    constraint "ip(id)" primary key
    constraint "ip(id->data.connections(id)" references data.connections(id) on delete cascade,
  local_ip inet not null,
  local_port bigint not null,
  remote_ip inet not null,
  remote_port bigint not null
);

create view connections.ip as
  select *
  from connections.ip_data I
  inner join data.connections D using (id);
