create table connections.ip_data(
  id timestamptz
    constraint "ip(id)" primary key
    constraint "ip(id->data.connections(id)" references data.connections(id) on delete cascade,
  local_ip inet not null,
  local_port bigint not null,
  remote_ip inet not null,
  remote_port bigint not null
);

create function connection.open(
  _id timestamptz,
  _proto terminals.protocols,
  _local_ip inet,
  _local_port bigint,
  _remote_ip inet,
  _remort_port bigint
) returns timestamptz as $$
  with data_id as (
    insert into data.connections (id, protocol, type)
    values ($1, $2, 'ip')
    returning id
  ) insert into connections.ip_data
  select D.id, $3, $4, $5, $6
  from data_id D returning id
$$ language sql;

create function connection.close(_id timestamptz)
returns timestamptz as $$
  update data.connections set closed=current_timestamp where id=$1 returning id
$$ language sql;

create view connections.ip as
  select *
  from connections.ip_data I
  inner join data.connections D using (id);
