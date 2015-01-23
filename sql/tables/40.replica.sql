create sequence replica.seq_servers;
create sequence replica.seq_rules;
create sequence replica.seq_issues;
create type replica.rules_types as enum('include', 'exclude');

create table replica.servers(
  id bigint
    default nextval('replica.seq_servers')
    constraint "servers(id)"
    primary key,

  hostname varchar
    not null,

  port bigint
    not null,

  protocols terminals.protocols[]
    not null,

  max_points bigint
    not null
    default 1,

  retry_interval interval
    not null
    default '00:00:01',

  max_connections bigint
    not null
    default 10,

  connection_type replica.connection_types
    not null
    default 'soft'
);

create table replica."owners<->servers"(
  owner_id bigint
    constraint "owners<->servers->owners.data"
    references owners.data(id)
    on delete cascade,

  server_id bigint
    constraint "owners<->servers->servers"
    references replica.servers(id)
    on delete cascade,

  constraint "owners<->servers(owner,server)"
  primary key(owner_id, server_id)
);

create table replica.rules(
  id bigint
    default nextval('replica.seq_rules')
    constraint "rules(id)"
    primary key,

  owner_id bigint
    not null
    constraint "rules->owners.data(id)"
    references owners.data(id)
    on delete cascade,

  object_id bigint
    constraint "rules->objects.data"
    references objects._data(id)
    on delete cascade,

  group_id bigint
    constraint "rules->objects.groups"
    references objects._groups(id)
    on delete cascade,

  specialization_id bigint
    constraint "rules->objects.specializations"
    references objects.specializations(id)
    on delete cascade,

  local_port bigint,

  terminal_protocol terminals.protocols,

  type replica.rules_types
    not null
    default 'include',

  constraint "rules(type=include) value in one column" check (type='exclude' or (((
      (object_id is not null)::integer +
      (group_id is not null)::integer +
      (specialization_id is not null)::integer +
      (local_port is not null)::integer +
      (terminal_protocol is not null)::integer)
    = 1) and type='include')),

  constraint "rules(type=exclude) value in one column" check (type='include' or (((
      (object_id is not null)::integer +
      (group_id is not null)::integer +
      (specialization_id is not null)::integer +
      (local_port is not null)::integer +
      (terminal_protocol is not null)::integer)
    = 1) and type='exclude'))
);

create sequence replica.seq_data;
create sequence replica.seq_answers;
create table replica.data(
  id bigint
    constraint "data(id)"
    primary key,

  parent_id bigint
    not null
    constraint "data(parent)->data.packets"
    references data.packets(id)
    on delete cascade,

  server_id bigint
    not null
    constraint "data->servers"
    references replica.servers(id)
    on delete cascade,

  protocol terminals.protocols
    not null,

  terminal_id bigint
    not null
    constraint "data->terminals.data"
    references terminals._data(id)
    on delete cascade,

  data_id bigint
    not null
    constraint "data->data.binary"
    references data."binary"(data_id)
    on delete cascade,

  answer_id bigint
    not null,

  answer_data_id bigint
    not null
    constraint "data->answers.id"
    references data."binary"(data_id)
    on delete cascade,

  connection_id bigint
    not null
    constraint "data->data.connections"
    references data."connections"(id)
    on delete cascade
);

create index "data(parent)"
  on replica.data(parent_id);

create table replica.undelivered(
  id bigint
    default nextval('replica.seq_data')
    constraint "undelivered(id)"
    primary key,

  parent_id bigint
    not null
    constraint "undelivered(parent)->data.packets"
    references data.packets(id)
    on delete cascade,

  server_id bigint
    not null
    constraint "undelivered->servers"
    references replica.servers(id)
    on delete cascade,

  protocol terminals.protocols
    not null,

  terminal_id bigint
    not null
    constraint "undelivered->terminals.data"
    references terminals._data(id)
    on delete cascade,

  data_id bigint
    not null
    constraint "undelivered->data.binary"
    references data."binary"(data_id)
    on delete cascade
);

create index "undelivered(id,server)"
  on replica.undelivered(id, server_id);

create index "undelivered(id,server,terminal)"
  on replica.undelivered(id, server_id, terminal_id);

create index "undelivered(parent)"
  on replica.undelivered(parent_id);

create table replica.issues(
  id bigint
    default nextval('replica.seq_issues')
    constraint "issues(id)" primary key,

  dbtime timestamptz
    not null
    default current_timestamp,

  server_id bigint
    not null
    constraint "issues->servers" references replica.servers(id)
    on delete cascade,

  connection_id bigint
    not null
    constraint "issues->connection" references data.connections(id)
    on delete cascade,

  packets_ids bigint[]
    not null,

  issue replica.issue
    not null
);

-- returns type replica.servers (defined by table)
create function replica.server_info(_server_id bigint)
returns setof replica.servers
as $$
begin
  return query
  select
    *
  from
    replica.servers S
  where
    S.id = $1;
end $$ language plpgsql stable;
