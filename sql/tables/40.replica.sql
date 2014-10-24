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

  max_connections bigint,

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
create table replica.data(
  id bigint
    default nextval('replica.seq_data')
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
);

create index "data(id,server && answer is null"
  on replica.data(id, server_id)
  where answer_id is null;

create index "data(id,server,terminal)"
  on replica.data(id, server_id, terminal_id)
  where answer_id is null;

create index "data(answer)"
  on replica.data(answer_id);

create index "data(parent)"
  on replica.data(parent_id);

create sequence replica.seq_answers;
create table replica.answers(
  id bigint
    default nextval('replica.seq_answers')
    constraint "answers(id)" primary key,

  connection_id bigint
    not null
    constraint "answers->connection"
    references data.connections(id)
    on delete cascade,

  data_id bigint
    not null
    constraint "data->data.binary"
    references data."binary"(data_id)
    on delete cascade
);

create index "answers(connection_id)"
  on replica.answers(connection_id);

alter table replica.data
  add constraint "data->answers" foreign key(answer_id)
  references replica.answers(id)
  on delete cascade;

create trigger "=>insert 00 correct id"
  before insert
  on replica.answers
  for each row
  when (checking.is_value_presents('replica', 'answers', 'id', new.id))
  execute procedure triggers.correct_timestamp_id();

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
