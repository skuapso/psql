--------------------------------------------------------------------------------
-- data.binary
--------------------------------------------------------------------------------
create sequence data.seq_binary;
create table data.binary(
  data_id bigint
    constraint "binary(data_id)" primary key
    default nextval('data.seq_binary'),
  data bytea not null
);
create unique index "binary(md5(data))" on data.binary(md5(data));
--------------------------------------------------------------------------------
-- /data.binary
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- data.connections
--------------------------------------------------------------------------------
create sequence data.seq_connections;
create table data.connections(
  id bigint
    constraint "connections(id)" primary key
    default nextval('data.seq_connections'),
  protocol terminals.protocols not null,
  closed timestamptz default null,
  type connections.types not null,
  terminal_id bigint
    constraint "connections->terminals._data(id)"
    references terminals._data(id) on delete set null
);
create index "connections(id, terminal_id)" on data.connections(id desc, terminal_id nulls last);
comment on table data.connections is 'table for connections';
--------------------------------------------------------------------------------
-- /data.connections
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- data.broken
--------------------------------------------------------------------------------
create table data.broken(
  id bigint
    constraint "broken(data_id)" primary key,
  data_id bigint
    not null
    constraint "broken->binary"
    references data.binary(data_id) on delete cascade,
  connection_id bigint
    not null
    constraint "broken->connection(id)"
    references data.connections(id) on delete cascade,
  repeat bool not null default false
);
--------------------------------------------------------------------------------
-- /data.broken
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- data.raws
--------------------------------------------------------------------------------
create table data.raws(
  id bigint
    constraint "raws(id)" primary key,
  data_id bigint
    not null
    constraint "raws->binary"
    references data.binary(data_id) on delete cascade,
  connection_id bigint
    not null
    constraint "raws->connection(id)"
    references data.connections(id) on delete cascade,
  answer_id bigint
    constraint "raws(answer)->binary(data_id)"
    references data.binary(data_id) on delete set null,
  answered varchar
);
--------------------------------------------------------------------------------
-- /data.raws
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- data.packets
--------------------------------------------------------------------------------
create sequence data.seq_packets;
create table data.packets(
  id bigint
    constraint "packets(id)" primary key
    default nextval('data.seq_packets'),
  data_id bigint
    not null
    constraint "packets->binary"
    references data.binary(data_id) on delete cascade,
  raw_id bigint
    not null
    constraint "packets->raw(id)"
    references data.raws(id) on delete cascade,
  type data.types not null,
  eventtime timestamptz,
  data jsonb not null
);
create index "packets(raw_id)" on data.packets
--using gin
(raw_id);

create trigger "=>insert set event"
  after insert
  on data.packets
  for each row
  when (packet.object(new.raw_id, true) is not null)
  execute procedure event.upload();
--------------------------------------------------------------------------------
-- /data.packets
--------------------------------------------------------------------------------
