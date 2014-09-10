--------------------------------------------------------------------------------
-- data.binary
--------------------------------------------------------------------------------
create table data.binary(
  data_id timestamptz
    constraint "binary(data_id)" primary key
    default current_timestamp,
  data bytea not null
);
create unique index "binary(md5(data))" on data.binary(md5(data));

create or replace function data.binary_id(
  _id timestamptz,
  _data bytea
) returns timestamptz as $$
  with
  defined as (select data_id from data.binary where md5(data)=md5($2)),
  count as (select count(*) from defined),
  inserted as (insert into data.binary
                select $1, $2
                from count
                where count.count=0
                returning data_id)
  select
    case when count.count=0 then inserted.data_id
    else defined.data_id
    end as id
  from defined
  full join inserted on(true)
  full join count on(true)
$$ language sql;
--------------------------------------------------------------------------------
-- /data.binary
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- data.connections
--------------------------------------------------------------------------------
create table data.connections(
  id timestamptz
    constraint "connections(id)" primary key
    default current_timestamp,
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
  id timestamptz
    constraint "broken(data_id)" primary key,
  data_id timestamptz
    not null
    constraint "broken->binary"
    references data.binary(data_id) on delete cascade,
  connection_id timestamptz
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
  id timestamptz
    constraint "raws(id)" primary key,
  data_id timestamptz
    not null
    constraint "raws->binary"
    references data.binary(data_id) on delete cascade,
  connection_id timestamptz
    not null
    constraint "raws->connection(id)"
    references data.connections(id) on delete cascade,
  answer_id timestamptz
    constraint "raws(answer)->binary(data_id)"
    references data.binary(data_id) on delete set null,
  answered varchar
);
--------------------------------------------------------------------------------
-- /data.raws
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- data.answers
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- /data.answers
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- data.packets
--------------------------------------------------------------------------------
create table data.packets(
  id timestamptz
    constraint "packets(id)" primary key
    default current_timestamp,
  data_id timestamptz
    not null
    constraint "packets->binary"
    references data.binary(data_id) on delete cascade,
  raw_id timestamptz
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
  when (terminal.object(packet.terminal(new.raw_id, true)) is not null)
  execute procedure event.prepare();

--------------------------------------------------------------------------------
-- /data.packets
--------------------------------------------------------------------------------
