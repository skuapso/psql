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
    constraint "connections(terminal_id->terminals._data(id))"
    references terminals._data(id) on delete set null
);
create index "connections(id, terminal_id)" on data.connections(id desc, terminal_id nulls last);
comment on table data.connections is 'table for connections';

create function connection.set_terminal(
  _id timestamptz,
  _terminal_id bigint
) returns timestamptz as $$
  update data.connections set terminal_id=$2 where id=$1 returning id;
$$ language sql;
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
    constraint "broken(data_id->binary(data_id))"
    references data.binary(data_id) on delete cascade,
  connection_id timestamptz
    not null
    constraint "broken(connection_id->connection(id))"
    references data.connections(id) on delete cascade,
  repeat bool not null default false
);

create function data.broken(
  _id timestamptz,
  _data bytea,
  _connection_id timestamptz)
returns timestamptz as $$
  insert into data.broken select $1, data.binary_id($1, $2), $3, false returning id
$$ language sql;
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
    constraint "raws(data_id->binary(data_id))"
    references data.binary(data_id) on delete cascade,
  connection_id timestamptz
    not null
    constraint "raws(connection_id->connection(id))"
    references data.connections(id) on delete cascade,
  answer_id timestamptz
    constraint "raws(answer_id)->binary(id))"
    references data.binary(data_id) on delete set null,
  answered varchar
);

create function data.raw(
  _id timestamptz,
  _data bytea,
  _connection_id timestamptz)
returns timestamptz as $$
  with data as (select data.binary_id($1, $2) as id),
  br as (insert into data.broken select $1, id, $3, true from data where id<>$1)
  insert into data.raws select $1, id, $3 from data where id=$1 returning id;
$$ language sql;
--------------------------------------------------------------------------------
-- /data.raws
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- data.answers
--------------------------------------------------------------------------------
create function data.answer(
  _id timestamptz,
  _data bytea,
  _answered varchar
) returns timestamptz as $$
  with data as (select data.binary_id(now(), $2) as id)
  update data.raws r set (answer_id, answered)=(d.id, $3) from data d where r.id=$1 returning r.id
$$ language sql;
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
    constraint "packets(data_id->binary(data_id))"
    references data.binary(data_id) on delete cascade,
  raw_id timestamptz
    not null
    constraint "packets(raw_id->raw(id))"
    references data.raws(id) on delete cascade,
  type data.types not null,
--  eventtime timestamptz not null,
  data jsonb not null
);
create index "packets(raw_id)" on data.packets
--using gin
(raw_id);

create function data.packet(
  _id timestamptz,
  _data bytea,
  _raw_id timestamptz,
  _type data.types,
--  _eventtime timestamptz,
  _desc jsonb
) returns timestamptz as $$
  with raw as (select id from data.raws where id=$3),
    craw as (select count(*) from raw),
    data as (select data.binary_id($1, $2) as id from craw where craw.count>0)
    insert into data.packets
    select $1, data.id, raw.id, $4, $5 from data
    inner join raw on(true)
    inner join craw on(true)
    where craw.count>0
    returning id
$$ language sql;

--------------------------------------------------------------------------------
-- /data.packets
--------------------------------------------------------------------------------
