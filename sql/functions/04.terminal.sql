create type terminals.protocols as enum(
  'ui'
);
create type terminals.command_send_type as enum(
  'immediately',
  'answer'
);
create type terminal.port_types as enum(
  'digital'
  ,'analog'
  ,'location'
  ,'speed'
);

create function terminal.add(
  _terminal_uin bigint,
  _terminal_protocol terminals.protocols
) returns bigint as $$
declare
  m bigint;
  q varchar;
  i bigint;
begin
  select id into m from terminals.models where $2=any(protocols) order by id desc limit 1;
  insert into terminals._data(id, uin, serial_no, model_id) values
    (null, $1, $1::varchar, m) returning id into i;
  return i;
end $$ language plpgsql volatile;

create function terminal.get(
  _terminal_uin bigint,
  _terminal_protocol terminals.protocols) returns bigint as $$
declare
  i boolean;
begin
  i = "option".get('terminal_auto_add')::boolean;
  return terminal.get($1, $2, i);
end $$ language plpgsql stable;

create function terminal.get(
  _terminal_uin bigint,
  _terminal_protocol terminals.protocols,
  _auto_add boolean) returns bigint as $$
declare
  i bigint;
begin
  select id into i from terminals._data where uin=$1 and $2=any(terminal.protocols(id));
  if i is null and _auto_add then
    i = terminal.add($1, $2);
  end if;
  return i;
end $$ language plpgsql stable;

create function terminal.model_protocols(_model_id bigint) returns terminals.protocols[] as $$
declare
  p terminals.protocols[];
begin
  select protocols into p from terminals.models where id=$1;
  return p;
end $$ language plpgsql stable;

create function terminal.model(_terminal_id bigint) returns bigint as $$
declare
  m bigint;
begin
  select model_id into m from terminals._data where id=$1;
  return m;
end $$ language plpgsql stable;

create function terminal.protocols(_terminal_id bigint) returns terminals.protocols[] as $$
begin
  return terminal.model_protocols(terminal.model($1));
end $$ language plpgsql stable;

create function terminal.uin(_terminal_id bigint) returns bigint as $$
declare
  u bigint;
begin
  select uin into u from terminals._data where id=$1;
  return u;
end $$ language plpgsql immutable;

create function terminal.last_connection(_terminal_id bigint) returns timestamptz as $$
declare
  i timestamptz;
begin
  select id into i from data.connections where terminal_id=$1 order by id desc limit 1;
  return i;
end $$ language plpgsql stable;

create function terminal.command(_terminal_id bigint) returns table(
  command_id bigint,
  command bytea,
  send_type terminals.command_send_type)
as $$
begin
  return query select C.id,C.command,C.type from terminals.commands C
  where terminal_id=$1 and executed is null order by dbtime limit 1;
end $$ language plpgsql stable;

create function terminal.command_executed(
  _terminal_id bigint,
  _command_id bigint)
returns bool
language plpgsql
as $$
begin
  update terminals.commands set executed=now() where id=$2 and terminal_id=$1;
  return found;
end $$;

create function terminal.object(_terminal_id bigint) returns bigint as $$
begin
  return terminal.object($1, current_timestamp);
end $$ language plpgsql stable;

create function terminal.object(_terminal_id bigint, _time timestamptz) returns bigint as $$
declare
  i bigint;
begin
  select id
  into i
  from objects.data
  where terminal_id=$1
  limit 1;
  return i;
end $$ language plpgsql immutable;

create function terminal.set_info(
  _terminal_id bigint,
  _info jsonb)
returns setof jsonb
as $$
begin
  return query
  update terminals._data
  set info=jsonb.extend(info, $2)
  where id=$1
  returning info;
end $$ language plpgsql;

create function terminal.ports(
  _terminal_id bigint,
  _common bool default null
) returns jsonb[]
as $$
declare
  p jsonb[];
begin
  select
    array_agg(ports)
  into
    p
  from
    terminals.data T
  inner join
    terminals.models M
    on (T.model_id = M.id)
  inner join
    terminals.ports P
    on (M.id = P.model_id)
  where
    T.id = $1
    and (
      $2 is null
      or (provides is not null)=$2
    );
  return p;
end $$ language plpgsql stable;
