create function connection.protocol(_id timestamptz) returns terminals.protocols as $$
declare
  p terminals.protocols;
begin
  select protocol into p from data.connections where id=$1;
  return p;
end $$ language plpgsql stable;

create function connection.local_port(_id timestamptz) returns bigint as $$
declare
  p bigint;
begin
  select local_port into p from connections.ip_data where id=$1;
  return p;
end $$ language plpgsql stable;

create function connection.terminal(_id timestamptz) returns bigint as $$
declare
  t bigint;
begin
  select terminal_id into t from data.connections where id=$1;
  return t;
end $$ language plpgsql stable;

create function connection.set_terminal(
  _id timestamptz,
  _terminal_id bigint)
returns setof timestamptz
as $$
begin
  return query
  update data.connections
  set terminal_id=$2
  where id=$1::bigint
  returning id::timestamptz;
end $$ language plpgsql;

create function connection.open(
  _id timestamptz,
  _proto terminals.protocols,
  _uin bigint,
  _local_ip inet,
  _local_port bigint,
  _remote_ip inet,
  _remort_port bigint
) returns setof timestamptz as $$
begin
  return query
  with data_id as (
    insert into data.connections (id, protocol, type, terminal_id)
    values ($1, $2, 'ip', terminal.get($3, $2))
    returning id
  ) insert into connections.ip_data
  select D.id, $4, $5, $6, $7
  from data_id D returning id::timestamptz;
end $$ language plpgsql;

create function connection.close(
  _id timestamptz)
returns setof timestamptz
as $$
begin
  return query
  update data.connections
  set closed=current_timestamp
  where id=$1::bigint
  returning id::timestamptz;
end $$ language plpgsql;
