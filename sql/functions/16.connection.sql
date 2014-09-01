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
  where id=$1
  returning id;
end $$ language plpgsql;
