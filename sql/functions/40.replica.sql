create type replica.issue
as enum(
  'connection_timeout',
  'connection_rejected',
  'send_timeout',
  'waiting_closed',
  'wrong_answer'
);

create type replica.connection_types
as enum(
  'soft',
  'aggressive'
);

create function replica.get_servers(
  _connection_id timestamptz)
returns table(
  server_id bigint,
  server_protocol terminals.protocols)
as $$
declare
  _terminal_id bigint = connection.terminal(_connection_id);
begin
  return query select * from replica.get_servers(_terminal_id, _connection_id);
end $$ language plpgsql stable;

create or replace function replica.get_servers(
  _terminal_id bigint,
  _connection_id timestamptz)
returns table(
  server_id bigint,
  server_protocol terminals.protocols)
as $$
declare
  _object_id bigint = terminal.object(_terminal_id);
  _group_id bigint  = object.group(_object_id);
  _specialization_id bigint = object.specialization(_object_id);
  _local_port bigint = connection.local_port(_connection_id);
  _terminal_protocol terminals.protocols = connection.protocol(_connection_id);
begin
  return query
    select * from
    replica.get_servers(_object_id, _group_id, _specialization_id, _local_port, _terminal_protocol);
end $$ language plpgsql stable;

create function replica.get_servers(
  _object_id bigint,
  _group_id bigint,
  _specialization_id bigint,
  _local_port bigint,
  _terminal_protocol terminals.protocols)
returns table(
  server_id bigint,
  server_protocol terminals.protocols)
as $$
begin
  return query
  select distinct
    S.server_id,
    replica.server_protocol(S.server_id, _terminal_protocol)
  from (
    select distinct replica.owner_server(owner_id, _terminal_protocol) as server_id
    from (
      select IR1.owner_id
      from replica.rules IR1
      where
        type = 'include'
        and terminal_protocol = _terminal_protocol
        and IR1.owner_id not in (
          select ER1.owner_id
          from replica.rules ER1
          where
            type = 'exclude'
            and (
              local_port = _local_port
              or object_id=_object_id
              or specialization_id=_specialization_id
              or (
                group_id is not null
                and array[_group_id] <@ ("group".childs(group_id) || group_id))))

      union all
      select IR2.owner_id
      from replica.rules IR2
      where
        type = 'include'
        and local_port = _local_port
        and IR2.owner_id not in (
          select ER2.owner_id
          from replica.rules ER2
          where
            type = 'exclude'
            and (
              terminal_protocol = _terminal_protocol
              or object_id=_object_id
              or specialization_id = _specialization_id
              or (
                group_id is not null
                and array[_group_id] <@ ("group".childs(group_id) || group_id))))

      union all
      select IR3.owner_id
      from replica.rules IR3
      where
        type = 'include'
        and group_id is not null
        and array[_group_id] <@ ("group".childs(group_id) || group_id)
        and IR3.owner_id not in (
          select ER3.owner_id
          from replica.rules ER3
          where
            type = 'exclude'
            and (
              terminal_protocol=_terminal_protocol
              or local_port=_local_port
              or object_id=_object_id
              or specialization_id = _specialization_id
              or (
                group_id is not null
                and array[_group_id] <@ ("group".childs(group_id) || group_id))))

      union all
      select IR4.owner_id
      from replica.rules IR4
      where type = 'include'
        and specialization_id=_specialization_id
      and IR4.owner_id not in (
        select ER4.owner_id
        from replica.rules ER4
        where
          type = 'exclude'
          and object_id=_object_id)

      union all
      select IR5.owner_id
      from
        replica.rules IR5
      where
        type = 'include'
        and object_id = _object_id
    ) O
  ) S;
end
$$ language plpgsql stable;

create function replica.owner_server(
  _id bigint,
  _prefered terminals.protocols)
returns bigint
as $$
declare
  s bigint;
begin
  select server_id into s
  from replica."owners<->servers" OS
  inner join replica.servers S on (OS.server_id=S.id)
  where OS.owner_id=$1
  and array[$2] <@ S.protocols
  limit 1;
  if not found then
    select server_id into s
    from replica."owners<->servers" OS
    inner join replica.servers S on (OS.server_id=S.id)
    where OS.owner_id = $1
    limit 1;
  end if;
  return s;
end $$ language plpgsql stable;

create function replica.server_protocol(
  _id bigint,
  _prefered terminals.protocols)
returns terminals.protocols
as $$
declare
  p terminals.protocols;
begin
  select $2 into p from replica.servers where id=$1 and array[$2] <@ protocols;
  if found then
    return p;
  end if;
  select protocols[1] into p from replica.servers where id=$1;
  return p;
end $$ language plpgsql stable;

create function replica.undelivered() returns table(
  server_id bigint,
  protocol terminals.protocols,
  terminal_protocol terminals.protocols,
  terminal_uin bigint)
as $$
begin
  return query
  select
    S.server_id,
    S.protocol,
    S.tproto[1],
    S.tuin
  from (
    select
      D.server_id,
      D.protocol,
      terminal.protocols(D.terminal_id) as tproto,
      terminal.uin(D.terminal_id) as tuin
    from
      replica.data D
    where
      D.answer_id is null
    order by D.id
    limit 1
  ) as S;
end $$ language plpgsql;

create function replica.undelivered(
  _server_id bigint)
returns table(
  server_id bigint,
  protocol terminals.protocols,
  terminal_protocol terminals.protocols,
  terminal_uin bigint)
as $$
begin
  return query
  select
    S.server_id,
    S.protocol,
    S.tproto[1],
    S.tuin
  from (
    select
      D.server_id,
      D.protocol,
      terminal.protocols(D.terminal_id) as tproto,
      terminal.uin(D.terminal_id) as tuin
    from
      replica.data D
    where
      D.answer_id is null
      and D.server_id=$1
    order by D.id
    limit 1
  ) as S;
end $$ language plpgsql;

create function replica.undelivered(
  _server_id bigint,
  _terminal_id bigint,
  _max_points bigint)
returns table(
  id timestamptz,
  data bytea,
  protocol terminals.protocols)
as $$
begin
  return query
  select
    D.id,
    B.data,
    D.protocol
  from
    replica.data D
  inner join
    data."binary" B
  using(data_id)
  where
    D.answer_id is null
    and D.server_id=$1
    and D.terminal_id=$2
  order by D.id
  limit $3;
end $$ language plpgsql;

create function replica.add_data(
  _id timestamptz,
  _parent_id timestamptz,
  _server_id bigint,
  _proto terminals.protocols,
  _data bytea,
  _terminal_id bigint)
returns setof timestamptz
as $$
begin
  return query
  insert into replica.data
  values ($1, $2, $3, $4, $6, data.binary_id($1, $5), null) returning id;
end $$ language plpgsql;

create function replica.set_answer(
  _id timestamptz,
  _answer bytea,
  _connection_id timestamptz,
  _data_ids timestamptz[])
returns setof timestamptz
as $$
begin
  return query
  with
    answer as (
      insert into replica.answers(id, connection_id, data_id)
      select $1, $3, data.binary_id($1, $2)
      returning id,data_id),
    data as (
      update
        replica.data D
      set
        (answer_id) = (answer.id)
      from
        answer
      where
        D.id in (select unnest($4))
      returning D.id)
  select answer.id from answer;
end $$ language plpgsql;
