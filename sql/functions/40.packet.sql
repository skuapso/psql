create function packet.terminal(
  _id bigint,
  _is_raw bool default false
) returns bigint
as $$
declare
  i bigint;
begin
  if $2 then
    select terminal_id into i
    from data.raws R
    inner join data.connections C on (R.connection_id = C.id)
    where R.id=$1;
  else
    select terminal_id into i
    from data.packets P
    inner join data.raws R on (P.raw_id=R.id)
    inner join data.connections C on (R.connection_id = C.id)
    where P.id=$1;
  end if;
  return i;
end $$ language plpgsql stable strict;

create function packet.object(
  _id bigint,
  _is_raw bool default false
) returns bigint
as $$
declare
  i bigint;
begin
  if $2 then
    select O.id into i
    from data.raws R
    inner join data.connections C on (R.connection_id = C.id)
    inner join objects._data O using (terminal_id)
    where R.id=$1;
  else
    select O.id into i
    from data.packets P
    inner join data.raws R on (P.raw_id=R.id)
    inner join data.connections C on (R.connection_id = C.id)
    inner join objects._data O using (terminal_id)
    where P.id=$1;
  end if;
  return i;
end $$ language plpgsql stable strict;

create function packet.type(_packet_id bigint) returns data.types as $$
declare
  t data.types;
begin
  select P.type into t from data.packets P where id=$1;
  return t;
end $$ language plpgsql stable strict;

create function packet.protocol(
  _id bigint,
  _is_raw bool default false
) returns terminals.protocols
as $$
declare
  i terminals.protocols;
  q text;
begin
  q = 'select C.protocol from';
  if $2 then
    q = q || ' data.raws P';
  else
    q = q || ' data.packets P inner join data.raws R on (P.raw_id=R.id)';
  end if;
  q = q || ' inner join data.connections C on (R.connection_id = C.id)';
  q = q || ' where P.id=$1';
  execute q using $1 into i;
  return i;
end $$ language plpgsql stable strict;
