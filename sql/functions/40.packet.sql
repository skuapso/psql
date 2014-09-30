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
    where R.id=$1::bigint;
  else
    select terminal_id into i
    from data.packets P
    inner join data.raws R on (P.raw_id=R.id)
    inner join data.connections C on (R.connection_id = C.id)
    where P.id=$1::bigint;
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
    where R.id=$1::bigint;
  else
    select O.id into i
    from data.packets P
    inner join data.raws R on (P.raw_id=R.id)
    inner join data.connections C on (R.connection_id = C.id)
    inner join objects._data O using (terminal_id)
    where P.id=$1::bigint;
  end if;
  return i;
end $$ language plpgsql stable strict;

create function packet.type(_packet_id timestamptz) returns data.types as $$
declare
  t data.types;
begin
  select P.type into t from data.packets P where id=$1;
  return t;
end $$ language plpgsql stable strict;
