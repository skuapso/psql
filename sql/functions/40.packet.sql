create function packet.terminal(_packet_id bigint) returns bigint as $$
declare
  i bigint;
begin
  select terminal_id into i
  from data.packets P
  inner join data.raws R on (P.raw_id=R.id)
  inner join data.connections C on (R.connection_id = C.id)
  where P.id=$1;
  return i;
end $$ language plpgsql stable;

create function packet.type(_packet_id bigint) returns data.types as $$
declare
  t data.types;
begin
  select P.type into t from data.packets P where id=$1;
  return t;
end $$ language plpgsql stable;
