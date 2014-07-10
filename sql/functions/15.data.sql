create type data.types as enum(
  'unknown',
  'broken',
  'ping',
  'low',
  'offline',
  'online',
  'high',
  'panic',
  'security',
  'authentication'
);

create type connections.types as enum(
  'ip'
);

create function data.binary_id(_id timestamptz, _data bytea) returns timestamptz as $$
declare
  i timestamptz;
begin
  select data_id into i from data.binary where md5(data)=md5($2) limit 1;
  if found then
    return i;
  end if;
  insert into data.binary values ($1, $2);
  return $1;
end $$ language plpgsql;
