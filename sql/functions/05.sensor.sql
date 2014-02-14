create function sensor.virtual(_sensor_id bigint) returns bool as $$
declare
  v bool;
begin
  select virtual into v
  from sensors.data D
  inner join sensors.models M on(D.model_id = M.id)
  where D.id=$1
  limit 1;
  return v;
end $$ language plpgsql immutable;

create function sensor.port_type(_sensor_id bigint) returns terminal.port_types as $$
declare
  pt terminal.port_types;
begin
  select port_type into pt from sensors.data D
  inner join sensors.models M on (D.model_id = M.id)
  inner join sensors.types T on (M.type_id = T.id)
  where D.id=$1;
  return pt;
end $$ language plpgsql stable;

create function sensor.data_type(_sensor_id bigint) returns varchar as $$
declare
  pt varchar;
begin
  select data_type into pt from sensors.data D
  inner join sensors.models M on (D.model_id = M.id)
  inner join sensors.types T on (M.type_id = T.id)
  where D.id=$1;
  return pt;
end $$ language plpgsql stable;

create function sensor.object(_object_sensor_id bigint) returns bigint as $$
declare
  oid bigint;
begin
  select object_id into oid from objects.sensors where id=$1;
  return oid;
end $$ language plpgsql stable;
