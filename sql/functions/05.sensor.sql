create function sensor.port_types(_sensor_id bigint) returns terminal.port_types[] as $$
declare
  pt terminal.port_types[];
begin
  select array_agg(port_type) into pt
  from sensors.types T
  inner join sensors.type_ports TP on(T.id=TP.id)
  where T.id=$1;
  if found then
    return pt;
  end if;
  select array_agg(port_type) into pt
  from sensors.models M
  inner join sensors.types T on (M.type_id=T.id)
  inner join sensors.type_ports TP on(T.id=TP.id)
  where M.id=$1;
  if found then
    return pt;
  end if;
  select array_agg(port_type) into pt from sensors.data D
  inner join sensors.models M on (D.model_id = M.id)
  inner join sensors.types T on (M.type_id = T.id)
  inner join sensors.type_ports TP on(T.id=TP.id)
  where D.id=$1;
  return pt;
end $$ language plpgsql stable;

create function sensor.compute_fun(_sensor_id bigint) returns varchar as $$
declare
  cf varchar;
begin
  select compute_fun into cf
  from sensors.types T
  inner join sensors.type_ports TP on(T.id=TP.id)
  where T.id=$1;
  if found then
    return cf;
  end if;
  select compute_fun into cf
  from sensors.models M
  inner join sensors.types T on (M.type_id=T.id)
  inner join sensors.type_ports TP on(T.id=TP.id)
  where M.id=$1;
  if found then
    return cf;
  end if;
  select compute_fun into cf from sensors._data D
  inner join sensors.models M on (D.model_id = M.id)
  inner join sensors.types T on (M.type_id = T.id)
  inner join sensors.type_ports TP on(T.id=TP.id)
  where D.id=$1;
  return cf;
end $$ language plpgsql stable;

create function sensor.object(_object_sensor_id bigint) returns bigint as $$
declare
  oid bigint;
begin
  select object_id into oid from objects.sensors where id=$1;
  return oid;
end $$ language plpgsql stable;

create function sensor.compute_location(_object_id bigint, _sensor_id bigint, _val jsonb)
returns jsonb
as $$
  var loc = JSON.parse(_val);
  loc.latitude = plv8.ll_convert(loc.latitude);
  loc.longitude = plv8.ll_convert(loc.longitude);
  return JSON.stringify(loc);
$$ language plv8 immutable;

create function sensor.type(_sensor_id bigint)
returns bigint
as $$
declare
  i bigint;
begin
  select id into i from sensors.types where id=$1;
  if found then
    return i;
  end if;
  select id into i
  from sensors.models M
  inner join sensors.types T on (M.type_id=T.id)
  where M.id=$1;
  if found then
    return i;
  end if;
  select id into i
  from sensors.data D
  inner join sensors.models M on (D.model_id = M.id)
  inner join sensors.types T on (M.type_id = T.id)
  where D.id=$1;
  return i;
end $$ language plpgsql stable;

create function sensor.purpose_type(_purpose_id bigint)
returns bigint
as $$
declare
  i bigint;
begin
  select type_id into i from sensors._purposes where id=$1;
  return i;
end $$ language plpgsql stable;
