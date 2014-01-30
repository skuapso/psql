create type sensor.purposes as enum(
  'location'
  ,'supply'
  ,'ignition'
  ,'external_power'
  ,'battery_power'

  ,'runhours'
  ,'motohours'
  ,'odometer'
  ,'lls'
);

create function sensor.purpose(_sensor_id bigint) returns sensor.purposes as $$
declare
  p sensor.purposes;
begin
  select purpose into p
  from sensors.data D
  inner join sensors.models M on(D.model_id = M.id)
  inner join sensors.types T on(M.type_id = T.id)
  where D.id=$1
  limit 1;
  return p;
end $$ language plpgsql immutable;

create function sensor.virtual(_sensor_id bigint) returns bool as $$
declare
  v bool;
begin
  select virtual into v
  from sensors.data D
  inner join sensors.models M on(D.model_id = M.id)
  inner join sensors.types T on(M.type_id = T.id)
  where D.id=$1
  limit 1;
  return v;
end $$ language plpgsql immutable;
