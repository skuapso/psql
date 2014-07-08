alter type terminals.protocols add value 'fort111';
create type fort111.boolean_sensors as enum ('penetration', 'ignition', 'glonass', 'gps');
create type fort111.integer_sensors as enum ('boot_no', 'rfid');
create type fort111.float_sensors as enum ('runned', 'hdop');
create type fort111.transmitters as enum ('gsm', 'wifi');
create type fort111.protocols as enum('2', '4');
create type fort111.gsm_sensors as enum('no', 'mmc', 'mnc', 'lac', 'cell_id');


create function fort111.upload_navigation() returns trigger as $$
declare
  oid bigint;
begin
  oid = terminal.object(packet.terminal(new.id));
  insert into events._data (id, type, object_id, terminal_id, time)
  select
    new.id
    ,packet.type(new.id)
    ,oid
    ,packet.terminal(new.id)
    ,case when new.eventtime is null then
      new.terminal_eventtime
    else
      new.eventtime
    end;
  insert into events._sensors
  select new.id, object.sensor(oid, 'analog', 'speed'), new.speed::varchar;
  insert into events._sensors
  select new.id, object.sensor(oid, 'location', 'location'),
        ('POINTZ('
        || new.longitude::float || ' '
        || new.latitude::float || ' '
        || new.altitude || ')')::geography::varchar
  where new.used>3
    and new.longitude is not null
    and new.latitude is not null
    and new.altitude is not null;
  return new;
end $$ language plpgsql;
