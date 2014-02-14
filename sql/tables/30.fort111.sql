create type fort111.boolean_sensors as enum ('penetration', 'ignition', 'glonass', 'gps');
create type fort111.integer_sensors as enum ('boot_no', 'rfid');
create type fort111.float_sensors as enum ('runned', 'hdop');
create type fort111.transmitters as enum ('gsm', 'wifi');
create type fort111.protocols as enum('2', '4');
create type fort111.gsm_sensors as enum('no', 'mmc', 'mnc', 'lac', 'cell_id');

create table fort111.navigation(
  id timestamptz,
  protocol fort111.protocols not null,
  terminal_eventtime timestamptz,
  internal_power real,
  external_power real,
  latitude navigation.coords_gm,
  longitude navigation.coords_gm,
  speed real,
  course real,
  altitude real,
  eventtime timestamptz,
  turn_on boolean,
  valid boolean,
  visible bigint,
  used bigint,
  crc bigint not null,
  msg_id bigint not null,
  transmit fort111.transmitters not null default 'gsm',

  constraint zidx_navigation_pk primary key(id),
  constraint zidx_navigation_fk foreign key(id) references data.packets(id) on delete cascade
);

create rule update_object_event
  as on insert
  to fort111.navigation
  where (terminal.object(packet.terminal(new.id)) is not null)
  do also
    insert into events.data
    (id, type, object_id, terminal_id, time, location)
    values (
      new.id
      ,packet.type(new.id)
      ,terminal.object(packet.terminal(new.id))
      ,packet.terminal(new.id)
      ,case when new.eventtime is null then
        new.terminal_eventtime
      else
        new.eventtime
      end
      ,case when new.used > 3 then
        ('POINTZ('
          || new.longitude::float || ' '
          || new.latitude::float || ' '
          || new.altitude || ')')::geography
      else
        null
      end
    );
create rule update_object_event_speed
  as on insert
  to fort111.navigation
  where (
    terminal.object(packet.terminal(new.id)) is not null
    and new.speed is not null
    and object.sensor(terminal.object(packet.terminal(new.id)), 'speed', 'speed') is not null
  )
  do also
    insert into events.sensors
    values (
      new.id,
      object.sensor(terminal.object(packet.terminal(new.id)), 'speed', 'speed'),
      new.speed::varchar
    );

create table fort111.signal(
  id timestamptz,
  sensor fort111.transmitters not null,
  value real not null,

  constraint zidx_signal_pk primary key(id, sensor),
  constraint zidx_signal_fk foreign key(id) references fort111.navigation(id) on delete cascade
);

create table fort111.gsm_info(
  id timestamptz,
  no bigint not null,
  mmc bigint not null,
  mnc bigint not null,
  lac bigint not null,
  cell_id bigint not null,

  constraint zidx_gsm_info_pk primary key(id, no),
  constraint zidx_gsm_info_fk foreign key(id) references fort111.navigation(id) on delete cascade
);

create table fort111.boolean_sensor(
  id timestamptz,
  sensor fort111.boolean_sensors not null,
  value boolean not null,

  constraint zidx_boolean_sensor_pk primary key(id, sensor),
  constraint zidx_boolean_sensor_fk foreign key(id) references fort111.navigation(id) on delete cascade
);

create table fort111.integer_sensor(
  id timestamptz,
  sensor fort111.integer_sensors not null,
  value bigint not null,

  constraint zidx_integer_sensor_pk primary key(id, sensor),
  constraint zidx_integer_sensor_fk foreign key(id) references fort111.navigation(id) on delete cascade
);

create table fort111.float_sensor(
  id timestamptz,
  sensor fort111.float_sensors not null,
  value real not null,

  constraint zidx_float_sensor_pk primary key(id, sensor),
  constraint zidx_float_sensor_fk foreign key(id) references fort111.navigation(id) on delete cascade
);

create table fort111.digital_in(
  id timestamptz,
  sensor bigint not null,
  value boolean not null,

  constraint zidx_digital_in_pk primary key(id, sensor),
  constraint zidx_digital_in_fk foreign key(id) references fort111.navigation(id) on delete cascade
);

create rule update_object_event
  as on insert
  to fort111.digital_in
  where (
    terminal.object(packet.terminal(new.id)) is not null
    and object.sensor(terminal.object(packet.terminal(new.id)), 'digital', new.sensor::varchar) is not null
  )
  do also
    insert into events.sensors
    values (
      new.id,
      object.sensor(terminal.object(packet.terminal(new.id)), 'digital', new.sensor::varchar),
      new.value::varchar
    );

create table fort111.digital_out(
  id timestamptz,
  sensor bigint not null,
  value boolean not null,

  constraint zidx_digital_out_pk primary key(id, sensor),
  constraint zidx_digital_out_fk foreign key(id) references fort111.navigation(id) on delete cascade
);

create table fort111.analog(
  id timestamptz,
  sensor bigint not null,
  value real not null,

  constraint zidx_analog_pk primary key(id, sensor),
  constraint zidx_analog_fk foreign key(id) references fort111.navigation(id) on delete cascade
);

create table fort111.counter(
  id timestamptz,
  sensor bigint not null,
  value bigint not null,

  constraint zidx_counter_pk primary key(id, sensor),
  constraint zidx_counter_fk foreign key(id) references fort111.navigation(id) on delete cascade
);

create table fort111.lls(
  id timestamptz,
  sensor bigint not null,
  value bigint not null,

  constraint zidx_lls_pk primary key(id, sensor),
  constraint zidx_lls_fk foreign key(id) references fort111.navigation(id) on delete cascade
);

create table fort111.can_lls(
  id timestamptz,
  sensor bigint not null,
  value real not null,

  constraint zidx_can_lls_pk primary key(id, sensor),
  constraint zidx_can_lls_fk foreign key(id) references fort111.navigation(id) on delete cascade
);

create table fort111.can(
  id timestamptz,
  speed real,
  rpm real,
  fuel_consumption real,
  mhs real,
  runned real,
  weight real,

  constraint zidx_can_pk primary key(id),
  constraint zidx_can_fk foreign key(id) references fort111.navigation(id) on delete cascade
);

create table fort111.one_wire(
  id timestamptz,
  sensor bigint not null,
  value bigint not null,

  constraint zidx_one_wire_pk primary key(id, sensor),
  constraint zidx_one_wire_fk foreign key(id) references fort111.navigation(id) on delete cascade
);
