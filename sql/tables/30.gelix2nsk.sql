create type gelix2nsk.sensors as enum('last_valid', 'restart_hw_fail', 'coldstart', 'zone_alarm');

create table gelix2nsk.navigation(
  id timestamptz,
  msg_id bigint not null,
  action_id bigint not null,
  latitude navigation.coords_gm not null,
  longitude navigation.coords_gm not null,
  altitude real not null,
  eventtime timestamptz not null,
  speed real not null,
  course real not null,
  used bigint not null,
  external_power real not null,
  valid boolean not null,
  new_record boolean not null,
  fix_gga bigint not null,
  hdop real not null,
  hmet real not null,

  constraint zidx_navigation_pk primary key(id),
  constraint zidx_navigation_fk foreign key(id) references data.packets(id) on delete cascade
);

create rule update_object_event
  as on insert
  to gelix2nsk.navigation
  where (terminal.object(packet.terminal(new.id)) is not null)
  do also
    insert into events._data
    (id, type, object_id, terminal_id, time, location)
    values (
      new.id
      ,packet.type(new.id)
      ,terminal.object(packet.terminal(new.id))
      ,packet.terminal(new.id)
      ,new.eventtime
      ,case when new.used > 3 then
        ('POINTZ('
            || new.longitude::float || ' '
            || new.latitude::float || ' '
            || new.hmet || ')')::geography
      else
        null
      end
    );

create table gelix2nsk.digital_in(
  id timestamptz,
  sensor bigint not null,
  value boolean not null,

  constraint zidx_digital_in_pk primary key(id, sensor),
  constraint zidx_digital_in_fk foreign key(id) references gelix2nsk.navigation(id) on delete cascade
);

create table gelix2nsk.digital_out(
  id timestamptz,
  sensor bigint not null,
  value boolean not null,

  constraint zidx_digital_out_pk primary key(id, sensor),
  constraint zidx_digital_out_fk foreign key(id) references gelix2nsk.navigation(id) on delete cascade
);

create table gelix2nsk.analog(
  id timestamptz,
  sensor bigint not null,
  value real not null,

  constraint zidx_analog_pk primary key(id, sensor),
  constraint zidx_analog_fk foreign key(id) references gelix2nsk.navigation(id) on delete cascade
);

create table gelix2nsk.sensor(
  id timestamptz,
  sensor gelix2nsk.sensors not null,
  value bigint not null,

  constraint zidx_sensor_pk primary key(id, sensor),
  constraint zidx_sensor_fk foreign key(id) references gelix2nsk.navigation(id) on delete cascade
);
