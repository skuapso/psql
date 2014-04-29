create type fort300.sensors as enum('started');

create table fort300.navigation (
  id timestamptz,
  action_id bigint not null,
  latitude navigation.coords_gm,
  longitude navigation.coords_gm,
  altitude real,
  eventtime timestamptz not null,
  speed real not null default 0,
  course real not null default 0,
  visible bigint not null default 0,
  used bigint not null default 0,
  signal real not null default 0,
  internal_power real not null default 0,
  external_power real not null default 0,
  msg_id bigint not null,
  macros_id bigint not null,
  gps_state bigint not null,

  constraint zidx_navigation_pk primary key(id),
  constraint zidx_navigation_fk foreign key(id) references data.packets(id) on delete cascade
);

create rule update_object_event
  as on insert
  to fort300.navigation
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
          || new.altitude || ')')::geography
      else
        null
      end
    );

create table fort300.sensor(
  id timestamptz,
  sensor fort300.sensors not null,
  value timestamptz not null,

  constraint zidx_sensor_pk primary key(id, sensor),
  constraint zidx_sensor_fk foreign key(id) references fort300.navigation(id) on delete cascade
);

create table fort300.digital_in(
  id timestamptz,
  sensor bigint not null,
  value boolean not null,

  constraint zidx_digital_in_pk primary key(id, sensor),
  constraint zidx_digital_in_fk foreign key(id) references fort300.navigation(id) on delete cascade
);

create table fort300.digital_out(
  id timestamptz,
  sensor bigint not null,
  value boolean not null,

  constraint zidx_digital_out_pk primary key(id, sensor),
  constraint zidx_digital_out_fk foreign key(id) references fort300.navigation(id) on delete cascade
);

create table fort300.analog(
  id timestamptz,
  sensor bigint not null,
  value real not null,

  constraint zidx_analog_pk primary key(id, sensor),
  constraint zidx_analog_fk foreign key(id) references fort300.navigation(id) on delete cascade
);

create table fort300.counter(
  id timestamptz,
  sensor bigint not null,
  value bigint not null,

  constraint zidx_counter_pk primary key(id, sensor),
  constraint zidx_counter_fk foreign key(id) references fort300.navigation(id) on delete cascade
);

create table fort300.lls(
  id timestamptz,
  sensor bigint not null,
  value bigint not null,

  constraint zidx_lls_pk primary key(id, sensor),
  constraint zidx_lls_fk foreign key(id) references fort300.navigation(id) on delete cascade
);

create table fort300.lls_freq(
  id timestamptz,
  sensor bigint not null,
  value real not null,

  constraint zidx_lls_freq_pk primary key(id, sensor),
  constraint zidx_lls_freq_fk foreign key(id) references fort300.navigation(id) on delete cascade
);

create table fort300.lls_temp(
  id timestamptz,
  sensor bigint not null,
  value real not null,

  constraint zidx_lls_temp_pk primary key(id, sensor),
  constraint zidx_lls_temp_fk foreign key(id) references fort300.navigation(id) on delete cascade
);

create table fort300.rfid(
  id timestamptz,
  sensor bigint not null,
  value bigint not null,

  constraint zidx_rfig_pk primary key(id, sensor),
  constraint zidx_rfid_fk foreign key(id) references fort300.navigation(id) on delete cascade
);
