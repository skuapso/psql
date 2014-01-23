create table teltonika.active(
  id timestamptz not null,
  uin bigint not null,

  constraint zidx_active_pk primary key(id),
  constraint zidx_active_fk foreign key(id) references data.packets(id) on delete cascade
);

create table teltonika.navigation(
  id timestamptz not null,
  eventtime timestamptz not null,
  latitude float not null,
  longitude float not null,
  altitude float not null,
  active_sensor bigint not null default 0,
  used bigint not null,
  course float not null,
  speed float not null,

  constraint zidx_parsed_pk primary key(id),
  constraint zidx_parsed_fk foreign key(id) references data.packets(id) on delete cascade
);

create table teltonika.digital(
  id timestamptz not null,
  sensor bigint not null,
  value bigint not null,

  constraint zidx_digital_pl primary key(id, sensor),
  constraint zidx_digital_fk foreign key(id) references teltonika.navigation(id) on delete cascade
);
