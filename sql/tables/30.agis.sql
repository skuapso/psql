create table agis.navigation(
  id bigint,
  msg_id bigint not null,
  action_id bigint not null,
  latitude navigation.coords_gm not null,
  longitude navigation.coords_gm not null,
  altitude real not null,
  eventtime timestamp with time zone not null,
  speed real not null,
  course real not null,
  used bigint not null,
  valid boolean not null,
  new_record boolean not null,
  fix_gga bigint not null,
  hdop real not null,
  hmet real not null,

  constraint zidx_navigation_pk primary key(id),
  constraint zidx_navigation_fk foreign key(id) references data.packets(id) on delete cascade
);
