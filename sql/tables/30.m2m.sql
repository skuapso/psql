create table m2m.navigation(
  id bigint,
  eventtime timestamp with time zone not null,
  latitude navigation.coords_gm not null,
  longitude navigation.coords_gm not null,
  speed real not null,
  course real not null,
  used bigint not null,
  validation bigint not null,
  msg_id bigint not null,
  analize bigint not null,
  action bigint not null,

  constraint zidx_navigation_pk primary key (id),
  constraint zidx_navigation_fk foreign key (id) references data.packets(id) on delete cascade
);

create table m2m.active(
  id bigint,
  eventtime timestamp with time zone not null,
  msg_id bigint not null,

  constraint zidx_active_pk primary key (id),
  constraint zidx_active_fk foreign key (id) references data.packets(id) on delete cascade
);
