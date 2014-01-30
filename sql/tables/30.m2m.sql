create table m2m.navigation(
  id timestamptz,
  eventtime timestamptz not null,
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

create rule update_object_event
  as on insert
  to m2m.navigation
  where (terminal.object(packet.terminal(new.id)) is not null)
  do also
    insert into events.data
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
            || new.latitude::float || ' 0)')::geography
      else
        null
      end
    );

create table m2m.active(
  id timestamptz,
  eventtime timestamptz not null,
  msg_id bigint not null,

  constraint zidx_active_pk primary key (id),
  constraint zidx_active_fk foreign key (id) references data.packets(id) on delete cascade
);
