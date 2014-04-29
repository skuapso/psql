create table agis.navigation(
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
  to agis.navigation
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
