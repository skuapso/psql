create table events.data(
  id timestamptz
    default current_timestamp
    constraint zidx_data_pk primary key
  ,type data.types
    not null
  ,object_id bigint
    not null
    constraint zidx_data_fk_object references objects._data(id) on delete cascade
  ,terminal_id bigint
    constraint zidx_data_fk_terminal references terminals._data(id) on delete set null
  ,time timestamptz
    not null
  ,location geography
  ,valid boolean
    not null
    default true
  ,next timestamptz
    constraint zidx_data_fk_next references events.data(id) initially deferred
    constraint zidx_data_uk_next unique initially deferred
  ,prev timestamptz
    constraint zidx_data_fk_prev references events.data(id) initially deferred
    constraint zidx_data_uk_prev unique initially deferred
);
create unique index zidx_data_uk_object_time_valid
on events.data(object_id, time)
where valid;
create index zidx_data_uk_object_time_not_valid
on events.data(object_id, time)
where not valid;

alter table objects._data
  add last_event_id timestamptz
  constraint zidx_data_fk_last_event references events.data(id) on delete cascade;

create table events.sensors(
  id timestamptz
    constraint zidx_sensors_fk references events.data(id)
  ,sensor_id bigint
    not null
    constraint zidx_data_fk_sensor references objects.sensors(id)
  ,value varchar
    not null
);
create index zidx_sensors_ik_id on events.sensors(id);

create trigger pre_i_00_check_object
  before insert
  on events.data
  for each row
  when (new.object_id is null)
  execute procedure triggers.reject();

create trigger pre_i_04_check_time
  before insert
  on events.data
  for each row
  when (new.time > new.id)
  execute procedure event.set_not_valid();

create trigger pre_i_05_check_presence
  before insert
  on events.data
  for each row
  when (new.valid and checking.is_value_presents(
    'events'::varchar, 'data'::varchar,
    array['time', 'object_id', 'valid']::varchar[],
    array[E'\'' || new.time || E'\'', new.object_id, true]::varchar[]
  ))
  execute procedure event.set_not_valid();

create trigger pre_i_20_set_neighbours
  before insert
  on events.data
  for each row
  when (new.valid)
  execute procedure event.set_neighbours();

create trigger post_i_40_update_object
  after insert
  on events.data
  for each row
  when (new.next is null and (new.id>new.time) and new.valid)
  execute procedure event.update_object();
