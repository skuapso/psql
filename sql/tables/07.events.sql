create sequence events.seq__data;
create table events._data(
  id bigint
    default nextval('events.seq__data')
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
  ,valid boolean
    not null
    default true
  ,data jsonb
  ,next bigint
    constraint zidx_data_fk_next references events._data(id) initially deferred
  ,prev bigint
    constraint zidx_data_fk_prev references events._data(id) initially deferred
);
create unique index zidx_data_uk_next on events._data(next);
create unique index zidx_data_uk_prev on events._data(prev);

create unique index zidx_data_uk_object_time_valid
on events._data(object_id, time)
where valid;

create index zidx_data_uk_object_time_not_valid
on events._data(object_id, time)
where not valid;

alter table objects._data
  add last_event_id bigint
  constraint zidx_data_fk_last_event references events._data(id)
  on delete set null;

create trigger pre_i_00_check_object
  before insert
  on events._data
  for each row
  when (new.object_id is null)
  execute procedure triggers.reject();

create trigger pre_i_03_check_type
  before insert
  on events._data
  for each row
  when (new.valid and new.type='broken')
  execute procedure event.set_not_valid();

create trigger pre_i_04_check_time
  before insert
  on events._data
  for each row
  when (new.valid and new.time > new.id::timestamptz)
  execute procedure event.set_not_valid();

create trigger pre_i_05_check_presence
  before insert
  on events._data
  for each row
  when (new.valid and checking.is_value_presents(
    'events'::varchar, '_data'::varchar,
    array['time', 'object_id', 'valid']::varchar[],
    array[E'\'' || new.time || E'\'', new.object_id, true]::varchar[]
  ))
  execute procedure event.set_not_valid();

create trigger pre_i_20_set_neighbours
  before insert
  on events._data
  for each row
  when (new.valid)
  execute procedure event.set_neighbours();

create trigger pre_i_25_set_diffs
  before insert
  on events._data
  for each row
  when (new.valid and new.prev is not null)
  execute procedure event.set_diffs();

create trigger pre_i_26_correct_diffs
  before insert
  on events._data
  for each row
  when (new.valid and new.next is not null)
  execute procedure event.correct_diffs();

create trigger pre_i_30_set_sensors
  before insert
  on events._data
  for each row
  execute procedure event.set_sensors_data();

create trigger pre_i_40_update_location
  before insert
  on events._data
  for each row
  when (new.valid and
--    (пройдена дистанция до границы полигона, изменились предположительные места)
    false
  )
  execute procedure event.set_places();

create trigger pre_i_90_merge_data
  before insert
  on events._data
  for each row
  when (new.valid)
  execute procedure event.merge_data();

create trigger post_i_50_update_object
  after insert
  on events._data
  for each row
  when (new.next is null and new.valid)
  execute procedure event.update_object();
