create table events.data(
  id timestamptz default current_timestamp
    constraint zidx_data_pk primary key
  ,type data.types not null
  ,object_id bigint not null
  ,terminal_id bigint not null
  ,time timestamptz not null
  ,valid boolean
  ,location geography(pointz, 4326)
  ,next timestamptz
  ,prev timestamptz

  ,constraint zidx_data_uk_object_time unique(time, object_id)
);
create index zidx_data_ik_object_time on events.data(object_id, time);
alter table objects.data
  add last_event_id timestamptz
  constraint zidx_data_fk_last_event references events.data(id);

create trigger insertb_00_check_object
  before insert
  on events.data
  for each row
  when (new.object_id is null)
  execute procedure triggers.reject();

create trigger insertb_05_check_time
  before insert
  on events.data
  for each row
  when (checking.is_value_presents(
    'events'::varchar, 'data'::varchar,
    array['time', 'object_id']::varchar[],
    array[E'\'' || new.time || E'\'', new.object_id]::varchar[]
  ))
  execute procedure triggers.reject();

create trigger insertb_10_check_time
  before insert
  on events.data
  for each row
  when (new.time>(current_timestamp + interval '0:0:2'))
  execute procedure event.set_not_valid();

create trigger insertb_20_set_neighbours
  before insert
  on events.data
  for each row
  when (new.valid)
  execute procedure event.set_neighbours();

create trigger inserta_30_update_prev
  after insert
  on events.data
  for each row
  when (new.prev is not null)
  execute procedure event.update_prev();

create trigger inserta_30_update_next
  after insert
  on events.data
  for each row
  when (new.next is not null)
  execute procedure event.update_next();

create trigger inserta_40_update_object
  after insert
  on events.data
  for each row
  when (new.next is null and new.valid)
  execute procedure event.update_object();
