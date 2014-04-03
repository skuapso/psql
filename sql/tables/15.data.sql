--------------------------------------------------------------------------------
-- data.connections
--------------------------------------------------------------------------------
create table data.connections(
  id timestamptz not null default current_timestamp,
  protocol terminals.protocols not null,
  ended timestamptz default null,
  type connections.types not null,
  terminal_id bigint,

  constraint zidx_connections_pk primary key(id),
  constraint zidx_connections_fk_terminal foreign key(terminal_id) references terminals._data(id) on delete set null
);
create index zidx_connections_ik_terminal on data.connections(terminal_id);
comment on table data.connections is 'table for connections';

create trigger insertb_zz_correct_id
  before insert on data.connections for each row
  when (checking.is_value_presents('data', 'connections', 'id', new.id))
  execute procedure triggers.correct_timestamp_id();
--------------------------------------------------------------------------------
-- /data.connections
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- data.broken
--------------------------------------------------------------------------------
create table data.broken(
  like basic.binary_data including defaults including comments,
  connection_id timestamptz not null default current_timestamp,
  count bigint not null default 0,
  last timestamptz not null default current_timestamp,

  constraint zidx_broken_pk primary key(id),
  constraint zidx_broken_fk_connection foreign key(connection_id) references data.connections(id) on delete cascade
);
create index zidx_broken_ik_connection on data.broken(connection_id);
create unique index zidx_broken_uk_data on data.broken(md5(data));

create rule add_count as
  on insert to data.broken
  where (checking.is_value_presents('data', 'broken', 'data', new.data)) do also
  update data.broken set count=count+1,last=now() where md5(data)=md5(new.data);

create trigger insertb_00_check_data
  before insert on data.broken for each row
  when (checking.is_value_presents('data', 'broken', 'data', new.data))
  execute procedure triggers.reject();

create trigger insertb_zz_correct_id
  before insert on data.broken for each row
  when (checking.is_value_presents('data', 'broken', 'id', new.id))
  execute procedure triggers.correct_timestamp_id();
--------------------------------------------------------------------------------
-- /data.broken
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- data.raws
--------------------------------------------------------------------------------
create table data.raws(
  like basic.binary_data including defaults including comments,
  connection_id timestamptz not null,
  count bigint not null default 0,
  answer bytea,
  answered varchar,
  last timestamptz not null default current_timestamp,

  constraint zidx_raws_pk primary key(id),
  constraint zidx_raws_fk_connection foreign key(connection_id) references data.connections(id) on delete cascade
);
create index zidx_raws_ik_connection on data.raws(connection_id);
create unique index zidx_raws_uk_data on data.raws(md5(data));

create rule add_answer as
  on insert to data.raws
  where (new.id is not null and new.answer is not null)
  do also
    update data.raws
    set answer=new.answer,
      answered=new.answered
    where id=new.id;

create rule add_count as
  on insert to data.raws
  where (checking.is_value_presents('data', 'raws', 'data', new.data)) do also
  update data.raws set count=count+1,last=now() where md5(data)=md5(new.data);

create trigger insertb_00_check_data
  before insert on data.raws for each row
  when (checking.is_value_presents('data', 'raws', 'data', new.data))
  execute procedure triggers.reject();

create trigger insertb_zz_correct_id
  before insert on data.raws for each row
  when (checking.is_value_presents('data', 'raws', 'id', new.id))
  execute procedure triggers.correct_timestamp_id();
--------------------------------------------------------------------------------
-- /data.raws
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- data.packets
--------------------------------------------------------------------------------
create table data.packets(
  like basic.binary_data including defaults including comments,
  count bigint not null default 0,
  raw_id timestamptz not null,
  type data.types not null,
  last timestamptz not null default current_timestamp,

  constraint zidx_packets_pk primary key(id),
  constraint zidx_packets_fk_raw foreign key(raw_id) references data.raws(id) on delete cascade
);
create unique index zidx_packets_uk_data on data.packets(md5(data));
create index zidx_packets_ik_raw_id on data.packets(raw_id);

create rule add_packet_count as
  on update to data.raws
  where (new.count=old.count+1) do also
  update data.packets set count=count+1,last=now() where raw_id=new.id;

create rule add_count as
  on insert to data.packets
  where (checking.is_value_presents('data', 'packets', 'data', new.data)) do also
  update data.packets set count=count+1,last=now() where md5(data)=md5(new.data);

create trigger insertb_00_check_data
  before insert on data.packets for each row
  when (checking.is_value_presents('data', 'packets', 'data', new.data))
  execute procedure triggers.reject();

create trigger insertb_zz_correct_id
  before insert on data.packets for each row
  when (checking.is_value_presents('data', 'packets', 'id', new.id))
  execute procedure triggers.correct_timestamp_id();
--------------------------------------------------------------------------------
-- /data.packets
--------------------------------------------------------------------------------
