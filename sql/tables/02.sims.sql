create table sims.data(
  no bigint,
  created timestamptz not null default current_timestamp,
  address inet not null,
  authorized boolean not null default true,

  constraint zidx_data_pk primary key(no)
);

create table sims.sessions(
  id varchar,
  nas_id bigint not null,
  nas_session_id varchar not null,
  nas_port bigint not null,
  sim_id bigint not null,
  started timestamptz not null default current_timestamp,
  updated timestamptz not null default current_timestamp,
  bytes_in bigint not null default 0,
  bytes_out bigint not null default 0,
  packets_in bigint not null default 0,
  packets_out bigint not null default 0,
  terminate_cause varchar,

  constraint zidx_sessions_pk primary key(id),
  constraint zidx_sessions_fk_sim foreign key(sim_id) references sims.data(no) on delete cascade,
  constraint zidx_sessions_fk_nas foreign key(nas_id) references radius.nas(id) on delete cascade
);
create index zidx_sessions_ik_sim on sims.sessions(sim_id);
create index zidx_sessions_ik_sim_tcnull on sims.sessions(sim_id) where (terminate_cause is null);

create trigger insertb_00_close_previous
  before insert on sims.sessions
  for each row
  execute procedure sim.close_session();
