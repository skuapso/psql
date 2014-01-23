create table basic.binary_data(
  id timestamptz not null default current_timestamp,
  data bytea not null
);
