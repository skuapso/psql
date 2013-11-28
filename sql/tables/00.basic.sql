create table basic.binary_data(
  id bigint,
  dbtime timestamp with time zone not null default now(),
  data bytea not null
);

create table basic.location(
  id bigint,
  dbtime timestamp with time zone not null default now(),
  latitude float not null,
  longitude float not null,
  altitude float not null
);

create table basic.route_point(
  like basic.location including defaults including comments,
  eventtime timestamp with time zone not null,
  speed float not null,
  course float not null
);
