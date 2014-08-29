create sequence owners.seq_data;

create table owners.data(
  id bigint
    default nextval('owners.seq_data')
    constraint "owners(id)"
    primary key,

  title varchar
    not null
);
