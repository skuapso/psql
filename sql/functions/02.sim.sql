create function sim.add(_no bigint) returns bigint as $$
declare
  i bigint;
  a inet;
begin
  select address into a from sims.data order by address desc limit 1;
  if a is null then
    a = "option".get('sim_first_address')::inet;
  end if;
  insert into sims.data (no, address) values (_no, a + 1) returning no into i;
  return i;
end $$ language plpgsql volatile;

create function sim.get(_no bigint) returns bigint as $$
declare
  i boolean;
begin
  i = "option".get('sim_auto_add')::boolean;
  return sim.get(_no, i);
end $$ language plpgsql stable;

create function sim.get(_no bigint, _add boolean) returns bigint as $$
declare
  i bigint;
begin
  select no into i from sims.data where no=_no;
  if i is null and _add then
    i = sim.add(_no);
  end if;
  return i;
end $$ language plpgsql stable;

create function sim.authorized(_id bigint) returns boolean as $$
declare
  a boolean;
begin
  select authorized into a from sims.data where no=_id;
  if not found then
    a = false;
  end if;
  return a;
end $$ language plpgsql;

create function sim.address(_id bigint) returns inet as $$
declare
  a inet;
begin
  select address into a from sims.data where no=_id;
  return a;
end $$ language plpgsql;

create function sim.close_session() returns trigger as $$
begin
  update sims.sessions set updated=now(),terminate_cause='Unknown' where sim_id=new.sim_id and terminate_cause is null;
  return new;
end $$ language plpgsql;
