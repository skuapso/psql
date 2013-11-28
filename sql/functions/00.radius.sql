create function radius.sim_authorization(_no bigint) returns table(
  Attribute varchar,
  Value varchar
) as $$
declare
  s varchar;
begin
  perform sim.get(_no);
  if sim.authorized(_no) then
    return query
      select varchar 'Auth-Type', varchar 'ACCEPT';
    s = option.get('radius_simultaneous_use');
    if s is not null and s::bool then
      return query
        select varchar 'Simultaneous-Use', varchar '1';
    end if;
  else
    return query
      select varchar 'Auth-Type', varchar 'REJECT';
  end if;
end $$ language plpgsql stable;

create function radius.sim_authorization_reply(_no bigint) returns table(
  Attribute varchar,
  Value varchar
) as $$
declare
  a inet;
  s varchar;
begin
  if sim.authorized(_no) then
    a = sim.address(_no);
    if a is not null then
      return query
        select varchar 'Framed-IP-Address', host(a)::varchar
        union all select varchar 'Framed-IP-Netmask', host(netmask(a))::varchar;
    end if;
    s = option.get('primary_dns');
    if s is not null then
      return query
        select varchar 'MS-Primary-DNS-Server', s;
      s = option.get('secondary_dns');
      if s is not null then
        return query
          select varchar 'MS-Secondary-DNS-Server', s;
      end if;
    end if;
    s = option.get('radius_update_interval');
    if s is not null then
      return query
        select varchar 'Acct-Interim-Interval', s;
    end if;
  end if;
end $$ language plpgsql stable;

create function radius.get_nas_id(_identifier varchar, _ip inet, _apn varchar) returns bigint as $$
begin
  return radius.get_nas_id(_identifier, _ip, _apn, option.get('radius_nas_auto_add')::bool);
end $$ language plpgsql;

create function radius.get_nas_id(_identifier varchar, _ip inet, _apn varchar, _add bool) returns bigint as $$
declare
  i bigint;
begin
  select id into i from radius.nas NAS
  inner join radius.nas_ips IP using(id)
  inner join radius.nas_apns APN using(id)
  where NAS.identifier = _identifier and IP.address = _ip and APN.apn = _apn;
  if i is null and _add then
    i = radius.add_nas($1, $2, $3);
  end if;
  return i;
end $$ language plpgsql;

create function radius.add_nas(_identifier varchar, _ip inet, _apn varchar) returns bigint as $$
declare
  i bigint;
begin
  i = radius.add_nas($1);
  if i is not null then
    perform radius.add_nas_ip(i, $2);
    perform radius.add_nas_apn(i, $3);
  end if;
  return i;
end $$ language plpgsql;

create function radius.add_nas(_identifier varchar) returns bigint as $$
declare
  i bigint;
begin
  select id into i from radius.nas where identifier=$1;
  if i is null then
    insert into radius.nas values (null, $1) returning id into i;
  end if;
  return i;
end $$ language plpgsql;

create function radius.add_nas_ip(_id bigint, _ip inet) returns inet as $$
begin
  perform id from radius.nas_ips where id=$1 and address=$2;
  if not found then
    insert into radius.nas_ips values ($1, $2);
  end if;
  return $2;
end $$ language plpgsql;

create function radius.add_nas_apn(_id bigint, _apn varchar) returns varchar as $$
begin
  perform id from radius.nas_apns where id=$1 and apn=$2;
  if not found then
    insert into radius.nas_apns values ($1, $2);
  end if;
  return $2;
end $$ language plpgsql;
