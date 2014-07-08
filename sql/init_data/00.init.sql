insert into options.data (name, value) values
  ('terminal_auto_add', 'true'),
  ('sim_auto_add', 'true'),
  ('sim_first_address', '10.255.224.0/32'),
  ('primary_dns', '8.8.8.8'),
  ('secondary_dns', '8.8.4.4'),
  ('radius_nas_auto_add', 'true')
  ;

insert into terminals.models(title, protocols) values
  ('Телтоника', '{"teltonika"}'),
  ('Форт-111', '{"fort111","egts"}'),
  ('Форт-300', '{"fort300"}'),
  ('Геликс-2', '{"gelix2nsk"}'),
  ('АвтоГис', '{"agis"}'),
  ('M2M GLX', '{"m2m"}'),
  ('Wialon', '{"wialon"}'),
  ('Форт-300GL', '{"fort300"}');

insert into objects.types (title) values ('Подвижный объект'), ('Стационарный объект');

insert into sensors.types (port_type, data_type) values
  ('digital', 'bigint')
  ,('analog', 'float')
  ,('counter', 'bigint')
  ,('location', 'geography')
  ;

create role "$user"         nocreatedb nocreaterole nocreateuser nologin noreplication;
create role "$all"          nocreatedb nocreaterole nocreateuser nologin noreplication;
alter table uac.groups disable trigger insertb_00_check_user_exists;
insert into uac.groups values (default, null, '$all');
alter table uac.groups enable trigger insertb_00_check_user_exists;
create role "$manager"      nocreatedb nocreaterole nocreateuser nologin noreplication;
create role "$user_manager" nocreatedb   createrole nocreateuser nologin noreplication;
create role "$engineer"     nocreatedb nocreaterole nocreateuser nologin noreplication;

grant execute on function uac.can_read_group(bigint) to "$user";
grant usage on schema _users      to "$user";
grant usage on schema events      to "$user";
grant usage on schema navigation  to "$user";
grant usage on schema object      to "$user";
grant usage on schema objects     to "$user";
grant usage on schema sensor      to "$user";
grant usage on schema sensors     to "$user";
grant usage on schema terminals   to "$user";
grant usage on schema uac         to "$user";
grant select on _users.data       to "$user";
grant select on _users.groups     to "$user";
grant select on _users.all        to "$user";
grant select on events.data             to "$user";
grant select on events.sensors          to "$user";
grant select on objects.data            to "$user";
grant select on objects.groups          to "$user";
grant select on objects.types           to "$user";
grant select on objects.models          to "$user";
grant select on objects.sensors         to "$user";
grant select on objects.specializations to "$user";
grant select on sensors.data            to "$user";
grant select on sensors.models          to "$user";
grant select on sensors.types           to "$user";
grant select on terminals.data          to "$user";

grant "$user" to "$manager";
grant usage   on schema "group"               to "$manager";
grant usage   on schema triggers              to "$manager";

grant update  on objects.seq__data            to "$manager";
grant all     on objects.data                 to "$manager";
revoke delete on objects._data              from "$manager";

grant update  on objects.seq__groups          to "$manager";
grant all     on objects.groups               to "$manager";
revoke delete on objects._groups            from "$manager";

grant update  on objects.seq__sensors         to "$manager";
grant all     on objects.sensors              to "$manager";
revoke delete on objects._sensors           from "$manager";

grant update  on objects.seq_types            to "$manager";
grant all     on objects.types                to "$manager";
revoke delete on objects.types              from "$manager";

grant update  on objects.seq_models           to "$manager";
grant all     on objects.models               to "$manager";
revoke delete on objects.models             from "$manager";

grant update  on objects.seq_specializations  to "$manager";
revoke delete on objects.specializations    from "$manager";
grant all     on objects.specializations      to "$manager";

grant update  on sensors.seq_ids              to "$manager";
grant all     on sensors._data                 to "$manager";
revoke delete on sensors._data               from "$manager";

grant update  on terminals.seq__data          to "$manager";
grant all     on terminals.data               to "$manager";
revoke delete on terminals._data            from "$manager";
