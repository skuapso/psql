set client_min_messages to 'notice';
do $$begin raise notice 'insert to options.data as %', current_user; end$$;
insert into options.data (name, value) values
  ('terminal_auto_add', 'true'),
  ('sim_auto_add', 'true'),
  ('sim_first_address', '10.0.0.0/32'),
  ('primary_dns', '8.8.8.8'),
  ('secondary_dns', '8.8.4.4'),
  ('radius_nas_auto_add', 'true')
  ;

do $$begin raise notice 'insert to terminals.models as %', current_user; end$$;
insert into terminals.models(title, protocols) values
  ('Геликс-2', '{"gelix2nsk"}'),
  ('Форт-300', '{"fort300"}'),
  ('Форт-111', '{"fort111","egts"}'),
  ('M2M GLX', '{"m2m"}'),
  ('Телтоника', '{"teltonika"}'),
  ('АвтоГис', '{"agis"}'),
  ('Wialon', '{"wialon"}')
  ;

do $$begin raise notice 'insert to terminals.ports as %', current_user; end$$;
insert into terminals.ports(model_id, ports) values
  (2, '{"digital": {"in": [1, 2, 3, 4, 5, 6]}}')
  ;

do $$begin raise warning 'terminals ports not filled yet'; end$$;

do $$begin raise notice 'insert to sensors.types as %', current_user; end$$;
insert into sensors.types (title) values
  ('Местоположение')
  ,('Цифровой датчик')
  ,('Аналоговый датчик')
  ;

do $$begin raise notice 'insert to objects.tools as %', current_user; end$$;
insert into objects.tools(title, type_id) values
  ('Местоположение', -1)
  ,('Скорость', -3)
  ,('Захваченые спутники', -2)
  ,('Видимые спутники', -2)
  ,('Внешнее питание', -3)
  ,('Напряжение батареи', -3)
  ,('Уровень GSM сиганала', -2)
  ,('Зажигание', -2)
  ;

do $$begin raise notice 'insert to terminals.ports with provides as %', current_user; end$$;
insert into terminals.ports(model_id, ports, provides)
  select id,column1,column2
  from terminals.models
  inner join
    (
      values
        ('"location"'::jsonb, 1)
        ,('"speed"', 2)
        ,('"used"', 3)
    ) S2
    on true;

do $$begin raise notice 'insert to sensors.type_ports as %', current_user; end$$;
insert into sensors.type_ports values
  (-1, 'location', 'sensor.compute_location')
  ,(-2, 'digital', null)
  ,(-3, 'speed', null)
  ,(-3, 'analog', null)
  ;

do $$begin raise notice 'setting roles as %', current_user; end$$;
drop role "$all";
drop role "$engineer";
drop role "$user_manager";
drop role "$manager";
drop role "$user";
drop role "$radius";
drop role "$writer";

create role "$all"          nocreatedb nocreaterole nocreateuser nologin noreplication;
create role "$engineer"     nocreatedb nocreaterole nocreateuser nologin noreplication;
create role "$user_manager" nocreatedb   createrole nocreateuser nologin noreplication;
create role "$manager"      nocreatedb nocreaterole nocreateuser nologin noreplication;
create role "$user"         nocreatedb nocreaterole nocreateuser nologin noreplication;
create role "$radius"       nocreatedb nocreaterole nocreateuser nologin noreplication;
create role "$writer"       nocreatedb nocreaterole nocreateuser nologin noreplication;
create role "osm"           nocreatedb nocreaterole nocreateuser   login noreplication;

create schema if not exists authorization osm;

alter table uac.groups disable trigger insertb_00_check_user_exists;
insert into uac.groups values (default, null, '$all');
alter table uac.groups enable trigger insertb_00_check_user_exists;

grant execute on function uac.can_read_group(bigint) to "$user";
grant usage on schema _users      to "$user";
grant usage on schema events      to "$user";
grant usage on schema navigation  to "$user";
grant usage on schema object      to "$user";
grant usage on schema objects     to "$user";
grant usage on schema sensor      to "$user";
grant usage on schema sensors     to "$user";
grant usage on schema terminal    to "$user";
grant usage on schema terminals   to "$user";
grant usage on schema uac         to "$user";
grant usage on schema jsonb       to "$user";
grant usage on schema gis         to "$user";
grant select on _users.data       to "$user";
grant select on _users.groups     to "$user";
grant select on _users.all        to "$user";
grant select on events.data             to "$user";
grant select on objects.data            to "$user";
grant select on objects.groups          to "$user";
grant select on objects.models          to "$user";
grant select on objects.sensors         to "$user";
grant select on objects.specializations to "$user";
grant select on objects.tools           to "$user";
grant select on sensors.data            to "$user";
grant select on sensors.models          to "$user";
grant select on sensors.types           to "$user";
grant select on sensors.type_ports      to "$user";
grant select on terminals.data          to "$user";
grant select on terminals.models        to "$user";
grant select on terminals.ports         to "$user";
grant select on gis._data               to "$user";

grant "$user" to "$manager";
grant usage   on schema "group"               to "$manager";
grant usage   on schema triggers              to "$manager";

grant update  on objects.seq__data            to "$manager";
grant all     on objects.data                 to "$manager";

grant update  on objects.seq__groups          to "$manager";
grant all     on objects.groups               to "$manager";
revoke delete on objects._groups            from "$manager";

grant all     on objects.sensors              to "$manager";
revoke delete on objects._sensors           from "$manager";

grant update  on objects.seq_models           to "$manager";
grant all     on objects.models               to "$manager";
revoke delete on objects.models             from "$manager";

grant update  on objects.seq_specializations  to "$manager";
revoke delete on objects.specializations    from "$manager";
grant all     on objects.specializations      to "$manager";

grant update  on objects.seq_tools            to "$manager";
grant all     on objects.tools                to "$manager";
revoke delete on objects.tools              from "$manager";

grant update  on sensors.seq_ids              to "$manager";
grant all     on sensors._data                to "$manager";
revoke delete on sensors._data              from "$manager";

grant update  on terminals.seq__data          to "$manager";
grant all     on terminals.data               to "$manager";

do $$begin raise warning 'i think it should be done differently'; end$$;
-- grant select on options.data to public?
-- may be alter table options.data add column "module" and create view for each module?
grant usage on schema option to "$writer";
grant usage on schema options to "$writer";
grant select on options.data to "$writer";

grant "$writer" to "$radius";
grant usage   on schema radius          to "$radius";
grant all     on radius.nas             to "$radius";
grant all     on radius.nas_apns        to "$radius";
grant all     on radius.nas_ips         to "$radius";
grant update  on radius.seq_nas         to "$radius";

grant usage   on schema sim             to "$radius";
grant usage   on schema sims            to "$radius";
grant all     on sims.data              to "$radius";
grant all     on sims.sessions          to "$radius";

grant usage on schema ui to "$user";
