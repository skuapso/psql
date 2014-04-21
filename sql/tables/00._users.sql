create view _users.data as
select
  rolname as user_name,
  rolconnlimit as max_connections,
  rolcreaterole as can_manage_roles
from
  pg_authid
where
  not rolsuper
  and rolcanlogin
  and (
    rolvaliduntil is null
    or rolvaliduntil>now()
  )
;

create view _users.groups as
select
  rolname as group_name,
  rolconnlimit as max_connections,
  rolcreaterole as can_manage_roles
from
  pg_authid
where
  not rolsuper
  and not rolcanlogin
  and (
    rolvaliduntil is null
    or rolvaliduntil>now()
  )
;

create view _users.all as
select
  rolname as user_name,
  rolconnlimit as max_connections,
  rolcreaterole as can_manage_roles
from
  pg_authid
where
  not rolsuper
  and (
    rolvaliduntil is null
    or rolvaliduntil>now()
  )
;
