--------------------------------------------------------------------------------
-- задает следующий по порядку id
--------------------------------------------------------------------------------
create or replace function triggers.set_id ()
                    returns trigger as $$
begin
  if tg_argv[0] is null then
    new.id = nextval (tg_table_schema || '.' || 'seq_' || tg_table_name);
  else
    new.id = nextval (tg_argv[0]);
  end if;
  return new;
end $$ language plpgsql;
comment on function triggers.set_id ()
  is 'задает следующий по порядку id';
--------------------------------------------------------------------------------
-- функция системное сообщение
--------------------------------------------------------------------------------
create or replace function triggers.system_notify ()
                    returns trigger as $$
begin
  raise warning 'system % %', tg_argv[0], tg_argv[1];
  perform pg_notify ('system', tg_argv[0] || ' ' || tg_argv[1]);
  return new;
end $$ language plpgsql;
--------------------------------------------------------------------------------
-- функция генерирует сообщение с указанием ошибочного поля
--------------------------------------------------------------------------------
create or replace function triggers.notify ()
                    returns trigger as $$
begin
  raise notice 'async notify trigger % % on %.%',
    tg_argv[0], tg_argv[1], tg_table_schema, tg_table_name;
  perform system.notify (tg_argv[0], tg_argv[1]);
  if lower (tg_when) = 'before' then
    new.id = null;
  end if;
  return new;
end $$ language plpgsql;
comment on function triggers.notify ()
  is 'генерирует сообщение с указанием ошибочного поля';
--------------------------------------------------------------------------------
-- отклоняет добавление записи
--------------------------------------------------------------------------------
create or replace function triggers.reject ()
                    returns trigger as $$
begin
  return null;
end $$ language plpgsql;
comment on function triggers.reject ()
  is 'отколняет добавление записи';
--------------------------------------------------------------------------------
-- устанавливает id терминала по uin терминала и протоколу
--------------------------------------------------------------------------------
create function triggers.set_terminal_by_uin() returns trigger as $$
begin
  new.terminal_id = terminal.get(new.terminal_id, tg_table_schema);
  return new;
end $$ language plpgsql;
--------------------------------------------------------------------------------
-- корректирует timestamp id
--------------------------------------------------------------------------------
create function triggers.correct_timestamp_id() returns trigger as $$
declare
  c timestamptz;
  t timestamptz;
  i bigint;
begin
  execute 'select id from ' || tg_table_schema || '.' || tg_table_name ||
      E' where id=(timestamptz \'' || new.id || E'\' - interval \'0.000001\') limit 1' into c;
  if c is null then
    new.id = new.id - interval '0.000001';
  else
    i = 1;
    c = null;
    loop
      t = new.id + ('0.00000' || i)::interval;
      execute 'select id from ' || tg_table_schema || '.' || tg_table_name ||
          E' where id=\'' || t || E'\' limit 1' into c;
      if c is null then
        new.id = t;
        exit;
      end if;
      i = i + 1;
      c = null;
    end loop;
  end if;
  return new;
end $$ language plpgsql;
--------------------------------------------------------------------------------
-- печатает new
--------------------------------------------------------------------------------
create function triggers.print_new() returns trigger as $$
begin
  raise warning 'new is %', new;
  return new;
end $$ language plpgsql;
