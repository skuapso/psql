grant "$all" to skuapso;
grant "$all" to t;
grant "$manager" to t;

set role t;

insert into objects.groups(title, parent_id)
select
  unnest(
    array[
      'Тестирование'
  ]),
  unnest(
    array[
      null
  ]::bigint[]);

insert into objects.models(title)
select unnest(
        array[
          'Камаз'
        ]);

insert into objects.data(no, model_id, group_id, terminal_id)
select unnest(
        array[
          '768'
      ]),
      unnest(
        array[
          1
      ]),
      unnest(
        array[
          1
      ]),
      unnest(
        array[
          terminal.add(356495042464794, 'fort111')
      ]);

reset role;

create view data.rate as
select
  *
from (
  select
    count(*)/extract('epoch' from (max(id::timestamptz) - min(id::timestamptz))) as raws
  from data.raws
) R
join (
  select
    count(*)/extract('epoch' from (max(id::timestamptz) - min(id::timestamptz))) as packets
  from data.packets
) P on true
join (
  select
    count(*)/extract('epoch' from (max(id::timestamptz) - min(id::timestamptz))) as events
  from events.data
) E on true
;
