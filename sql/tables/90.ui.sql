drop view ui.items_tree;
create view ui.items_tree as(
select array_to_json(array_agg(row_to_json)) as json from
  (select
    row_to_json(owners)
    from (select *,'owner' as "type" from owners.data) owners
  union all select
    row_to_json(groups)
    from (select *,'group' as "type" from groups.tree) groups
  union all select
    row_to_json(objects)
    from (select *,'object' as "type" from objects.data) objects
  union all select
    row_to_json(objects_models)
    from (select *,'object_model' as "type" from objects.models) objects_models
  ) S);
