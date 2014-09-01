create function ui.items() returns setof jsonb as $$
begin
  return query
  select row_to_json(groups)::jsonb from (
    select * from objects.groups
  ) groups
  union all select row_to_json(terminals)::jsonb from(
    select * from terminals.data
    where id in (select terminal_id from objects.data)
  ) terminals
  union all select row_to_json(objects_models)::jsonb from (
    select *,'object_model' as "type" from objects.models
  ) objects_models
  union all select row_to_json(objects_specializations)::jsonb from (
    select *,'specialization' as "type" from objects.specializations
  ) objects_specializations
  union all select row_to_json(objects)::jsonb from (
    select * from objects.data
  ) objects;
end $$ language plpgsql stable;
