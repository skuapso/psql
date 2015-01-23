create function ui.items() returns setof jsonb as $$
begin
  return query
  select row_to_json(groups)::jsonb from (
    select *,'group' as "type" from objects.groups
  ) groups
  union all select row_to_json(terminals_models)::jsonb from (
    select *,'terminal_model' as "type" from terminals.models
  ) terminals_models
  union all select row_to_json(terminals)::jsonb from (
    select *,'terminal' as "type" from terminals.data
    where id in (select terminal_id from objects.data)
  ) terminals
  union all select row_to_json(objects_models)::jsonb from (
    select *,'object_model' as "type" from objects.models
  ) objects_models
  union all select row_to_json(objects_specializations)::jsonb from (
    select *,'specialization' as "type" from objects.specializations
  ) objects_specializations
  union all select row_to_json(sensors)::jsonb from (
    select *,'sensor' as "type" from sensors.data
  ) sensors
  union all select row_to_json(tool)::jsonb from (
    select *,'object_tool' as "type" from objects.tools
  ) tool
  union all select row_to_json(geographies)::jsonb from (
    select *,'geo' as "type" from gis._data order by id
  ) geographies
  union all select row_to_json(objects)::jsonb from (
    select *,'object' as "type" from objects.data
  ) objects
  ;
end $$ language plpgsql stable;
