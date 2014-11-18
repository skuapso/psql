create function sensors.merge(jsonb, jsonb)
returns jsonb
as $$
begin
  if $2 is null then
    return $1;
  end if;
  return jsonb.extend($1, $2);
end $$ language plpgsql immutable;

create aggregate sensors.merge(jsonb) (
  sfunc = jsonb_extend,
  stype = jsonb,
  initcond = '{}'
);
