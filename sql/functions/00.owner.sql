create function owner.json(_owner_id bigint) returns json as $$
declare
  r record;
begin
  select 'owner' as type, id, title, parent_id into r from owners.data where id=$1;
  return row_to_json(r);
end $$ language plpgsql stable;
