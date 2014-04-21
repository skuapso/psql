create function _user.exists(name text, can_login boolean = true) returns boolean as $$
begin
  if $2 is null then
    perform 1 from _users.all where user_name=$1;
  elsif $2 then
    perform 1 from _users.data where user_name=$1;
  else
    perform 1 from _users.groups where group_name=$1;
  end if;
  return found;
end $$ language plpgsql stable;
