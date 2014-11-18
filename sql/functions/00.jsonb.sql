set plv8.start_proc = '';
create function jsonb.plv8_init() returns bool as $$
  plv8.ll_convert = function(ll) {
    var s;
    if (ll == null) return null;
    if (typeof ll == 'object') {
      var s = (ll['d'] >= 0) ? 1 : -1;
      return ll['d'] + s * ll['m']/60;
    } else {
      return ll;
    }
  };
  plv8.extend = function(a, b, e) {
    var i;
    var exclude = e || [];
    if (a == null) return b;
    if ((typeof a == 'object') && (typeof b == 'object')) {
      for (i in b) {
        if (exclude.indexOf(i) > -1) continue;
        if (typeof b[i] == 'object') {
          a[i] = plv8.extend(a[i], b[i], e);
        } else if (b[i] != null) {
          a[i] = b[i];
        }
      }
    } else {
      a = b;
    }
    return a;
  };
$$ language plv8;

create function jsonb.extend(jsonb, jsonb, varchar[]) returns jsonb as $$
  select jsonb_extend($1, $2);
$$ language sql immutable strict;

create function jsonb.extend(anyelement, anyelement, varchar[] default '{}') returns jsonb as $$
  select jsonb.extend($1::jsonb, $2::jsonb, $3)
$$ language sql immutable strict;

create function jsonb.extend(jsonb, anyelement, varchar[] default '{}') returns jsonb as $$
  select jsonb.extend($1, $2::jsonb, $3)
$$ language sql immutable strict;

create function jsonb.extend(anyelement, jsonb, varchar[] default '{}') returns jsonb as $$
  select jsonb.extend($1::jsonb, $2, $3)
$$ language sql immutable strict;

create function jsonb.extend(jsonb, jsonb) returns jsonb as $$
  select jsonb.extend($1, $2, '{}'::varchar[]);
$$ language sql immutable strict;

create function jsonb.path(_path jsonb, text[] default '{}') returns text[] as $$
declare
  k text;
  v jsonb;
begin
  k = jsonb_typeof($1);
  if k = 'object' then
    select key,value into k,v from jsonb_each($1) limit 1;
    return jsonb.path(v, $2 || k);
  else
    return $2 || ($1->>0);
  end if;
end $$ language plpgsql immutable strict;
