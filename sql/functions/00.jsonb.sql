set plv8.start_proc = '';
create function jsonb.plv8_init() returns bool as $$
  plv8.ll_convert = function(loc, p) {
    var r;
    if (loc == null) return null;
    if (typeof loc != 'object') return null;
    r = loc[p];
    if (typeof loc[p] == 'object') {
      var s = (loc[p]['d'] >= 0) ? 1 : -1;
      r = loc[p]['d'] + s * loc[p]['m']/60;
    }
    return r;
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

create function jsonb.extend(jsonb, jsonb, varchar[] default '{}') returns jsonb as $$
  var a = JSON.parse($1);
  var b = JSON.parse($2);

  return JSON.stringify(plv8.extend(a, b));
$$ language plv8 immutable;

create function jsonb.extend(anyelement, anyelement, varchar[] default '{}') returns jsonb as $$
  select jsonb.extend($1::jsonb, $2::jsonb)
$$ language sql immutable;

create function jsonb.extend(jsonb, anyelement, varchar[] default '{}') returns jsonb as $$
  select jsonb.extend($1, $2::jsonb)
$$ language sql immutable;

create function jsonb.extend(anyelement, jsonb, varchar[] default '{}') returns jsonb as $$
  select jsonb.extend($1::jsonb, $2)
$$ language sql immutable;
