drop cast if exists (timestamptz as bigint);
drop cast if exists (bigint as timestamptz);
create cast (timestamptz as bigint) without function as implicit;
create cast (bigint as timestamptz) without function;
