create type data.types as enum(
  'unknown',
  'broken',
  'ping',
  'low',
  'offline',
  'online',
  'high',
  'panic',
  'security',
  'authentication'
);

create type connections.types as enum(
  'ip'
);
