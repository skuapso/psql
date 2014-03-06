insert into options.data (name, value) values
  ('terminal_auto_add', 'true'),
  ('sim_auto_add', 'true'),
  ('sim_first_address', '10.255.224.0/32'),
  ('primary_dns', '10.255.200.4'),
  ('secondary_dns', '10.255.201.3'),
  ('radius_nas_auto_add', 'true')
  ;

insert into terminals.models(id, title, protocols) values
  (null, 'Телтоника', '{"teltonika"}'),
  (null, 'Форт-111', '{"fort111"}'),
  (null, 'Форт-300', '{"fort300"}'),
  (null, 'Геликс-2', '{"gelix2nsk"}'),
  (null, 'АвтоГис', '{"agis"}'),
  (null, 'M2M GLX', '{"m2m"}'),
  (null, 'Форт-300GL', '{"fort300"}');
