insert into options.data (name, value) values
  ('terminal_auto_add', 'true'),
  ('sim_auto_add', 'true'),
  ('sim_first_address', '10.255.224.0/32'),
  ('primary_dns', '10.255.200.4'),
  ('secondary_dns', '10.255.201.3'),
  ('radius_nas_auto_add', 'true')
  ;

insert into owners.data(id, title, description, parent_id) values
  (null, 'TBL', '', null),
  (null, 'Автодор', '', null),
  (null, 'НКУ', '', null),
  (null, 'ДСУ', '', 2);

insert into terminals.models(id, title, protocols) values
  (null, 'Телтоника', '{"teltonika"}'),
  (null, 'Форт-111', '{"fort111"}'),
  (null, 'Форт-300', '{"fort300"}'),
  (null, 'Геликс-2', '{"gelix2nsk"}'),
  (null, 'АвтоГис', '{"agis"}'),
  (null, 'M2M GLX', '{"m2m"}'),
  (null, 'Форт-300GL', '{"fort300"}');

insert into terminals.data(id, uin, serial_no, model_id, owner_id) values
  (null, 356307040370859, '356307040370859', 1, 1),
  (null, 355915037037637, '355915037037637', 2, 1);

insert into objects.groups(id, title, owner_id, parent_id) values
  (null, 'Блоки на тестировании', 4, null),
  (null, '1', 1, 1),
  (null, '2', 1, 1),
  (null, '3', 1, 2),
  (null, '4', 1, 3),
  (null, '5', 1, 4),
  (null, '6', 1, 5),
  (null, '7', 1, 6),
  (null, '8', 1, 6);

insert into objects.models(id, title) values
  (null, 'Белаз'),
  (null, 'Камаз');
 
insert into objects.data(id, no, terminal_id, model_id, owner_id, group_id)  values
  (null, 'Леха', 1, 1, 1, 1),
  (null, 'Test1', 2, 1, 1, 6);

insert into replica.servers (owner_id, hostname, port, protocols, max_points, max_connections) values
  (1, 'localhost', 31004, '{"m2m"}', 10, 10),
  (1, 'localhost', 31005, '{"gelix2nsk"}', 10, 65535);

insert into replica.include_rules (server_id, terminal_protocol) values
  (1, 'fort111'),
  (1, 'fort300'),
  (1, 'teltonika'),
  (1, 'gelix2nsk'),
  (1, 'm2m'),

  (2, 'fort111'),
  (2, 'fort300'),
  (2, 'teltonika'),
  (2, 'gelix2nsk'),
  (2, 'm2m')
  ;

insert into replica.exclude_rules (server_id, local_port) values
  (1, 31004),
  (2, 31004),
  (1, 31005),
  (2, 31005);
