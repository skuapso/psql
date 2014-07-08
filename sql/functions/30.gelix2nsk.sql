alter type terminals.protocols add value 'gelix2nsk';
create type gelix2nsk.sensors as enum('last_valid', 'restart_hw_fail', 'coldstart', 'zone_alarm');
