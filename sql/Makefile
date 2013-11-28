TABLES		:= $(wildcard tables/*.sql)
FUNCTIONS	:= $(wildcard functions/*.sql)
TRIGGERS	:= $(wildcard triggers/*.sql)
INITDATA	:= $(wildcard init_data/*.sql)

.PHONY: help install uninstall reinstall update
.PHONY: install_schemas install_tables install_functions install_triggers install_init_data
.PHONY: db backup update copy_% sha grant_%
.PHONY: current
.PHONY:	visual

help:
	@echo
	@echo 'This utility is for managing database'
	@echo
	@echo 'Usage:'
	@echo "\tmake <action>"
	@echo
	@echo "Actions:"
	@echo "\thelp\t\t- this help"
	@echo "\tshow_config\t- show current configuration"
	@echo "\tinstall\t\t- installs the database"
	@echo "\tuninstall\t- uninstalls the database"
	@echo "\treinstall\t- reinstalls the database"
	@echo "\tinstall_schemas\t- installs schemas"
	@echo "\tinstall_functions\t- installs functions"
	@echo "\tinstall_tables\t- installs tables"
	@echo "\tupdate\t\t- updates database"
	@echo

include Makefile.config

DBAUTH	:= -U $(DBUSER) -h $(DBHOST) -p $(DBPORT) $(DB)
PSQL	:= /usr/bin/psql $(DBAUTH)
PGDUMPC := /usr/bin/pg_dump -F c -v -x $(DBAUTH)
PGDUMPP	:= /usr/bin/pg_dump -F p -O -s -x $(DBAUTH)
DBSHA	:= $(PGDUMPP) | egrep -v '^--' | egrep -v '^$$' | sha1sum -t | cut -f1 -d' '

install: install_schemas install_functions install_tables install_triggers install_init_data
	

install_schemas:
	@for i in $(FUNCTIONS); do echo "create schema \"`echo $$i | sed 's/^functions\/[0-9][0-9]\.//' | sed 's/\.sql$$//'`\"" | $(PSQL); done

install_functions:
	@echo "\t*** Installing functions"
	@for i in $(FUNCTIONS); do echo "\t\t*** installing $$i"; $(PSQL) < $$i; done

install_tables:
	@echo "\t*** Installing tables"
	@for i in $(TABLES); do echo "\t\t*** installing $$i"; $(PSQL) < $$i; done

install_triggers:
	@echo "\t*** Installing triggers"
	@for i in $(TRIGGERS); do echo "\t\t*** installing $$i"; $(PSQL) < $$i; done

install_init_data:
	@echo "\t*** Initalizing database with data"
	@for i in $(INITDATA); do echo "\t\t*** installing $$i"; $(PSQL) < $$i; done

uninstall:
	@for i in $(FUNCTIONS); do echo "drop schema \"`echo \"$$i\" | sed 's/^functions\/[0-9][0-9]\.//' | sed 's/\.sql$$//'`\" cascade" | $(PSQL); done

reinstall: uninstall install

db:
	$(PSQL)

backup:
	$(PGDUMPC) > backup.sql

copy:
	@echo "usage: make copy_{db_name}"

copy_%:
	/usr/bin/dropdb $(DBAUTH)
	/usr/bin/createdb $(DBAUTH)
	/usr/bin/pg_dump -F p -x -O -v -U $(DBUSER) $* | $(PSQL)

sha:
	@echo `$(DBSHA)`

pf:
	@vim updates/`date +%F`-`$(DBSHA)`.sql

update:
	@updatefile="updates/*-`$(DBSHA)`.sql"; \
	while [ -f $$updatefile ]; do \
		echo processing $$updatefile; \
		for i in `ls -1 $$updatefile`; do \
			$(PSQL) < $$i; \
		done; \
		updatefile="updates/*-`$(DBSHA)`.sql"; \
	done;

grant_%:
	@echo $*
	@for i in ` \
		echo "select nspname from pg_catalog.pg_namespace where nspowner<>10" \
		| $(PSQL) \
		| head -n-2 \
		| tail -n+3`; do \
	  echo "grant usage on schema $$i to \"$*\"" | $(PSQL); \
	  echo "grant select on all tables in schema $$i to \"$*\"" | $(PSQL); \
	  echo "grant select on all sequences in schema $$i to \"$*\"" | $(PSQL); \
	 done;

current:
	@rm -rf current
	@mkdir -p current
	@for i in ` \
		echo "select nspname from pg_catalog.pg_namespace where nspowner<>10" \
		| $(PSQL) \
		| head -n-2 \
		| tail -n+3`; do \
	  echo "dumping $$i"; \
	  $(PGDUMPP) -n $$i > current/$$i.sql; \
	done;

test:
	echo "\""

visual:
	@./postgresql_autodoc --index-functions md5 \
	  -l ./ -t dot \
	  -d $(DB) -u $(DBUSER) -p $(DBPORT) -h $(DBHOST)
	@dot -Tpdf -o $(DB).pdf $(DB).dot
	@rm $(DB).dot
	@evince $(DB).pdf
