.PHONY: init drop

init:
	psql -f ./sql/init.sql
	psql -d srw_db -c 'CREATE EXTENSION "uuid-ossp";'

drop:
	psql -f ./sql/drop.sql
