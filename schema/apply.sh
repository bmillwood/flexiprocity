#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
set -x
export PGUSER=${PGUSER:-postgres}
export PGDATABASE=${PGDATABASE:-flexiprocity}
dropdb "$PGDATABASE" || true
createdb "$PGDATABASE"

for role in agent api meddler
do
  echo 'DO $$BEGIN'
  # Oh no, bash substitution into sql strings. Don't pass anything weird.
  cat <<EOF
    IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = '$role')
    THEN CREATE ROLE $role WITH LOGIN;
    END IF;
EOF
  echo 'END$$;'
  echo "GRANT CONNECT ON DATABASE :DBNAME TO $role;"
done | psql -v ON_ERROR_STOP=on

psql -v ON_ERROR_STOP=on \
  -f structure.sql -f ../secrets/seeds.sql
