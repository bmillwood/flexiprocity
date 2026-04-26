#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
set -x
export PGUSER=${PGUSER:-postgres}
export PGDATABASE=${PGDATABASE:-flexiprocity}
dropdb "$PGDATABASE" || true
createdb "$PGDATABASE"

./create-users.sh

psql -v ON_ERROR_STOP=on -f structure.sql

for extra in ../secrets/seeds.sql
do
  [ -r "$extra" ] || continue
  psql -v ON_ERROR_STOP=on -f "$extra"
done
