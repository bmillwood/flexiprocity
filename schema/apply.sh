#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
set -x
export PGUSER=${PGUSER:-postgres}
export PGDATABASE=${PGDATABASE:-flexiprocity}
dropdb "$PGDATABASE" || true
createdb "$PGDATABASE"
psql -v ON_ERROR_STOP=on \
     -c "GRANT CONNECT ON DATABASE flexiprocity TO api, inbox, meddler" \
     -f structure.sql -f ../secrets/seeds.sql
