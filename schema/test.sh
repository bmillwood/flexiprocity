#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
set -x
export PGUSER=${PGUSER:-postgres}
export PGDATABASE=${PGDATABASE:-flexiprocity_test}
dropdb "$PGDATABASE" || true
createdb "$PGDATABASE"
psql -v ON_ERROR_STOP=on -f mock.sql -f structure.sql
psql -v ON_ERROR_STOP=on -f test.sql --tuples-only --quiet --no-align
