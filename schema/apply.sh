#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
set -x
dropdb flexiprocity || true
createdb flexiprocity
psql -v ON_ERROR_STOP=on -d flexiprocity -f structure.sql
