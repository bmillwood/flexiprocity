#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail

diff -u \
    <(pg_dump -U postgres --schema-only flexiprocity) \
    <(ssh postgres@rpm 'pg_dump --schema-only flexiprocity')
