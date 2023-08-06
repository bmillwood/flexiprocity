#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail

# "For development" per https://www.graphile.org/postgraphile/usage-cli/
node_modules/.bin/postgraphile \
    --append-plugins @graphile-contrib/pg-simplify-inflector \
    --watch \
    --dynamic-json \
    --no-setof-functions-contain-nulls \
    --no-ignore-rbac \
    --show-error-stack=json \
    --extended-errors hint,detail,errcode \
    --export-schema-graphql schema.graphql \
    --graphiql "/" \
    --enhance-graphiql \
    --allow-explain \
    --enable-query-batching \
    --legacy-relations omit \
    --owner-connection "socket:/run/postgresql?db=flexiprocity&user=ben" \
    --connection "socket:/run/postgresql?db=flexiprocity&user=api" \
    --jwt-secret "$(cat ../secrets/jwt/public-key.pem)" \
    --jwt-verify-algorithms RS256 \
    --jwt-verify-clock-tolerance 1 \
    --cors \
    --schema public
