#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail

# per https://www.graphile.org/postgraphile/usage-cli/
dev="\
    --show-error-stack=json \
    --watch \
    --extended-errors hint,detail,errcode \
    --owner-connection socket:/run/postgresql?db=flexiprocity&user=postgres \
    --export-schema-graphql schema.graphql \
    --graphiql / \
    --enhance-graphiql \
    --allow-explain \
"

prod="\
    --retry-on-init-fail \
    --extended-errors errcode \
    --disable-query-log \
"

case "$1" in
  dev)
    opts=$dev;;
  prod)
    opts=$prod;;
  *)
    exit 1;;
esac

npm exec \
    --package postgraphile \
    --package graphile-utils \
    --package @graphile/pg-pubsub \
    --package @graphile-contrib/pg-simplify-inflector \
    -- \
    postgraphile \
    --plugins @graphile/pg-pubsub \
    --append-plugins @graphile-contrib/pg-simplify-inflector,"$PWD/subscriptions.js" \
    --subscriptions \
    --dynamic-json \
    --no-setof-functions-contain-nulls \
    --no-ignore-rbac \
    --enable-query-batching \
    --legacy-relations omit \
    --connection "socket:/run/postgresql?db=flexiprocity&user=api" \
    --jwt-secret "$(cat ../secrets/jwt/public-key.pem)" \
    --jwt-verify-algorithms RS256 \
    --jwt-verify-clock-tolerance 1 \
    --cors \
    --schema public \
    $opts
