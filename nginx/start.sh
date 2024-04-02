#!/usr/bin/env bash
set -ux
trap 'pkill --parent $$' EXIT

tail --follow=name --retry var/log/{access,error}.log &
while sleep 1
do
    rm -rf var
    mkdir -p var/{log,run}
    source ../secrets/sentry.env
    sed -re "s~SENTRY_URL~$SENTRY_URL~" nginx.conf.template > nginx.conf
    inotifywait --quiet -e modify nginx.conf.template &
    nginx -p "$PWD" -c nginx.conf -e var/log/error.log &
    wait %inotifywait
    kill %nginx
    wait %nginx
done
