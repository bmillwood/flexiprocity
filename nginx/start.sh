#!/usr/bin/env bash
set -ux
trap 'pkill --parent $$' EXIT

tail --follow=name --retry var/log/{access,error}.log &
while sleep 1
do
    rm -rf var
    mkdir -p var/{log,run}
    inotifywait --quiet -e modify nginx.conf &
    nginx -p "$PWD" -c nginx.conf -e var/log/error.log &
    wait %inotifywait
    kill %nginx
    wait %nginx
done
