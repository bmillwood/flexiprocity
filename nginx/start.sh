#!/usr/bin/env bash
set -ux
trap 'pkill --parent $$' EXIT

tail --follow=name --retry var/log/{access,error}.log &
while sleep 1
do
    rm -rf var
    mkdir -p var/{log,run}
    source ../secrets/sentry.env
    sed -r \
        -e "s~SENTRY_URL~$SENTRY_URL~" \
        -e "s~SSL_DIR~../secrets/ssl~" \
        -e "s~FRONTEND_DIR~../frontend~" \
        flexiprocity.conf.template > flexiprocity.conf
    inotifywait --quiet -e modify flexiprocity.conf.template standalone.conf &
    nginx -p "$PWD" -c standalone.conf -e var/log/error.log &
    wait %inotifywait
    kill %nginx
    wait %nginx
done
