#!/usr/bin/env bash
set -ux
trap 'pkill --parent $$' EXIT
while sleep 1
do
  inotifywait --quiet -e modify -e delete elm.json src tests &
  elm make --output=elm.js src/Main.elm \
    && elm-test \
    && ./version-info.sh > version.js
  wait %inotifywait
done
