#!/usr/bin/env bash
set -ux
trap 'pkill --parent $$' EXIT
while sleep 1
do
  inotifywait --quiet -e modify -e delete elm.json src &
  elm make --output=elm.js src/Main.elm
  wait %inotifywait
done
