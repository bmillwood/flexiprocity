#!/usr/bin/env bash
set -ux
trap 'pkill --parent $$' EXIT

killGrandchild() {
  pkill --parent $(pgrep --parent $1)
}

while sleep 1
do
  inotifywait --quiet -e modify -e delete app src *.cabal &
  if cabal build "$@"
  then
    cabal run flexiprocity-agent &
    cabal test
    wait %inotifywait
    jobs -x killGrandchild %?run
    wait %?run
  else
    wait %inotifywait
  fi
done
