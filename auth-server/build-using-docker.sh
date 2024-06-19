#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail -x
dockername=auth-server-build
baseimage=haskell:9.6
if [ -z "$(docker container ls --all --quiet --filter name="^$dockername"'$')" ]
then
    mkdir -p ubuntu
    docker run --detach --name "$dockername" --mount type=bind,src=$PWD,dst=/mnt "$baseimage" bash -ec 'while true; do sleep 1e9; done'
    docker exec "$dockername" bash -c 'cd /mnt; cabal update; cabal install --only-dependencies'
fi
if [ -z "$(docker container ls --quiet --filter name="^$dockername"'$')" ]
then
    docker start "$dockername"
fi
docker exec "$dockername" bash -c 'cd /mnt; cabal install --install-method=copy --installdir=/mnt/ubuntu'
