#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
if [ -z "$(docker container ls --all --quiet --filter name='^auth-server-build$')" ]
then
    mkdir -p ubuntu
    docker run --name auth-server-build --mount type=bind,src=$PWD,dst=/mnt haskell:9.4 bash -c 'cd /mnt; cabal update; cabal install --only-dependencies'
fi
if [ -z "$(docker container ls --quiet --filter name='^auth-server-build$')" ]
then
    docker start auth-server-build
fi
docker exec auth-server-build bash -c 'cd /mnt; cabal install --install-method=copy --installdir=/mnt/ubuntu'
