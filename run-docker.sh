#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
rev=$(git rev-parse --short HEAD)
case $1 in
    build)
        docker build . -t flexiprocity:"$rev"
        ;;
    build-auth)
        docker build auth-server -t flexiprocity-auth-server:"$rev"
        ;;
    run)
        docker run --rm -it --publish 57959:57959 \
            --mount type=bind,src=$PWD/secrets,dst=/opt/flexiprocity/secrets,readonly \
            --mount type=bind,src=/run/postgresql,dst=/run/postgresql,readonly \
            flexiprocity:"$rev"
        ;;
    upload)
        repository_url=$(aws ecr describe-repositories --repository-names flexiprocity | jq -r '.repositories|.[].repositoryUri')
        repository=${repository_url%/*}
        aws ecr get-login-password | docker login --username AWS --password-stdin "$repository"
        docker tag flexiprocity:"$rev" "$repository_url:$rev"
        docker push "$repository_url:$rev"
        ;;
esac
