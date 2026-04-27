#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
set -x
mkdir -p jwt
cd jwt
openssl genrsa -out private-key.pem 3072
openssl rsa -in private-key.pem -pubout -out public-key.pem
