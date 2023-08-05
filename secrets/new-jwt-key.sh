#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail
set -x
mkdir -p jwt
cd jwt
ssh-keygen -t rsa -m PEM -N "" -f private-key.pem
# The public key is generated in OpenSSH format, even though the private key is
# in PEM. There's seemingly no way to prevent ssh-keygen from doing this.
rm private-key.pem.pub
ssh-keygen -e -m PEM -f private-key.pem > public-key.pem
