#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail

query="subscription S{userUpdate{name picture}}"

websocat \
    --header=Sec-WebSocket-Protocol:graphql-transport-ws \
    --no-close \
    -v \
    ws://localhost:5000/graphql <<EOF
{"type": "connection_init", "payload": {}}
{"id":"$0","type":"subscribe","payload":{"query":"$query","variables":null,"operationName":"S"}}
EOF
