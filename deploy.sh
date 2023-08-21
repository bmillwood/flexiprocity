#!/usr/bin/env bash
set -o errexit -o nounset -o pipefail

tmux new-session \; \
     split-window -h \; \
     send-keys "cd auth-server" Enter "./build.sh" Enter \; \
     split-window \; \
     send-keys "cd nginx" Enter "./start.sh" Enter \; \
     split-window \; \
     send-keys "cd postgraphile" Enter "./start.sh prod" Enter \; \
     split-window \; \
     send-keys "cd frontend" Enter "./build.sh" Enter \; \
     select-layout main-vertical \; \
     select-pane -t 0
