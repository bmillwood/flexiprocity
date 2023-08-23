#!/bin/sh
tmux new-session \; \
     split-window -h \; \
     send-keys "source secrets/facebook.env" Enter \
        "cd auth-server" Enter "./auth-server" Enter \; \
     split-window \; \
     send-keys "cd nginx" Enter "nginx -p /opt/flexiprocity/nginx -c nginx.conf -e var/log/error.log" Enter \; \
     split-window \; \
     send-keys "cd postgraphile" Enter "sh start.sh prod" Enter \; \
     select-layout main-vertical \; \
     select-pane -t 0
