FROM alpine:3 AS auth-server-build
# need curl for cabal update
RUN apk add g++ ghc cabal curl musl-dev libffi-dev zlib-dev
WORKDIR /opt/auth-server
RUN cabal update
COPY auth-server/auth-server.cabal ./auth-server.cabal
RUN cabal build --only-dependencies -j4
COPY auth-server/LICENSE .
COPY auth-server/CHANGELOG.md .
COPY auth-server/app app
COPY auth-server/src src
RUN mkdir ./bin
RUN cabal install --install-method=copy --installdir=/opt/auth-server/bin

FROM node:alpine AS frontend-build
RUN apk add curl
WORKDIR /opt/frontend
RUN curl -L https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz | gunzip > /usr/local/bin/elm
RUN chmod +x /usr/local/bin/elm
RUN npm install -g uglify-js
COPY frontend/elm.json .
COPY frontend/src src
RUN elm make --optimize --output=elm.premin.js src/Main.elm
RUN uglifyjs elm.premin.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output elm.js

FROM alpine:3 AS postgraphile-build
RUN apk add npm
WORKDIR /opt/postgraphile
RUN npm install -g pnpm
COPY postgraphile/package.json postgraphile/pnpm-lock.yaml ./
RUN pnpm install

FROM alpine:3
RUN apk add gmp libffi libstdc++ nginx nodejs tmux
WORKDIR /opt/flexiprocity
COPY --from=auth-server-build /opt/auth-server/bin auth-server
COPY --from=frontend-build /opt/frontend/elm.js frontend/elm.js
COPY frontend/index.html frontend/driver.js frontend/*.css frontend/
COPY nginx/nginx.conf nginx/
RUN mkdir -p nginx/var/log nginx/var/run
COPY --from=postgraphile-build /opt/postgraphile/node_modules postgraphile/node_modules
COPY postgraphile/postgraphile.tags.json5 postgraphile/start.sh postgraphile/
COPY docker-entrypoint.sh .
CMD ["sh", "docker-entrypoint.sh"]