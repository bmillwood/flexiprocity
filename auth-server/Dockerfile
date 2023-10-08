FROM alpine:3 AS build
# need curl for cabal update
RUN apk add --no-cache g++ ghc cabal curl musl-dev libffi-dev zlib-dev
WORKDIR /opt/auth-server
RUN cabal update
COPY auth-server.cabal ./auth-server.cabal
RUN cabal build --only-dependencies -j4
COPY LICENSE .
COPY CHANGELOG.md .
COPY app app
COPY src src
RUN mkdir ./bin
RUN cabal install --install-method=copy --installdir=/opt/auth-server/bin

FROM alpine:3
RUN apk add --no-cache gmp libffi libstdc++
WORKDIR /opt/flexiprocity/auth-server
COPY --from=build /opt/auth-server/bin .
CMD ["./auth-server"]