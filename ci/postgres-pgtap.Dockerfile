FROM postgres:16.15@sha256:933463a6c8eba2b9f5cd414138738cd8d5519cd2c471214c222652f547a66a32
RUN apt-get update \
 && apt-get install -y --no-install-recommends postgresql-16-pgtap \
 && rm -rf /var/lib/apt/lists/*
