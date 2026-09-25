FROM postgres:16.15@sha256:1a6ab3f5345eb6dbe04a1349529caabdb0ab09293a09590fad07b2246bfa4b54
RUN apt-get update \
 && apt-get install -y --no-install-recommends postgresql-16-pgtap \
 && rm -rf /var/lib/apt/lists/*
