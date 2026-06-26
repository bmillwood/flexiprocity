FROM postgres:16.14@sha256:287eced1f33b59ed265ed13a60d3680dd7646d70c4dc0e785f59a470ebc03eeb
RUN apt-get update \
 && apt-get install -y --no-install-recommends postgresql-16-pgtap \
 && rm -rf /var/lib/apt/lists/*
