FROM postgres:16@sha256:972eeb4e0a5fee4c3046cf868896719227e845aa9e38ff79a353efb3b2b2c10a
RUN apt-get update \
 && apt-get install -y --no-install-recommends postgresql-16-pgtap \
 && rm -rf /var/lib/apt/lists/*
