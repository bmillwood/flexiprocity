FROM postgres:16.15@sha256:f1c3376c26f2609ab9f29f71f824103fe2fcd8ee0346485cb6122a4f93df6f94
RUN apt-get update \
 && apt-get install -y --no-install-recommends postgresql-16-pgtap \
 && rm -rf /var/lib/apt/lists/*
