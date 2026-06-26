FROM postgres:18.4@sha256:1a5b3e745bbd82d6deb146505e504da3c2f248cac15e431951b148fbe4f8613a
RUN apt-get update \
 && apt-get install -y --no-install-recommends postgresql-16-pgtap \
 && rm -rf /var/lib/apt/lists/*
