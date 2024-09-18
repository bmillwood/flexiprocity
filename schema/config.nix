{ lib, ... }:
{
  services.postgresql = {
    enable = true;
    ensureUsers = [
      { name = "api"; ensureClauses.login = true; }
      { name = "inbox"; ensureClauses.login = true; }
      { name = "meddler"; ensureClauses.login = true; }
    ];
    identMap = ''
      local api api
      local api inbox
      local api meddler
      local postgres postgres
    '';
    authentication = lib.mkForce ''
      local all all peer map=local
    '';
  };
  # I'd normally put --single-transaction on this psql, but you can't create databases
  # inside a transaction
  systemd.services.postgresql.postStart = lib.mkAfter ''
      $PSQL --tuples-only --no-align -v ON_ERROR_STOP=1 <<EOF
        SELECT NOT EXISTS (SELECT 1 FROM pg_catalog.pg_database WHERE datname = 'flexiprocity') AS need_db
        \gset
        \if :need_db
          CREATE DATABASE flexiprocity;
          GRANT CONNECT ON DATABASE flexiprocity TO api, inbox, meddler;
          \c flexiprocity
          \i ${./structure.sql}
        \endif
      EOF
    '';
}
