{ config, lib, pkgs, ... }:
let
  cfg = config.services.flexiprocity;
  inherit (lib) mkIf mkMerge mkOption types;
in
{
  options = {
    services.flexiprocity = {
      backupTo = lib.mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          Passed to the date command as a format string.

          Will be created by the backup script if it doesn't exist.
        '';
        example = "/mnt/backup/pg_dump/%Y/%m-%dT%T.sql";
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.postgresql = {
        enable = true;
        ensureUsers = [
          { name = "agent"; ensureClauses.login = true; }
          { name = "api"; ensureClauses.login = true; }
          { name = "inbox"; ensureClauses.login = true; }
          { name = "meddler"; ensureClauses.login = true; }
        ];
        identMap = ''
          local api agent
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
            GRANT CONNECT ON DATABASE flexiprocity TO agent, api, inbox, meddler;
            \c flexiprocity
            \i ${./structure.sql}
          \endif
        EOF
      '';
    }
    (mkIf (cfg.backupTo != null) {
      systemd = {
        services."backup-flexiprocity" = {
          path = with pkgs; [
            coreutils
            postgresql
            sudo
          ];
          script = ''
            target=$(date +"${cfg.backupTo}")
            mkdir -p "$(dirname "$target")"
            sudo -u postgres -i pg_dump --column-inserts -d flexiprocity > "$target"
          '';
          serviceConfig = {
            Type = "oneshot";
            User = "root";
          };
        };
        timers."backup-flexiprocity" = {
          wantedBy = [ "timers.target" ];
          timerConfig = {
            OnCalendar = "daily";
            Persistent = true;
            Unit = "backup-flexiprocity.service";
          };
        };
      };
    })
  ]);
}
