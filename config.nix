{ config, lib, pkgs, ... }:
let
  cfg = config.services.flexiprocity;
  agent = pkgs.callPackage ./agent {};
  authServer = pkgs.callPackage ./auth-server {};
  frontend = pkgs.callPackage ./frontend {};
  inherit (lib) mkIf mkMerge mkOption types;
in
{
  imports = [
    ./schema/config.nix
    ./postgraphile/config.nix
  ];

  options = {
    services.flexiprocity = {
      enable = lib.mkEnableOption "flexiprocity";

      virtualHost = mkOption {
        type = types.str;
      };

      sentry.dsn = mkOption {
        type = types.nullOr types.str;
        default = null;
      };
      sentry.loaderUrl = mkOption {
        type = types.nullOr types.str;
        default = null;
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services = {
      flexiprocity-jwt-keys = {
        description = "Generate JWT signing keypair for flexiprocity";
        wantedBy = [ "multi-user.target" ];
        unitConfig.ConditionPathExists = "!/home/api/secrets/jwt/private-key.pem";
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          User = "api";
        };
        path = [ pkgs.openssl ];
        script = ''
          mkdir -p /home/api/secrets/jwt
          cd /home/api/secrets/jwt
          openssl genrsa -out private-key.pem 3072
          openssl rsa -in private-key.pem -pubout -out public-key.pem
        '';
      };
      flexiprocity-agent = {
        description = "flexiprocity agent process";
        requires = [ "postgresql.target" ];
        after = [ "postgresql.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          User = "api";
          ExecStart = "${agent}/bin/flexiprocity-agent";
        };
      };
      flexiprocity-auth-server = {
        description = "flexiprocity auth server";
        requires = [ "flexiprocity-jwt-keys.service" ];
        after = [ "network.target" "flexiprocity-jwt-keys.service" ];
        wantedBy = [ "multi-user.target" ];
        environment = mkMerge [
          {
            # JWT keys under jwt/ are auto-generated; other files in
            # this dir (Google client secret, Friendica instances list,
            # etc.) are still set up manually.
            SECRETS_DIR = "/home/api/secrets";
          }
          (mkIf (cfg.sentry.dsn != null) {
            SENTRY_DSN = cfg.sentry.dsn;
          })
        ];
        serviceConfig = {
          User = "api";
          ExecStart = "${authServer}/bin/auth-server";
        };
      };
    };
    users.users.api = {
      isNormalUser = true;
    };
    services.nginx = {
      enable = true;
      virtualHosts = {
        ${cfg.virtualHost} = {
          forceSSL = true;
          enableACME = true;
          locations = mkMerge [
            {
              "/auth/" = {
                proxyPass = "http://127.0.0.1:5001/";
                recommendedProxySettings = true;
              };
              "/" = {
                root = "${frontend}";
                tryFiles = "$uri /index.html =404";
              };
            }
            (mkIf (cfg.sentry.loaderUrl != null) {
              "/sentry.js" = {
                return = "301 ${cfg.sentry.loaderUrl}";
              };
            })
          ];
        };
      };
    };
  };
}
