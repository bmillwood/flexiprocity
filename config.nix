{ config, lib, pkgs, ... }:
let
  cfg = config.services.flexiprocity;
  authServer = pkgs.callPackage ./auth-server {};
  frontend = pkgs.callPackage ./frontend {
    inherit (cfg) gitRoot flexiprocitySubmodule;
  };
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

      # see frontend for why we need these two
      gitRoot = mkOption {
        type = types.path;
      };
      flexiprocitySubmodule = mkOption {
        type = types.str;
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services = {
      authServer = {
        description = "flexiprocity auth server";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        environment = mkMerge [
          {
            # Currently this dir has to be set up manually, which is a shame
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
