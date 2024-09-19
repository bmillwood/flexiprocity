{ config, lib, pkgs, ... }:
let
  cfg = config.services.flexiprocity;
  authServer = pkgs.callPackage ./auth-server {};
  frontend = pkgs.callPackage ./frontend {
    inherit (cfg) gitRoot flexiprocitySubmodule;
  };
  inherit (lib) mkIf mkOption types;
in
{
  imports = [
    ./schema/config.nix
  ];

  options = {
    services.flexiprocity = {
      enable = lib.mkEnableOption "flexiprocity";

      # see frontend for why we need these two
      gitRoot = lib.mkOption {
        type = types.path;
      };
      flexiprocitySubmodule = lib.mkOption {
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
        serviceConfig = {
          User = "api";
          ExecStart = "${authServer}/bin/auth-server";
          # Currently this dir has to be set up manually, which is a shame
          Environment = "SECRETS_DIR=/home/api/secrets";
        };
      };
      postgraphile = {
        description = "flexiprocity postgraphile";
        after = [ "postgresql.service" ];
        wantedBy = [ "multi-user.target" ];
        path = with pkgs; [
          # I don't know why it needs bash
          bash
          nodePackages.nodejs
          nodePackages.npm
        ];
        script = lib.concatStringsSep " " [
          # -y: skip installation confirmation
          "npm exec -y"
          # seems like it should be implied by the command being run, but apparently not
          "--package postgraphile"
          "--package @graphile-contrib/pg-simplify-inflector"
          "--"
          "postgraphile"
          "--append-plugins @graphile-contrib/pg-simplify-inflector"
          "--dynamic-json"
          "--no-setof-functions-contain-nulls"
          "--no-ignore-rbac"
          "--enable-query-batching"
          "--legacy-relations omit"
          "--connection 'socket:/run/postgresql?db=flexiprocity'"
          "--jwt-secret \"$(cat /home/api/secrets/jwt/public-key.pem)\""
          "--jwt-verify-algorithms RS256"
          "--jwt-verify-clock-tolerance 1"
          "--cors"
          "--schema public"
        ];
        serviceConfig = {
          User = "api";
        };
      };
    };
    users.users.api = {
      isNormalUser = true;
    };
    services.nginx = {
      enable = true;
      virtualHosts = {
        "flexiprocity.rpm.cc" = {
          forceSSL = true;
          enableACME = true;
          locations."/auth" = {
            proxyPass = "http://127.0.0.1:5001/";
            recommendedProxySettings = true;
          };
          locations."/" = {
            root = "${frontend}";
            tryFiles = "$uri /index.html =404";
          };
        };
      };
    };
  };
}
