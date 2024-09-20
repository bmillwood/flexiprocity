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
      postgraphileDevMode = lib.mkOption {
        type = types.bool;
        default = false;
      };

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
      postgraphile =
        let
          prodFlags = [
            "--retry-on-init-fail"
            "--extended-errors errcode"
            "--disable-query-log"
          ];
          devFlags = [
            "--show-error-stack=json"
            "--extended-errors hint,detail,errcode"
            # copied from my dev script, but not possible here
            #"--watch"
            #"--owner-connection 'socket:/run/postgresql?db=flexiprocity&user=postgres'"
            "--export-schema-graphql schema.graphql"
            "--graphiql /"
            "--enhance-graphiql"
            "--allow-explain"
          ];
          scriptPieces = [
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
          ] ++ (if cfg.postgraphileDevMode then devFlags else prodFlags);
        in {
        description = "flexiprocity postgraphile";
        after = [ "postgresql.service" ];
        wantedBy = [ "multi-user.target" ];
        path = with pkgs; [
          # I don't know why it needs bash
          bash
          nodePackages.nodejs
          nodePackages.npm
        ];
        script = lib.concatStringsSep "\n" [
          # since we want to create this directory, WorkingDirectory isn't useful
          "mkdir -p ~/postgraphile"
          "cd ~/postgraphile"
          # somewhat silly to copy this every time, but can't think of a better way
          "cp ${./postgraphile/postgraphile.tags.json5} postgraphile.tags.json5"
          (lib.concatStringsSep " " scriptPieces)
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
      appendHttpConfig = ''
        map $cookie_jwt $authorization {
          "" "";
          default "Bearer $cookie_jwt";
        }
      '';
      virtualHosts = {
        "flexiprocity.rpm.cc" = {
          forceSSL = true;
          enableACME = true;
          locations = {
            "/auth" = {
              proxyPass = "http://127.0.0.1:5001/";
              recommendedProxySettings = true;
            };
            "/graphql" = {
              extraConfig = ''
                proxy_set_header Authorization "$authorization";
              '';
              proxyPass = "http://localhost:5000/graphql";
              recommendedProxySettings = true;
            };
            "/" = {
              root = "${frontend}";
              tryFiles = "$uri /index.html =404";
            };
          } // (if cfg.postgraphileDevMode then {
              "/graphiql" = {
                proxyPass = "http://localhost:5000/";
              };
            } else {}
          );
        };
      };
    };
  };
}
