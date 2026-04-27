{ config, lib, pkgs, ... }:
let
  cfg = config.services.flexiprocity;
  postgraphile = pkgs.callPackage ./postgraphile.nix {};
  pkgRoot = "${postgraphile}/lib/node_modules/flexiprocity-postgraphile";
  inherit (lib) mkIf mkMerge mkOption types;
in
{
  options = {
    services.flexiprocity = {
      postgraphileDevMode = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services.postgraphile =
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
        flags = [
          "--plugins @graphile/pg-pubsub"
          "--append-plugins @graphile-contrib/pg-simplify-inflector,${pkgRoot}/subscriptions.js"
          "--subscriptions"
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
      requires = [ "postgresql.target" ];
      after = [ "postgresql.target" ];
      wantedBy = [ "multi-user.target" ];
      # cd into pkgRoot so postgraphile auto-detects postgraphile.tags.json5
      # (it only looks in cwd) and node resolves graphile-utils for
      # subscriptions.js via the bundled node_modules.
      script = ''
        cd ${pkgRoot}
        exec ${postgraphile}/bin/postgraphile ${lib.concatStringsSep " " flags}
      '';
      path = [ pkgs.coreutils ];
      serviceConfig = {
        User = "api";
      };
    };
    services.nginx = {
      appendHttpConfig = ''
        map $cookie_jwt $authorization {
          "" "";
          default "Bearer $cookie_jwt";
        }
      '';
      virtualHosts.${cfg.virtualHost}.locations = mkMerge [
        {
          "/graphql" = {
            extraConfig = ''
              proxy_set_header Authorization "$authorization";
            '';
            proxyPass = "http://localhost:5000/graphql";
            proxyWebsockets = true;
            recommendedProxySettings = true;
          };
        }
        (mkIf cfg.postgraphileDevMode {
          "/graphiql" = {
            proxyPass = "http://localhost:5000/";
          };
        })
      ];
    };
  };
}
