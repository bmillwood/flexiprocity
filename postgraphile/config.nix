# I did not get postgraphile working as a "real" package.
# The latest version requires yarn's v2 lockfile, and nixpkgs doesn't support it:
#   https://github.com/NixOS/nixpkgs/issues/254369
# npm exec works OK in the meantime, though maybe I should learn how to pin
# package versions.
{ config, lib, pkgs, ... }:
let
  cfg = config.services.flexiprocity;
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
        "cp ${./postgraphile.tags.json5} postgraphile.tags.json5"
        # because otherwise overwriting it next time will fail
        "chmod u+w postgraphile.tags.json5"
        (lib.concatStringsSep " " scriptPieces)
      ];
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
