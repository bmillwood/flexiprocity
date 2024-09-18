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
    systemd.services.authServer = {
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
