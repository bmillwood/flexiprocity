{ lib, pkgs, ... }:
let
  authServer = pkgs.callPackage ./auth-server {};
  frontend = pkgs.callPackage ./frontend {
    privacyPolicyVersion = "worry about this later";
  };
in
{
  config = {
    systemd.services.authServer = {
      description = "flexiprocity auth server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        User = "api";
        ExecStart = "${authServer}/bin/auth-server";
      };
    };
    users.users.api = {
      isNormalUser = true;
    };
    services.nginx = {
      enable = true;
      virtualHosts = {
        "flexiprocity.rpm.cc" = {
          forceSSL = false;
          enableACME = false;
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
