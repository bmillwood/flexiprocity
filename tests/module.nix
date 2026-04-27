{ pkgs ? import <nixpkgs> {} }:

pkgs.testers.nixosTest {
  name = "flexiprocity-module";

  nodes.machine = { config, lib, pkgs, ... }: {
    imports = [ ../config.nix ];

    services.flexiprocity = {
      enable = true;
      virtualHost = "flexiprocity.test";
    };

    # ACME and HTTPS aren't reachable from inside the test VM.
    services.nginx.virtualHosts."flexiprocity.test" = {
      forceSSL = lib.mkForce false;
      enableACME = lib.mkForce false;
    };

    # postgraphile reads /home/api/secrets/jwt/public-key.pem to verify
    # JWTs. Generate a throwaway keypair at activation, using the same
    # script as local dev so we don't drift.
    system.activationScripts.flexiprocity-test-jwt = ''
      if [ ! -f /home/api/secrets/jwt/public-key.pem ]; then
        mkdir -p /home/api/secrets
        cd /home/api/secrets
        PATH=${pkgs.openssl}/bin:$PATH ${pkgs.bash}/bin/bash ${../secrets/new-jwt-key.sh}
        chown -R api /home/api/secrets
      fi
    '';

    # auth-server's whole job is talking to external identity providers,
    # and it reads several secret files (RSA key, client secrets) at
    # startup. Not much we can usefully test in a sealed VM.
    systemd.services.flexiprocity-auth-server.enable = lib.mkForce false;

    networking.hosts."127.0.0.1" = [ "flexiprocity.test" ];
  };

  testScript = ''
    machine.wait_for_unit("multi-user.target")
    machine.wait_for_unit("postgresql.target")
    machine.wait_for_unit("flexiprocity-agent.service")
    machine.wait_for_unit("postgraphile.service")
    machine.wait_for_unit("nginx.service")
    machine.wait_for_open_port(80)

    # Frontend root is served by nginx.
    machine.succeed("curl -fsS http://flexiprocity.test/ -o /dev/null")
  '';
}
