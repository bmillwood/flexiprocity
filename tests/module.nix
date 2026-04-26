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

    # postgraphile runs `npm exec` at startup, which needs network access
    # that the test sandbox doesn't have. Skip it here.
    systemd.services.postgraphile.enable = lib.mkForce false;

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
    machine.wait_for_unit("nginx.service")
    machine.wait_for_open_port(80)

    # Frontend root is served by nginx.
    machine.succeed("curl -fsS http://flexiprocity.test/ -o /dev/null")
  '';
}
