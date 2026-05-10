{ pkgs ? import <nixpkgs> {} }:

let
  testHost = "flexiprocity.test";

  # Self-signed cert for the test virtualhost. We trust it as a CA below so
  # curl (and the OIDC redirect chain) can use HTTPS without -k.
  testCerts = pkgs.runCommand "flexiprocity-test-certs" {
    nativeBuildInputs = [ pkgs.openssl ];
  } ''
    mkdir -p $out
    openssl req -x509 -newkey rsa:2048 -nodes -days 3650 \
      -keyout $out/key.pem \
      -out $out/cert.pem \
      -subj '/CN=${testHost}' \
      -addext 'subjectAltName=DNS:${testHost}'
  '';

  # The provider is named "google" because the schema reads
  # jwt.claims.google via get_google_field()/get_google_email(). The actual
  # IdP here is dex's mockCallback connector — see services.dex below.
  oidcProviders = pkgs.writeText "oidc_providers.json" (builtins.toJSON {
    google = {
      issuer = "http://127.0.0.1:5556/dex";
      client_id = "flexiprocity-test";
      client_secret = "test-client-secret";
    };
  });
in

pkgs.testers.nixosTest {
  name = "flexiprocity-module";

  nodes.machine = { config, lib, pkgs, ... }: {
    imports = [ ../config.nix ];

    services.flexiprocity = {
      enable = true;
      virtualHost = testHost;
      postgraphileDevMode = true;
    };

    # ACME isn't reachable from the test VM; substitute a self-signed cert
    # and trust it as a root so HTTPS works end-to-end.
    services.nginx.virtualHosts.${testHost} = {
      enableACME = lib.mkForce false;
      sslCertificate = "${testCerts}/cert.pem";
      sslCertificateKey = "${testCerts}/key.pem";
    };
    security.pki.certificateFiles = [ "${testCerts}/cert.pem" ];

    # Dex stands in for Google here. mockCallback returns a hardcoded
    # identity (kilgore@kilgore.trout) without any user interaction, so
    # curl -L can walk the whole authorization-code dance.
    services.dex = {
      enable = true;
      settings = {
        issuer = "http://127.0.0.1:5556/dex";
        storage.type = "memory";
        web.http = "127.0.0.1:5556";
        oauth2.skipApprovalScreen = true;
        enablePasswordDB = false;
        staticClients = [{
          id = "flexiprocity-test";
          name = "flexiprocity test";
          secret = "test-client-secret";
          redirectURIs = [ "https://${testHost}/auth/login/oidc/complete/google" ];
        }];
        connectors = [{
          type = "mockCallback";
          id = "mock";
          name = "Mock";
        }];
      };
    };

    # auth-server reads oidc_providers.json from SECRETS_DIR at init.
    # tmpfiles.d runs before user/group activation, so use a oneshot.
    systemd.services.flexiprocity-test-secrets = {
      description = "Stage flexiprocity test secrets";
      wantedBy = [ "multi-user.target" ];
      before = [ "flexiprocity-auth-server.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = "api";
      };
      script = ''
        mkdir -p /home/api/secrets
        cp ${oidcProviders} /home/api/secrets/oidc_providers.json
      '';
    };

    networking.hosts."127.0.0.1" = [ testHost ];
  };

  testScript = ''
    import json

    machine.wait_for_unit("multi-user.target")
    machine.wait_for_unit("postgresql.target")
    machine.wait_for_unit("flexiprocity-agent.service")
    machine.wait_for_unit("postgraphile.service")
    machine.wait_for_unit("nginx.service")
    machine.wait_for_unit("dex.service")
    machine.wait_for_unit("flexiprocity-auth-server.service")
    machine.wait_for_open_port(443)
    machine.wait_for_open_port(5556)
    machine.wait_for_open_port(5000)
    machine.wait_for_open_port(5001)

    # Frontend root is served by nginx.
    machine.succeed("curl -fsS https://${testHost}/ -o /dev/null")

    # Hit a postgres function through postgraphile. Anonymous, so it
    # should resolve to null rather than an auth error.
    response = machine.succeed(
      "curl -fsS -X POST -H 'Content-Type: application/json' "
      "-d '{\"query\":\"{ currentUserId }\"}' "
      "https://${testHost}/graphql"
    )
    payload = json.loads(response)
    assert payload == {"data": {"currentUserId": None}}, payload

    # Drive the OIDC flow: GET /start/google → 303 to dex → mockCallback
    # → 302 back to /complete/google → 303 to /. The cookie jar
    # accumulates the OIDC session cookie and finally the JWT cookie.
    machine.succeed(
      "curl -fsS -L --cookie-jar /tmp/cookies "
      "https://${testHost}/auth/login/oidc/start/google -o /dev/null"
    )
    cookies = machine.succeed("cat /tmp/cookies")
    assert "\tjwt\t" in cookies, "no jwt cookie after auth flow:\n" + cookies

    # Authenticated mutation: getOrCreateUserId reads jwt.claims.google.email
    # and inserts a row.
    response = machine.succeed(
      "curl -fsS --cookie /tmp/cookies "
      "-X POST -H 'Content-Type: application/json' "
      "-d '{\"query\":\"mutation { getOrCreateUserId(input: {clientMutationId: \\\"x\\\"}) { clientMutationId } }\"}' "
      "https://${testHost}/graphql"
    )
    payload = json.loads(response)
    assert "errors" not in payload, payload
    assert payload["data"]["getOrCreateUserId"]["clientMutationId"] == "x", payload

    # currentUserId now resolves to the new row's id, proving the JWT
    # round-trips through nginx → postgraphile and the schema sees the
    # google claims.
    response = machine.succeed(
      "curl -fsS --cookie /tmp/cookies "
      "-X POST -H 'Content-Type: application/json' "
      "-d '{\"query\":\"{ currentUserId }\"}' "
      "https://${testHost}/graphql"
    )
    payload = json.loads(response)
    assert payload["data"]["currentUserId"] is not None, payload

    # Not strictly part of the test, but useful to pocket
    machine.wait_for_file("/run/postgraphile/schema.graphql")
    machine.copy_from_machine("/run/postgraphile/schema.graphql", ".")
  '';
}
