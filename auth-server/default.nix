{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) lib fetchFromGitHub haskell;
in
nixpkgs.haskellPackages.callPackage ./auth-server.nix {
  oidc-client = haskell.lib.compose.overrideSrc {
    src = fetchFromGitHub {
      owner = "bmillwood";
      repo = "haskell-oidc-client";
      rev = "2d19db09bf13f02f49248f7b21703b2c59e06ecc";
      hash = "sha256-DuVFfsv0lpV0sMfAk0blbDhMQeia5lzUTH1GH/FSslg=";
    };
  } nixpkgs.haskellPackages.oidc-client;
}
