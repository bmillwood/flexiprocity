{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) lib fetchFromGitHub haskell;
in
nixpkgs.haskellPackages.callPackage ./auth-server.nix {
  bluesky-tools =
    haskell.lib.compose.doJailbreak
      (nixpkgs.haskellPackages.callPackage ./bluesky-tools.nix {});
  oidc-client = haskell.lib.compose.overrideSrc {
    src = fetchFromGitHub {
      owner = "bmillwood";
      repo = "haskell-oidc-client";
      rev = "f9e6fe672b0e1e4cf89b29cad85ad07dcad364d4";
      hash = "sha256-qFLBstUqgbSINa6RDPb/csfaQ0421i3Sm0/GiGqtrLI=";
    };
  } nixpkgs.haskellPackages.oidc-client;
}
