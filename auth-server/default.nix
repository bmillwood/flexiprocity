{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) lib fetchFromGitHub haskell;
in
nixpkgs.haskellPackages.callPackage ./auth-server.nix {
  bluesky-tools =
    nixpkgs.haskellPackages.callPackage ./bluesky-tools.nix {};
  oidc-client =
    lib.pipe nixpkgs.haskellPackages.oidc-client [
      (haskell.lib.compose.overrideSrc {
        src = fetchFromGitHub {
          owner = "bmillwood";
          repo = "haskell-oidc-client";
          rev = "a0af0d617b8752ff4c30f009855257f5d6bc3153";
          hash = "sha256-SP1OnHnwd5MBDzFUW2G8e/auqKlaLkmkvhq8GxHEoOM=";
        };
      })
      haskell.lib.markUnbroken
    ];
}
