{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) lib fetchFromGitHub haskell;
in
nixpkgs.haskellPackages.callPackage ./flexiprocity-agent.nix {
  bluesky-tools =
    nixpkgs.haskellPackages.callPackage ../auth-server/bluesky-tools.nix {};
}
