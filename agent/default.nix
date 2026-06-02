{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.haskellPackages.callPackage ./flexiprocity-agent.nix {}
