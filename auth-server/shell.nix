{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc96" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
