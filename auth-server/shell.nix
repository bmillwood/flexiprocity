{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    nixpkgs.haskell.lib.addBuildTools drv [ nixpkgs.cabal-install ];
}
