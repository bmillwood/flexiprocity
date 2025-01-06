{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.haskellPackages.shellFor {
  packages = hpkgs: [
    (import ./. { inherit nixpkgs; })
  ];
  nativeBuildInputs = [
    nixpkgs.cabal-install
    nixpkgs.inotify-tools # for inotifywait in build.sh
  ];
}
