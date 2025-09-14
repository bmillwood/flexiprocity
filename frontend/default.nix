{ nixpkgs ? <nixpkgs>
, config ? {}
}:

let
  pkgs = import nixpkgs config;
  inherit (pkgs) stdenvNoCC;
in
stdenvNoCC.mkDerivation {
  name = "flexiprocity-frontend";
  src = ./.;
  buildInputs = [
    pkgs.elmPackages.elm
    pkgs.git
  ];
  configurePhase = pkgs.elmPackages.fetchElmDeps {
    # generate with `elm2nix convert`
    elmPackages = import ./elm-srcs.nix;
    elmVersion = "0.19.1";
    registryDat = ./registry.dat;
  };
  buildPhase = ''
    mkdir "$out"
    cp -r icons *.{html,css} driver.js "$out/"
    elm make --output="$out/elm.js" src/Main.elm
  '';
}
