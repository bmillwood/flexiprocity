{ nixpkgs ? <nixpkgs>
, config ? {}
}:

let
  pkgs = import nixpkgs config;
  inherit (pkgs) stdenv;
in
stdenv.mkDerivation {
  name = "flexiprocity-frontend";
  src = ../.;
  buildInputs = [
    pkgs.elmPackages.elm
    pkgs.git
  ];
  configurePhase = pkgs.elmPackages.fetchElmDeps {
    # generate with `elm2nix --convert`
    elmPackages = import ./elm-srcs.nix;
    elmVersion = "0.19.1";
    registryDat = ~/.elm/0.19.1/packages/registry.dat;
  };
  buildPhase = ''
    mkdir "$out"
    pushd frontend
    cp *.{html,css} driver.js "$out/"
    bash output-version-file.sh > "$out"/version.js
    elm make --output="$out/elm.js" src/Main.elm
    popd
  '';
}
