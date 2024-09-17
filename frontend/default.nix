{ nixpkgs ? <nixpkgs>
, config ? {}
, # This derivation uses a src that doesn't contain the git dir,
  # and it seems a shame to pull in the whole git dir just for this
  privacyPolicyVersion
}:

let
  pkgs = import nixpkgs config;
  inherit (pkgs) stdenv;
in
stdenv.mkDerivation {
  name = "flexiprocity-frontend";
  src = ./.;
  buildInputs = [
    pkgs.elmPackages.elm
  ];
  configurePhase = pkgs.elmPackages.fetchElmDeps {
    # generate with `elm2nix --convert`
    elmPackages = import ./elm-srcs.nix;
    elmVersion = "0.19.1";
    registryDat = ~/.elm/0.19.1/packages/registry.dat;
  };
  buildPhase = ''
    mkdir "$out"
    cp *.{html,css,js} "$out/"
    bash output-version-file.sh "${privacyPolicyVersion}" > "$out"/version.js
    elm make --output="$out/elm.js" src/Main.elm
  '';
}
