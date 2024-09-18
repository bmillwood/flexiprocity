{ nixpkgs ? <nixpkgs>
, config ? {}
, # sigh. the problem is that src needs to be the full git repository in order
  # for the privacy policy version stuff to work; this is already sad because
  # it means the frontend dir isn't self-contained, but it's worse than that
  # because flexiprocity could be a submodule, in which case copying the
  # flexiprocity dir isn't enough because its .git just references the enclosing
  # repo, so we need that too
  # using a path value for flexiprocitySubmodule doesn't work because that just
  # results in it getting copied to its own place in the nix store, whereas we
  # specifically need the submodule inside the git repo
  # surely there's a better way but this will do for now
  gitRoot ? ../.
, flexiprocitySubmodule ? "."
}:

let
  pkgs = import nixpkgs config;
  inherit (pkgs) stdenv;
in
stdenv.mkDerivation {
  name = "flexiprocity-frontend";
  src = gitRoot;
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
    pushd "./$flexiprocitySubmodule/frontend"
    cp *.{html,css} driver.js "$out/"
    bash output-version-file.sh > "$out"/version.js
    elm make --output="$out/elm.js" src/Main.elm
    popd
  '';
}
