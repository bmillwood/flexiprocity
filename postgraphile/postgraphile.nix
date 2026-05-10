{ buildNpmPackage, lib }:

let
  pname = "flexiprocity-postgraphile";
in
buildNpmPackage {
  inherit pname;
  version = "0.0.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./package.json
      ./package-lock.json
      ./subscriptions.js
      ./postgraphile.tags.json5
    ];
  };

  # Set to lib.fakeHash on the first build, then copy the real hash from
  # the error message into here.
  npmDepsHash = "sha256-Ggmo30zaxi3Ln7CaZpwc7UqWV31pwxMho1T0lT54tR0=";

  # We don't have a build step — this "package" only declares deps so we
  # can pin them via package-lock.json.
  dontNpmBuild = true;

  # buildNpmPackage only links our own package's bins by default. Expose
  # postgraphile (a transitive bin) explicitly.
  postInstall = ''
    mkdir -p $out/bin
    ln -s $out/lib/node_modules/${pname}/node_modules/.bin/postgraphile \
      $out/bin/postgraphile
  '';
}
