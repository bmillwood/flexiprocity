# https://nixos.org/manual/nixpkgs/stable/#javascript-buildNpmPackage
{ nixpkgs ? <nixpkgs>
, config ? {}
}:

let
  pkgs = import nixpkgs config;
in
pkgs.buildNpmPackage rec {
  pname = "postgraphile";
  version = "4.13.0";

  src = pkgs.fetchFromGitHub {
    owner = "graphile";
    repo = "crystal";
    rev = "v${version}";
    sha256 = "sha256-fBBAYMDrpnMUMidzgj7mEYkY/YJa9x3BIjBt3FPSVKY=";
  };

  preBuild = ''
    export PATH=${pkgs.yarn}/bin:$PATH
    patchShebangs --build scripts
  '';

  postPatch = ''
    ln -s ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-vAOAImL07IA2L7GZah4ZLjlp6w1oYQY/hvdPk7c1gxA=";
  npmPackFlags = [ "--ignore-scripts" ];
}
