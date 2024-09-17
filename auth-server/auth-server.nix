{ mkDerivation, aeson, base, bytestring, containers, cookie
, crypton, crypton-x509, crypton-x509-store, http-api-data
, http-client, http-client-tls, http-types, jose-jwt, lib, memory
, mtl, oidc-client, random, req, servant, servant-server, text
, time, wai, wai-cors, warp, cryptonite, x509, x509-store
}:
let
  cryptoDeps =
    if builtins.compareVersions jose-jwt.version "0.10" >= 0
    then [
      crypton crypton-x509 crypton-x509-store
    ] else [
      cryptonite x509 x509-store
    ];
in
mkDerivation {
  pname = "auth-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  doHaddock = false;
  enableLibraryProfiling = false;
  enableSharedLibraries = false;
  libraryHaskellDepends = [
    aeson base bytestring containers cookie http-api-data http-client
    http-client-tls http-types jose-jwt memory mtl oidc-client random
    req servant servant-server text time wai wai-cors warp
  ] ++ cryptoDeps;
  executableHaskellDepends = [ base ];
  description = "Server which provides jwts for flexiprocity";
  license = lib.licenses.bsd3;
  mainProgram = "auth-server";
}
