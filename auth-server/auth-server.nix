{ mkDerivation, aeson, async, base, bluesky-tools, bytestring
, containers, cookie, crypton, crypton-x509, crypton-x509-store
, http-api-data, http-client, http-client-tls, http-types, jose
, lens, lib, memory, mtl, network-uri, oidc-client, random
, raven-haskell, req, servant, servant-server, text, time, wai
, wai-cors, warp
}:
mkDerivation {
  pname = "flexiprocity-auth-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  doHaddock = false;
  enableLibraryProfiling = false;
  enableSharedLibraries = false;
  libraryHaskellDepends = [
    aeson async base bluesky-tools bytestring containers cookie crypton
    crypton-x509 crypton-x509-store http-api-data http-client
    http-client-tls http-types jose lens memory mtl network-uri
    oidc-client random raven-haskell req servant servant-server text
    time wai wai-cors warp
  ];
  executableHaskellDepends = [ base ];
  description = "Server which provides jwts for flexiprocity";
  license = lib.licenses.bsd3;
  mainProgram = "auth-server";
}
