{ mkDerivation, aeson, async, base, bluesky-tools, http-client
, http-client-tls, lib, network-uri, postgresql-simple, text
}:
mkDerivation {
  pname = "flexiprocity-agent";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  doHaddock = false;
  enableLibraryProfiling = false;
  enableSharedLibraries = false;
  libraryHaskellDepends = [
    aeson async base bluesky-tools http-client http-client-tls
    network-uri postgresql-simple text
  ];
  executableHaskellDepends = [ base ];
  description = "Server which runs misc tasks for flexiprocity";
  license = lib.licenses.bsd3;
  mainProgram = "flexiprocity-agent";
}
