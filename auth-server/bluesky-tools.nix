{ mkDerivation, aeson, async, base, containers, dns, fetchgit
, http-api-data, http-client, http-types, lib, network-uri, text
, transformers
}:
mkDerivation {
  pname = "bluesky-tools";
  version = "0.6.0.0";
  src = fetchgit {
    url = "https://github.com/bmillwood/bluesky-tools";
    sha256 = "02wyrwdwq4y2c4r3y1bs6rflx1h1wvw4hxbp8342g1g6z1y89f2i";
    rev = "2161365fa0970653d5a356828958b4bdc4cd78d8";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson async base containers dns http-api-data http-client
    http-types network-uri text transformers
  ];
  testHaskellDepends = [ base text ];
  description = "Tools for interacting with Bluesky / AT Protocol";
  license = lib.licenses.bsd3;
}
