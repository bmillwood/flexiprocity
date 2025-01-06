{ mkDerivation, aeson, async, base, containers, dns, fetchgit
, http-api-data, http-client, http-types, lib, network-uri, text
, transformers
}:
mkDerivation {
  pname = "bluesky-tools";
  version = "0.4.0.0";
  src = fetchgit {
    url = "https://github.com/bmillwood/bluesky-tools";
    sha256 = "1z1svvbyvr9jbn0nw4051lfwh5qq277d5fwhvjwjrjvyv0g4s2q9";
    rev = "971e087bbd3000009d74fda34d5745b3c280f949";
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
