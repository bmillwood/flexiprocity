{ mkDerivation, aeson, async, base, containers, dns, fetchgit
, http-api-data, http-client, http-types, lib, network-uri, text
, transformers
}:
mkDerivation {
  pname = "bluesky-tools";
  version = "0.6.0.2";
  src = fetchgit {
    url = "https://github.com/bmillwood/bluesky-tools";
    sha256 = "sha256-Du9mF7bYmZJjl/esMLxu7MF4qjSnbf00cTQyyhoG0c0=";
    rev = "484ecc0335bcc49d5a0ca1c59299f2fbcce08d08";
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
