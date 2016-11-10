{ mkDerivation, aeson, attoparsec, base, base64-bytestring, bson
, bytestring, case-insensitive, cereal, containers, Decimal
, deepseq, http-api-data, http-media, http-types, lens, mtl, parsec
, pwstore-fast, scientific, servant, stdenv, text, time
, transformers, union-find, unordered-containers, vector
}:
mkDerivation {
  pname = "herculus-lib";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring bson bytestring
    case-insensitive cereal containers Decimal deepseq http-api-data
    http-media http-types lens mtl parsec pwstore-fast scientific
    servant text time transformers union-find unordered-containers
    vector
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/githubuser/lib#readme";
  description = "Shared by server and client";
  license = stdenv.lib.licenses.unfree;
  doHaddock = false;
}
