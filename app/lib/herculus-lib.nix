{ mkDerivation, aeson, attoparsec, base, base64-bytestring, bson
, bytestring, case-insensitive, cereal, containers, Decimal
, deepseq, http-api-data, http-media, http-types, lens, mtl, parsec
, pwstore-fast, scientific, servant, stdenv, text, these, time
, transformers, union-find, unordered-containers, vector, email-validate
}:
mkDerivation {
  pname = "herculus-lib";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring bson bytestring
    case-insensitive cereal containers Decimal deepseq http-api-data
    http-media http-types lens mtl parsec pwstore-fast scientific
    servant text these time transformers union-find unordered-containers
    vector email-validate
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/githubuser/lib#readme";
  description = "Shared by server and client";
  license = stdenv.lib.licenses.unfree;
  doHaddock = false;
}
