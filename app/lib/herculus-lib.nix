{ mkDerivation, aeson, attoparsec, base, base64-bytestring, bson
, bytestring, case-insensitive, cereal, containers, email-validate
, http-api-data, http-media, http-types, lens, mtl
, optparse-applicative, parsec, protolude, purescript-bridge
, pwstore-fast, recursion-schemes, scientific, servant
, servant-purescript, stdenv, text, these, time, transformers
, union-find, unordered-containers, vector
}:
mkDerivation {
  pname = "herculus-lib";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring bson bytestring
    case-insensitive cereal containers email-validate http-api-data
    http-media http-types lens mtl parsec protolude pwstore-fast
    recursion-schemes scientific servant text these time transformers
    union-find unordered-containers vector
  ];
  executableHaskellDepends = [
    base containers lens optparse-applicative purescript-bridge
    servant-purescript text
  ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  license = stdenv.lib.licenses.unfree;
}
