{ mkDerivation, aeson, attoparsec, base, base64-bytestring, bson
, bytestring, case-insensitive, cereal, comonad, containers
, data-fix, email-validate, file-embed, free, hashable
, http-api-data, http-media, http-types, lens, lib, megaparsec, mtl
, optparse-applicative, parser-combinators, pretty-show, protolude
, purescript-bridge, pwstore-fast, recursion-schemes, scientific
, semialign, servant, servant-purescript, text, these, time
, transformers, union-find, unordered-containers, vector
, wl-pprint-text
}:
mkDerivation {
  pname = "herculus-lib";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring bson bytestring
    case-insensitive cereal comonad containers data-fix email-validate
    file-embed free hashable http-api-data http-media http-types lens
    megaparsec mtl parser-combinators pretty-show protolude
    pwstore-fast recursion-schemes scientific semialign servant text
    these time transformers union-find unordered-containers vector
    wl-pprint-text
  ];
  executableHaskellDepends = [
    base containers lens optparse-applicative purescript-bridge
    servant-purescript text
  ];
  testHaskellDepends = [ base ];
  license = "unknown";
}
