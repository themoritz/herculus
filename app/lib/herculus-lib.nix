{ mkDerivation, aeson, attoparsec, base, base64-bytestring, bson
, bytestring, case-insensitive, cereal, comonad, containers
, email-validate, file-embed, free, http-api-data, http-media
, http-types, lens, megaparsec, mtl, optparse-applicative
, pretty-show, protolude, purescript-bridge, pwstore-fast
, recursion-schemes, scientific, servant, servant-purescript
, stdenv, text, these, time, transformers, union-find
, unordered-containers, vector, wl-pprint-text
}:
mkDerivation {
  pname = "herculus-lib";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring bson bytestring
    case-insensitive cereal comonad containers email-validate
    file-embed free http-api-data http-media http-types lens megaparsec
    mtl pretty-show protolude pwstore-fast recursion-schemes scientific
    servant text these time transformers union-find
    unordered-containers vector wl-pprint-text
  ];
  executableHaskellDepends = [
    base containers lens optparse-applicative purescript-bridge
    servant-purescript text
  ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  license = stdenv.lib.licenses.unfree;
}
