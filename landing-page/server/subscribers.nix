{ mkDerivation, aeson, base, bson, bytestring, data-default, diener
, lifted-base, monad-control, mongoDB, mtl, network
, optparse-applicative, parsec, servant-server, stdenv, text, time
, transformers, transformers-base, warp, cabal-install
}:
mkDerivation {
  pname = "herculus-subscribers";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bson data-default diener lifted-base monad-control
    mongoDB mtl network parsec servant-server text time transformers
    transformers-base
  ];
  executableHaskellDepends = [
    base bytestring diener mongoDB mtl network optparse-applicative
    servant-server text warp
  ];
  testHaskellDepends = [ base ];
  # not sure if necessary
  # doHaddock = false;
  homepage = "https://github.com/rubenmoor/herculus-subscribers#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
  # todo: move to default.nix
  buildTools = [ cabal-install ];
}
