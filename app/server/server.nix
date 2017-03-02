{ mkDerivation, aeson, base, base64-bytestring, bytestring
, containers, directory, either, entropy, fast-logger, filepath
, herculus-lib, lens, mime-mail, monad-control, monad-logger
, mongoDB, mtl, neat-interpolation, network, optparse-applicative
, pandoc, pretty-show, servant, servant-server, stdenv, stm, text
, time, transformers, transformers-base, wai, wai-websockets, warp
, websockets
}:
mkDerivation {
  pname = "server";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring containers directory
    entropy fast-logger filepath herculus-lib lens mime-mail
    monad-control monad-logger mongoDB mtl neat-interpolation network
    optparse-applicative pandoc pretty-show servant servant-server stm
    text time transformers transformers-base wai websockets
  ];
  executableHaskellDepends = [
    aeson base bytestring either herculus-lib mongoDB mtl servant
    servant-server stm text transformers wai-websockets warp websockets
  ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  license = stdenv.lib.licenses.unfree;
}
