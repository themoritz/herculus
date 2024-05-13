{ mkDerivation, aeson, base, base64-bytestring, bson-lens
, bytestring, containers, directory, doctemplates, either, entropy
, fast-logger, filepath, herculus-lib, lens, lib, mime-mail
, monad-control, monad-logger, mongoDB, mtl, network
, optparse-applicative, pandoc, pretty-show, recursion-schemes
, servant, servant-server, stm, temporary, text, time, transformers
, transformers-base, unordered-containers, wai, wai-websockets
, warp, websockets
}:
mkDerivation {
  pname = "server";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring bson-lens bytestring containers
    directory doctemplates entropy fast-logger filepath herculus-lib
    lens mime-mail monad-control monad-logger mongoDB mtl network
    optparse-applicative pandoc pretty-show recursion-schemes servant
    servant-server stm temporary text time transformers
    transformers-base unordered-containers wai websockets
  ];
  executableHaskellDepends = [
    aeson base bytestring either herculus-lib mongoDB mtl servant
    servant-server stm text transformers wai-websockets warp websockets
  ];
  testHaskellDepends = [ base herculus-lib ];
  license = "unknown";
  mainProgram = "server-exe";
}
