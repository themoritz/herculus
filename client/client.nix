{ mkDerivation, aeson, base, bytestring, containers, deepseq
, ghcjs-base, ghcjs-dom, http-api-data, lens, herculus-lib, mtl, react-flux
, react-flux-servant, servant, stdenv, text, transformers
}:
mkDerivation {
  pname = "client";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers deepseq ghcjs-base ghcjs-dom
    http-api-data lens herculus-lib mtl react-flux react-flux-servant
    servant text transformers
  ];
  license = stdenv.lib.licenses.unfree;
  doHaddock = false;
}
