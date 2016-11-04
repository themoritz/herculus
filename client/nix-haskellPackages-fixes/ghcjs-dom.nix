{ mkDerivation, base, ghcjs-dom-jsffi, stdenv, text, transformers
}:
mkDerivation {
  pname = "ghcjs-dom";
  version = "0.4.0.0";
  sha256 = "0bnh05zzd9bcr96wiymxmbvbxd07bg6yshw4wc8pspj62vika5x2";
  libraryHaskellDepends = [
    base ghcjs-dom-jsffi text transformers
  ];
  description = "DOM library that supports both GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
