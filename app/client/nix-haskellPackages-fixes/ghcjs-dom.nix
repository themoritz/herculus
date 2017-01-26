{ mkDerivation, base, ghcjs-dom-jsffi, stdenv, text, transformers
}:
mkDerivation {
  pname = "ghcjs-dom";
  version = "0.5.0.1";
  sha256 = "1rqdjfjfgwqxnfr8y342zymrsq8vmb6v4phwfgk6jhc446pfbjcl";
  libraryHaskellDepends = [ base ghcjs-dom-jsffi text transformers ];
  description = "DOM library that supports both GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
  doHaddock = false;
}
