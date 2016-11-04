{ mkDerivation, stdenv, base, ghcjs-base, ghcjs-prim, ghc-prim, text, transformers }:
mkDerivation {
  pname = "ghcjs-dom-jsffi";
  version = "0.4.0.0";
  sha256 = "0zrlyzq4xgg3pqm60whc602iy2ijh4ga9d620q83w8v5vm2a81fl";
  libraryHaskellDepends = [
    base ghcjs-base ghcjs-prim ghc-prim text transformers
  ];
  description = "DOM library using JSFFI and GHCJS";
  license = stdenv.lib.licenses.mit;
}
