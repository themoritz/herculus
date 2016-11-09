{ mkDerivation, base, ghc-prim, ghcjs-base, ghcjs-prim, stdenv
, text, transformers
}:
mkDerivation {
  pname = "ghcjs-dom-jsffi";
  version = "0.5.0.1";
  sha256 = "1kbwrkmk6hxik9bclr7mpg1g1gg2x7c76z93r2pw9r6qyn4vmvny";
  libraryHaskellDepends = [
    base ghc-prim ghcjs-base ghcjs-prim text transformers
  ];
  description = "DOM library using JSFFI and GHCJS";
  license = stdenv.lib.licenses.mit;
}
