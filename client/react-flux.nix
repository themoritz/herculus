{ mkDerivation, aeson, base, bytestring, deepseq, ghcjs-base, mtl, stdenv
, template-haskell, text, time, unordered-containers
}:
mkDerivation {
  pname = "react-flux";
  version = "1.2.3";
  sha256 = "1ixipyzl1517as7sxfz6l0sgxm2w1vmsjfnmsi110asjnvl6ij35";
  libraryHaskellDepends = [
    aeson base bytestring deepseq ghcjs-base mtl template-haskell text time
    unordered-containers
  ];
  homepage = "https://bitbucket.org/wuzzeb/react-flux";
  description = "A binding to React based on the Flux application architecture for GHCJS";
  license = stdenv.lib.licenses.bsd3;
}
