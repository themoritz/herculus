{ mkDerivation, ansi-terminal, base, bytestring, data-default
, either, fast-logger, fetchgit, monad-control, monad-logger, mtl
, stdenv, time, transformers, transformers-base
}:
mkDerivation {
  pname = "diener";
  version = "0.1.0.0";
  src = fetchgit {
    url = "http://github.com/themoritz/diener";
    sha256 = "1pjf2hbp5qyda725wbra8yrjyzv6v0dnl0iw9idqq04jicz1x313";
    rev = "63ce80f903f654259e5a025535c7f88b3f82933c";
  };
  libraryHaskellDepends = [
    ansi-terminal base bytestring data-default either fast-logger
    monad-control monad-logger mtl time transformers transformers-base
  ];
  homepage = "http://github.com/themoritz/diener#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
