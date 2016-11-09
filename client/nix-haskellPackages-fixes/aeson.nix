{ mkDerivation, attoparsec, base, base-orphans, bytestring
, containers, deepseq, dlist, fail, ghc-prim, hashable, HUnit, mtl
, nats, QuickCheck, quickcheck-instances, scientific, semigroups
, stdenv, syb, tagged, template-haskell, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, time
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "aeson";
  version = "0.11.2.1";
  sha256 = "0k5p06pik7iyjm1jjkjbpqqn0mqps6b8mz9p9sp9hmganl4cffyc";
  revision = "1";
  editedCabalFile = "e97fac43eddd037bf21752ea10150a224b9c08d267f634ea54f799023a6c5e13";
  libraryHaskellDepends = [
    attoparsec base bytestring containers deepseq dlist fail ghc-prim
    hashable mtl nats scientific semigroups syb tagged template-haskell
    text time transformers unordered-containers vector
  ];
  testHaskellDepends = [
    attoparsec base base-orphans bytestring containers ghc-prim
    hashable HUnit nats QuickCheck quickcheck-instances semigroups
    tagged template-haskell test-framework test-framework-hunit
    test-framework-quickcheck2 text time unordered-containers vector
  ];
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
}
