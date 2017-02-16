{ pkgs ? import <nixpkgs> {}, compiler ? "ghc802" }: 
let
  haskellPackages = pkgs.haskell.packages.${compiler};
in
  haskellPackages.callPackage ./herculus-lib.nix {}

