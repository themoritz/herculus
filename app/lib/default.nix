{ pkgs ? import <nixpkgs> {}, compiler ? "ghc801" }: 
let
  haskellPackages = pkgs.haskell.packages.${compiler};
in
  haskellPackages.callPackage ./herculus-lib.nix {}

