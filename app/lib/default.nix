{ pkgs ? import <nixpkgs> {}, compiler ? "ghc947" }:

let
  haskellPackages = import ../haskell-packages.nix { inherit pkgs compiler; };

in
  haskellPackages.herculus-lib
