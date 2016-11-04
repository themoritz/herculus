{ pkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:

(import ./default.nix { inherit pkgs compiler; }).env
