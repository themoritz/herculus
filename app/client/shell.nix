{ pkgs ? import <nixpkgs> {}, compiler ? "ghcjsHEAD" }:

(import ./ghcjs-bundle.nix { inherit pkgs compiler; }).env
