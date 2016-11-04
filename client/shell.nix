{ pkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

(import ./default.nix { inherit pkgs compiler; }).env
