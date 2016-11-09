{ pkgs ? import <nixpkgs> {}, compiler ? "ghcjsHEAD" }:

(import ./default.nix { inherit pkgs compiler; }).env
