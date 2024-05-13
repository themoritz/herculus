{ pkgs ? import <nixpkgs> {} }:

# Assumes that the client has been built and bundled manually.
pkgs.stdenv.mkDerivation {

  name = "herculus-purescript-client";
  src = ./public/.;

  buildInputs = [
    pkgs.zopfli
  ];

  buildPhase = ''
    zopfli --i15 bundle-*.js
  '';

  installPhase = ''
    mkdir -p $out
    cp -r ./* $out
  '';

}
