{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "herculus-landing-page";
  src = ./.;
  buildInputs = [ jekyll nodejs ];
  buildPhase = "jekyll build --config _config_prod.yml";
  installPhase = "mkdir $out && cp -r _site/* $out";
}
