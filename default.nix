{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  name = "herculus";
  src = ./.;
  buildInputs = [ python nodejs ];
  buildPhase = "npm install --no-optional";
  installPhase = "";
}
