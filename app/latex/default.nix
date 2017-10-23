{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "herculus-latex";
  src = ./.;
  installPhase = ''
    mkdir $out
    rm -rf default.nix result auto
    cp -r ./* $out
  '';
}
