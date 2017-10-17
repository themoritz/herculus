{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "herculus-latex";
  src = ./.;
  installPhase = ''
    mkdir $out
    rm -f default.nix result auto
    cp -r ./* $out
  '';
}
