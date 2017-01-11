{ pkgs ? import <nixpkgs> {} }:

pkgs.haskell.compiler.ghcjsHEAD.overrideDerivation (super: {
  version = "0.2.0-dedupe";
  src = pkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs";
    rev = "ad0043155a48dbf2f37fe786cca39acc89b96ff7";
    sha256 = "16m5zgjy6kcxjnhrhszy5vg7zj4hjz43jsprpjr1y9wrmp6xcx2j";
  };
})
