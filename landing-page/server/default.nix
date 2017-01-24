{ pkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
let
  haskellPackages = pkgs.haskell.packages.${compiler};
  myPackages = pkgs.recurseIntoAttrs(
    haskellPackages.override {
      overrides = self: super: {
        diener = self.callPackage (import ./diener.nix) {};
      };
    }
  );
in
  myPackages.callPackage ./subscribers.nix { }
