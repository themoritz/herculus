{ pkgs ? import <nixpkgs> {}, compiler ? "ghc802" }: 
let
  haskellPackages = pkgs.haskell.packages.${compiler};
  myPackages = pkgs.recurseIntoAttrs (
    haskellPackages.override {
      overrides = self: super: {
        servant-purescript = super.servant-purescript_0_8_0_0;
      };
    }
  );
in
  myPackages.callPackage ./herculus-lib.nix {}

