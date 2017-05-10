{ pkgs ? import <nixpkgs> {}, compiler ? "ghc802" }: 
let
  haskellPackages = pkgs.haskell.packages.${compiler};
  myPackages = pkgs.recurseIntoAttrs (
    haskellPackages.override {
      overrides = self: super: {
        servant-purescript = self.callPackage ./nix/servant-purescript.nix {};
      };
    }
  );
in
  myPackages.callPackage ./herculus-lib.nix {}

