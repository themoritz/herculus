{ pkgs ? import <nixpkgs> {}, compiler ? "ghc801" }: 
let
  haskellPackages = pkgs.haskell.packages.${compiler};
  myPackages = pkgs.recurseIntoAttrs (
    haskellPackages.override {
      overrides = self: super: {
        herculus-lib = self.callPackage (import ../lib) {};
      };
    }
  );
in
  myPackages.callPackage ./server.nix { }

