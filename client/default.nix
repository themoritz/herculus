{ pkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }: 
let
  haskellPackages = pkgs.haskell.packages.${compiler};
  myPackages = pkgs.recurseIntoAttrs (
    haskellPackages.override {
      overrides = self: super: {
        herculus-lib = self.callPackage (import ../lib) {};
        react-flux = self.callPackage (import ./react-flux.nix) {};
      };
    }
  );
in
  myPackages.callPackage ./client.nix { }

