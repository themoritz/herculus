{ pkgs ? import <nixpkgs> {}, compiler ? "ghcjsHEAD" }: 

let

  ghcjsDedupe = pkgs.haskell.compiler.ghcjsHEAD.overrideDerivation (super: {
    version = "0.2.0-dedupe";
    src = pkgs.fetchFromGitHub {
      owner = "ghcjs";
      repo = "ghcjs";
      rev = "ad0043155a48dbf2f37fe786cca39acc89b96ff7";
      sha256 = "16m5zgjy6kcxjnhrhszy5vg7zj4hjz43jsprpjr1y9wrmp6xcx2j";
    };
  });

  haskellPackages = pkgs.haskell.packages.${compiler};
  myPackages = pkgs.recurseIntoAttrs (
    haskellPackages.override {
      ghc = ghcjsDedupe;
      overrides = self: super: {
        herculus-lib = self.callPackage (import ../lib/herculus-lib.nix) {};
        react-flux = self.callPackage (import ./nix-haskellPackages-fixes/react-flux.nix) {};
        ghcjs-dom = self.callPackage (import ./nix-haskellPackages-fixes/ghcjs-dom.nix) {};
        ghcjs-dom-jsffi = self.callPackage (import ./nix-haskellPackages-fixes/ghcjs-dom-jsffi.nix) {};
        scientific = self.callPackage (import ./nix-haskellPackages-fixes/scientific.nix) {};
        aeson = self.callPackage (import ./nix-haskellPackages-fixes/aeson.nix) {};
        react-flux-servant = self.callPackage (import ./react-flux-servant.nix) {};
      };
    }
  );

in

  myPackages.callPackage ./client.nix { }

