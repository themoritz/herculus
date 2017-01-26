{ pkgs ? import <nixpkgs> {}, compiler ? "ghcjsHEAD" }: 

let

  haskellPackages = pkgs.haskell.packages.${compiler};
  myPackages = pkgs.recurseIntoAttrs (
    haskellPackages.override {
      ghc = import ./nix-ghcjs { inherit pkgs; };
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

