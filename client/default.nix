{ pkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }: 
let
  haskellPackages = pkgs.haskell.packages.${compiler};
  myPackages = pkgs.recurseIntoAttrs (
    haskellPackages.override {
      overrides = self: super: 
        let 
          ghcjs-dom-jsffi = self.callPackage (import ./nix-haskellPackages-fixes/ghcjs-dom-jsffi.nix) {};
        in {
          herculus-lib = self.callPackage (import ../lib) {};
          react-flux = self.callPackage (import ./nix-haskellPackages-fixes/react-flux.nix) {};
          ghcjs-dom = self.callPackage (import ./nix-haskellPackages-fixes/ghcjs-dom.nix) {};
          ghcjs-dom-jsffi = pkgs.haskell.lib.overrideCabal ghcjs-dom-jsffi (drv: {
            doCheck = false;
            doHaddock = false;
            postPatch = ''
              sed -i 's|1\.24|1\.22|' ghcjs-dom-jsffi.cabal
            '';
          });
        };
    }
  );
in
  myPackages.callPackage ./client.nix { }

