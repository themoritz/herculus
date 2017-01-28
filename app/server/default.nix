{ pkgs ? import <nixpkgs> {}, compiler ? "ghc801" }: 

let

  haskellPackages = pkgs.haskell.packages.${compiler};
  myPackages = pkgs.recurseIntoAttrs (
    haskellPackages.override {
      overrides = self: super: {
        herculus-lib = self.callPackage (import ../lib/herculus-lib.nix) {};
      };
    }
  );
  server = myPackages.callPackage ./server.nix { };
  latex = pkgs.texlive.combine { inherit (pkgs.texlive)
    scheme-basic
    collection-fontsrecommended
    collection-latexrecommended
    lato
    slantsc;
  };

in

  server.overrideDerivation (super: {
    buildInputs = super.buildInputs ++ [ pkgs.makeWrapper latex ];
    postInstall = ''
      wrapProgram "$out/bin/server-exe" --suffix PATH : ${latex}/bin
    '';
  })