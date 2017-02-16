{ pkgs ? import <nixpkgs> {}, compiler ? "ghc802" }: 

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
    collection-langgerman
    collection-fontsrecommended
    collection-latexrecommended
    lato
    slantsc;
  };

in

  server.overrideDerivation (super: {
    buildInputs = super.buildInputs ++ [ pkgs.makeWrapper ];
    postInstall = ''
      wrapProgram "$out/bin/server-exe" --suffix PATH : ${latex}/bin \
                                        --suffix PATH : ${pkgs.ssmtp}/sbin
    '';
  })
