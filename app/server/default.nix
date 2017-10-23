{ pkgs ? import <nixpkgs> {}, compiler ? "ghc802" }: 

let

  haskellPackages = pkgs.haskell.packages.${compiler};
  myPackages = pkgs.recurseIntoAttrs (
    haskellPackages.override {
      overrides = self: super: {
        herculus-lib = self.callPackage ../lib/herculus-lib.nix {};
        servant-purescript = self.callPackage ../lib/nix/servant-purescript.nix {};
      };
    }
  );
  server = myPackages.callPackage ./server.nix { };
  latex = pkgs.texlive.combine { inherit (pkgs.texlive)
    scheme-full
    lato
    slantsc
    titlesec
    enumitem
    lastpage
    collection-langgerman
    collection-fontsrecommended
    collection-latexrecommended;
  };

in

  server.overrideDerivation (super: {
    buildInputs = super.buildInputs ++ [ pkgs.makeWrapper ];
    postInstall = ''
      wrapProgram "$out/bin/server-exe" --suffix PATH : ${latex}/bin \
                                        --suffix PATH : ${pkgs.ssmtp}/sbin
    '';
  })
