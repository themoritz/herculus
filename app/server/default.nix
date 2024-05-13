{ pkgs ? import <nixpkgs> {}, compiler ? "ghc947" }:

let
  haskellPackages = import ../haskell-packages.nix { inherit pkgs compiler; };

  latex = pkgs.texlive.combine { inherit (pkgs.texlive)
    # scheme-full
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
  haskellPackages.herculus-server.overrideDerivation (super: {
    buildInputs = super.buildInputs ++ [ pkgs.makeWrapper ];
    postInstall = ''
      wrapProgram "$out/bin/server-exe" --suffix PATH : ${latex}/bin \
                                        --suffix PATH : ${pkgs.msmtp}/sbin
    '';
  })
