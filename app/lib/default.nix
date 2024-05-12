{ pkgs ? import <nixpkgs> {}, compiler ? "ghc947" }:
let
  haskellPackages = pkgs.haskell.packages.${compiler};
  myPackages = pkgs.recurseIntoAttrs (
    haskellPackages.override {
      overrides = self: super: {
        union-find = self.callHackageDirect {
          pkg = "union-find";
          ver = "0.1";
          sha256 = "0zxcqwgqgbab41smr2kiaw5xz22w8gbcp3n73pyal93jld1br0qh";
        } {};
        servant-purescript = self.callCabal2nix "servant-purescript" (pkgs.fetchFromGitHub {
          owner = "themoritz";
          repo = "servant-purescript";
          rev = "05e214acfea4861975f2a8f9f1c71fb42e337155";
          sha256 = "1g04z8rxknlh6r4anlsj1cga6fa044p0m3i43snpkxpw62a37q3m";
        }) {};
        servant-subscriber = self.callCabal2nix "servant-subscriber" (pkgs.fetchFromGitHub {
          owner = "themoritz";
          repo = "servant-subscriber";
          rev = "ba65d36a940030efc5a7f155ebcb43f2464fed66";
          sha256 = "10n02b58z3dcgj3lpvb8s3id5a6y73if5nay1jlc1y3sxwpmvcag";
        }) {};
      };
    }
  );
in
  myPackages.callPackage ./herculus-lib.nix {}
