{ pkgs, compiler }:
let
  haskellPackages = pkgs.haskell.packages.${compiler};
in
  pkgs.recurseIntoAttrs (
    haskellPackages.override {
      overrides = self: super: {
        union-find = self.callHackageDirect {
          pkg = "union-find";
          ver = "0.1";
          sha256 = "0zxcqwgqgbab41smr2kiaw5xz22w8gbcp3n73pyal93jld1br0qh";
        } {};
        purescript-bridge = self.callHackageDirect {
          pkg = "purescript-bridge";
          ver = "0.11.1.2";
          sha256 = "1hwcn3hdj2w1nvm59qn0a57kyjr7abnsnl48ksx4ancpfagz1r64";
        } {};
        servant-purescript = self.callCabal2nix "servant-purescript" (pkgs.fetchFromGitHub {
          owner = "themoritz";
          repo = "servant-purescript";
          rev = "9ac42eb9b20782dd43dea95e3da826c83f97e057";
          sha256 = "0mgbcr2vx6dk1i0f9j98h58w3ngv50c9d4l4rc86jk5h838vsd30";
        }) {};
        servant-subscriber = self.callCabal2nix "servant-subscriber" (pkgs.fetchFromGitHub {
          owner = "themoritz";
          repo = "servant-subscriber";
          rev = "ba65d36a940030efc5a7f155ebcb43f2464fed66";
          sha256 = "10n02b58z3dcgj3lpvb8s3id5a6y73if5nay1jlc1y3sxwpmvcag";
        }) {};
        herculus-lib = self.callPackage ./lib/herculus-lib.nix {};
        herculus-server = self.callPackage ./server/server.nix {};
      };
    }
  )
