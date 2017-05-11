{ pkgs ? import <nixpkgs> {}
, herculus-lib ? import ../lib {}
, apiUrl ? "http://localhost:3000/api/"
, websocketUrl ? "ws://localhost:3000/websocket"
}:

with pkgs;

let

  nodeEnv = import ./node-env.nix {
    inherit (pkgs) stdenv python2 utillinux runCommand writeTextFile;
    inherit nodejs;
  };

  nodeDeps = import ./node-packages.nix {
    inherit (pkgs) fetchurl fetchgit;
    inherit nodeEnv;
  };

  bowerDeps = buildBowerComponents {
    name = "herculus-purescript-client";
    generated = ./bower-packages.nix;
    src = {
      outPath = ./.;
      name = "herculus-purescript-client";
    };
  };

in

stdenv.mkDerivation {

  name = "herculus-purescript-client";
  src = ./.;

  buildInputs = [
    git
    zopfli
    nodejs
    nodePackages.bower
    herculus-lib
  ];

  buildPhase = ''
    HOME=.
    rm -rf bower_components node_modules output .pulp-cache .psci-modules
    ln -s ${bowerDeps}/bower_components .
    cp -r --no-preserve=mode ${nodeDeps.package}/lib/node_modules/herculus-purescript-client/node_modules .
    chmod -R a+x .
    ln -s -f ${purescript}/bin/purs ./node_modules/purescript/vendor
    psc-gen --target "./src"
    API_URL="${apiUrl}" \
      WEBSOCKET_URL="${websocketUrl}" \
      npm run build-prod
    zopfli --i15 public/bundle-*.js
  '';

  installPhase = ''
    mkdir -p $out
    cp -r ./public/* $out
  '';

}
