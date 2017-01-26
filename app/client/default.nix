{ pkgs ? import <nixpkgs> {}, apiUrl, websocketUrl }:

let
  ghcjs-bundle = import ./ghcjs-bundle.nix { inherit pkgs; };

in

  with pkgs;
  
  stdenv.mkDerivation {
    name = "herculus-client";
    src = ./.;
    buildInputs = [ ghcjs-bundle git zopfli nodejs ];
    buildPhase = ''
      HOME=.
      npm install --no-optional
      echo "require('${ghcjs-bundle}/bin/client.jsexe/all.js');" >> entry.js
      API_URL="${apiUrl}" \
        WEBSOCKET_URL="${websocketUrl}" \
        GIT_REV=`git rev-parse --short HEAD` \
        `npm bin`/webpack --config webpack.prod.config.js --progress
      cd public/js && mv bundle.js app.js
      zopfli --i15 public/js/app.js
    '';
    installPhase = ''
      mkdir -p $out
      cp -r ./public/* $out
    '';
  }
