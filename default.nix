{ pkgs ? import <nixpkgs> {}, apiUrl, websocketUrl }:

let
  client = import ./client { inherit pkgs; };

in

  with pkgs;
  
  stdenv.mkDerivation {
    name = "herculus";
    src = ./.;
    buildInputs = [ client git zopfli nodejs ];
    buildPhase = ''
      HOME=.
      npm install --no-optional
      cp entry.dev.js entry.prod.js
      echo "require('${client}/bin/client.jsexe/all.js');" >> entry.prod.js
      API_URL="${apiUrl}" \
        WEBSOCKET_URL="${websocketUrl}" \
        GIT_REV=`git rev-parse --short HEAD` \
        node_modules/.bin/webpack --config webpack.prod.config.js --progress --colors
    '';
    installPhase = "mkdir $out && cp -r ./assets/public/* $out";
  }
