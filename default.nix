{ pkgs ? import <nixpkgs> {} }:

let
  client = import ./client { inherit pkgs; };

in

  with pkgs;
  
  stdenv.mkDerivation {
    name = "herculus";
    src = ./.;
    buildInputs = [ client zopfli nodejs ];
    buildPhase = ''
      export HOME=.
      npm install --no-optional
      cp entry.dev.js entry.prod.js
      echo "require('${client}/bin/client.jsexe/all.js');" >> entry.prod.js
      node_modules/.bin/webpack --config webpack.prod.config.js --progress --colors
    '';
    installPhase = "mkdir $out && cp -r ./assets/public/* $out";
  }
