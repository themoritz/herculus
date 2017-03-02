{ pkgs ? import <nixpkgs> {}
, herculus-lib ? import ../lib {}
, apiUrl ? "http://localhost:3000/api/"
, websocketUrl ? "ws://localhost:3000/websocket"
}:

with pkgs;

stdenv.mkDerivation {
  name = "herculus-client";
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
    npm install --no-optional
    # bower install
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
