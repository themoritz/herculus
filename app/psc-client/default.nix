{ pkgs ? import <nixpkgs> {}
, herculus-lib ? import ../lib {}
, apiUrl ? "/"
, websocketUrl ? "/"
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
    bower install
    psc-gen --target "./src"
    # GIT_REV=`git rev-parse --short HEAD` \
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
