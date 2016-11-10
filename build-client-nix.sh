set -e

JSDIR=assets/public/js
mkdir -p $JSDIR

# Client. If lib or other dependencies have changed, need to rerun
# `nix-shell --command 'cabal configure --ghcjs'`
cd client && \
    cabal build

cp dist/build/client/client.jsexe/all.js ../$JSDIR/ghcjs.js

cd ..

if [[ $1 != "--skip-combine" ]]; then
  bash ./combine-js-app.sh
fi
