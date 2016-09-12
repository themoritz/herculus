#!/bin/bash

set -e

JSDIR=assets/public/js
mkdir -p $JSDIR

# Client
cd client && stack build client

cp `stack path --local-install-root`/bin/client.jsexe/all.js ../$JSDIR/ghcjs.js

cd ..

if [[ $1 != "--skip-combine" ]]; then
  ./combine-js-app.sh
fi
