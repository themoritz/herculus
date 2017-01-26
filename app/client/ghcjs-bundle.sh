#!/bin/bash

set -e

JSDIR=public/js
mkdir -p $JSDIR

# Client
stack build herculus-lib
stack build client

cp `stack path --local-install-root`/bin/client.jsexe/all.js $JSDIR/ghcjs.js

if [[ $1 != "--skip-combine" ]]; then
  ./combine-js-app.sh
fi
