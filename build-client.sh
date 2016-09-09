#!/bin/bash

set -e

# Client
cd client && stack build client

cp `stack path --local-install-root`/bin/client.jsexe/all.js ../assets/public/js/ghcjs.js

cd ..

if [[ $1 != "--skip-combine" ]]; then
  ./combine-js-app.sh
fi
