#!/bin/bash

set -e

# Client
cd client-react && stack build client && cd ..
cp client-react/.stack-work/dist/x86_64-linux/Cabal-1.22.8.0_ghcjs/build/client/client.jsexe/all.js assets/public/js/main.js
