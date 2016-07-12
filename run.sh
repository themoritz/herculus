#!/bin/bash

set -e

# Client
cd client && stack build client && cd ..
cp client/.stack-work/dist/x86_64-linux/Cabal-1.22.8.0_ghcjs/build/client/client.jsexe/all.js assets/public/js/main.js

# Server
cd server
stack build server
stack exec server-exe
