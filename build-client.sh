#!/bin/bash

set -e

# Client
cd client && stack build client

cp `stack path --local-install-root`/bin/client.jsexe/all.js ../assets/public/js/app.js

cd ..
