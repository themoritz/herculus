#!/bin/bash

set -e

cd assets/public/js

cp bundle.js app.js
cat ghcjs.js >> app.js
