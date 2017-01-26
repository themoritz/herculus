#!/bin/bash

set -e

JSDIR=public/js

mkdir -p $JSDIR
cd $JSDIR

cp bundle.js app.js
cat ghcjs.js >> app.js
