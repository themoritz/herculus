set -e

# Server. If lib or other dependencies have changed, need to rerun
# `nix-shell --command 'cabal configure'`
cd server
cabal build
dist/build/server-exe/server-exe
