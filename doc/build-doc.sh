set -e

TARGET=../app/client/public/doc
mkdir -p $TARGET

nix-shell --command 'mkdocs build'

cp -r site/* ../$TARGET
