set -e

TARGET=assets/public/doc
mkdir -p $TARGET

cd doc && \
    nix-shell --command 'mkdocs build'

cp -r site/* ../$TARGET

cd ..
