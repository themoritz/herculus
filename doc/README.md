# Herculus Documentation

Before developing or deploying, always generate the documentation of the
Hexl prelude using the following command:

``` shell
cd ../app/ && cabal run lib:doc-gen -- -p > ../doc/docs/reference.md && cd -
```

## Developing

``` shell
nix-shell --command 'mkdocs serve'
```

You can then visit http://127.0.0.1:8000/ with your browser. Changes to the documentation will be automatically reflected.

## Deployment

``` shell
nix-shell --command 'mkdocs build'

scp -r site user@host:/root/code/herculus/doc
```
