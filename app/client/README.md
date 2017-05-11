# Herculus PureScript Client

## Let nix manage dependencies

Install bower2nix and node2nix:

``` shell
$ nix-env -f '<nixpkgs>' -iA nodePackages.bower2nix nodePackages.node2nix
```

Generate nix files:

``` shell
$ bower2nix bower.json bower-packages.nix
$ node2nix -6 --development --composition /dev/null
```
