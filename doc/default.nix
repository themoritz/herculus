{ pkgs ? import <nixpkgs> {} }:

with pkgs.python27Packages; 

let

  herculus-lib = import ./../app/lib { inherit pkgs; };

  livereload = buildPythonPackage rec {
    name = "livereload-2.5.1";
    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/e9/2e/c4972828cf526a2e5f5571d647fb2740df68f17e8084a9a1092f4d209f4c/${name}.tar.gz";
      sha256 = "0b2yyfnpddmrwjfqsndidzchkf3l9jlgzfkwl8dplim9gq6y2ba2";
    };
    buildInputs = [
      tornado
    ];
  };

  mkdocs = buildPythonPackage rec {
    name = "mkdocs-0.16.1";
    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/1a/52/b168d0ac9e60f587b97b1a743b08ed74ffed2d39f7ecd46d3eaa404445bd/${name}.tar.gz";
      sha256 = "1gsjmmm5psc3hrxc7cl77da6cb69fm175zdyrr65m8fa6gg12q8n";
    };
    buildInputs = [
      mock
    ];
    propagatedBuildInputs = [
      click
      markdown
      pyyaml
      jinja2
      tornado
      livereload
    ];
  };

  mkdocs-cinder = buildPythonPackage rec {
    name = "mkdocs-cinder-0.9.4";
    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/f9/16/6998e0aae828ab4ee48927bad487f5e82dfc4f0d7a4e4922faf4275fa870/${name}.tar.gz";
      sha256 = "0bkd1rixb83ki9mwsw2r7aj241blzb9ma5v7s0jz0bzh8sh1asz7";
    };
    doCheck = false;
  };

in

  pkgs.stdenv.mkDerivation {
    name = "herculus-doc";
    src = ./.;
    buildInputs = [
      pkgs.python35
      mkdocs
      mkdocs-cinder
      herculus-lib
    ];
    buildPhase = ''
      doc-gen -p > docs/reference.md
      mkdocs build
    '';
    installPhase = "mkdir $out && cp -r ./site/* $out";
  }
