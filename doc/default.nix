{ pkgs ? import <nixpkgs> {} }:

with pkgs.python27Packages; 

let

  livereload = buildPythonPackage rec {
    name = "livereload-2.5.1";
    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/e9/2e/c4972828cf526a2e5f5571d647fb2740df68f17e8084a9a1092f4d209f4c/${name}.tar.gz";
      md5 = "7a54fef366b45891652e838f97c52c3b";
    };
    buildInputs = [
      tornado
    ];
  };

  mkdocs = buildPythonPackage rec {
    name = "mkdocs-0.16.1";
    src = pkgs.fetchurl {
      url = "https://pypi.python.org/packages/1a/52/b168d0ac9e60f587b97b1a743b08ed74ffed2d39f7ecd46d3eaa404445bd/${name}.tar.gz";
      md5 = "4aeaa2ef50a653242b8962ea558bc584";
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
      md5 = "17e7cc53d4ed3a5b99422ff076eed482";
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
    ];
    buildPhase = ''
      mkdocs build
    '';
    installPhase = "mkdir $out && cp -r ./site/* $out";
  }
