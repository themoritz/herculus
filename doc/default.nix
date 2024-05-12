{ pkgs ? import <nixpkgs> {}
, herculus-lib ? import ./../app/lib { inherit pkgs; }
}:

with pkgs.python3Packages;

let

  mkdocs-cinder = buildPythonPackage rec {
    pname = "mkdocs-cinder";
    version = "1.2.0";

    src = fetchPypi {
      inherit pname version;
      sha256 = "1li5l3q25a47fg6da1r7rq8gxv2j1dvjkw1vam3q719dg50fj8rf";
    };
  };

in

  pkgs.stdenv.mkDerivation {
    name = "herculus-doc";
    src = ./.;
    buildInputs = [
      pkgs.python3
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
