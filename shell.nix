{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base-prelude, bytestring, containers
      , exceptions, feed, filepath, lens, lucid, mmark, stdenv
      , system-filepath, text, time, turtle, unordered-containers, xml
      , xml-conduit, xml-types, yaml
      }:
      mkDerivation {
        pname = "dch-qa";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base base-prelude bytestring containers exceptions feed filepath
          lens lucid mmark system-filepath text time turtle
          unordered-containers xml xml-conduit xml-types yaml
        ];
        license = stdenv.lib.licenses.unfree;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
