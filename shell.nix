# { stdenv, pkgs, haskell }:
with import <nixpkgs> { };

stdenv.mkDerivation rec {
  name = "monadfix-site-tools";
  buildInputs = [
    haskell.compiler.ghc843
    pkgs.git
    pkgs.zlib
    pkgs.cabal-install
    pkgs.pkgconfig
    pkgs.which # for ./new-install.sh
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
  '';
}
