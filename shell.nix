let
  release     = import ./release.nix {};
  pkgs        = release.pkgs;
  all-hies    = import (builtins.fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  hie         = all-hies.versions."${builtins.replaceStrings [ "-" "." ] [ "" "" ] pkgs.ghc.name}";
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = [
    release.haskell-book
    hie
  ];
}
