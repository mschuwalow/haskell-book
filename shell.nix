let
  release     = (import ./release.nix {});
  pkgs        = release.pkgs;
  compiler    = release.haskell-book.compiler.name;
  all-hies    = import (builtins.fetchTarball "https://github.com/infinisil/all-hies/tarball/4b6aab017cdf96a90641dc287437685675d598da") {};
  hie         = all-hies.versions."${builtins.replaceStrings [ "-" "." ] [ "" "" ] compiler}";
in
release.pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = release.haskell-book.env.buildInputs ++ [ hie pkgs.cabal-install pkgs.stack ];
}
