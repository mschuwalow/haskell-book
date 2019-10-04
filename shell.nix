let
  release     = (import ./release.nix {});
  compiler    = release.haskell-book.name;
  all-hies    = import (builtins.fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  hie         = all-hies.versions."${builtins.replaceStrings [ "-" "." ] [ "" "" ] compiler}";
in
release.pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = release.haskell-book.env.buildInputs ++ [ hie ];
}
