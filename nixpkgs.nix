let
  rev = "bd6ba87381ef6274a830253b60e481209d4d7c7d";
  sha256 = "1hm53rpdz8a9jr3klm93wab6z32ryv4q65kkpmc89cikcipqcrcm";
in
import (
  builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }
)
