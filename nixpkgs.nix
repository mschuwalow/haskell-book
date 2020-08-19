let
  rev = "3506bce1d405f078c09a5201e3bb46eb64d5e9e7";
  sha256 = "1hhdcyd0fsn97z2xhz5676rhs4rjcy7phw0mmgjqamw7mg6mb6g4";
in import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/tarball/${rev}";
  inherit sha256;
})
