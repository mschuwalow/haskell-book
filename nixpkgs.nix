let
  rev = "e34208e10033315fddf6909d3ff68e2d3cf48a23";
  sha256 = "0ngkx5ny7bschmiwc5q9yza8fdwlc3zg47avsywwp8yn96k2cpmg";
in import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/tarball/${rev}";
  inherit sha256;
})
