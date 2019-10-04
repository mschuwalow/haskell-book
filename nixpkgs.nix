{ fetchgit }:
let
  rev = "bd6ba87381ef6274a830253b60e481209d4d7c7d";
  sha256 = "0vg3rjxf1w0vgxnmpg5p80adgdcjv4f23ggsv1aakq03ga36y294";
in
import (
  fetchgit {
    inherit rev;
    inherit sha256;
    url = "https://github.com/NixOS/nixpkgs";
  }
)
