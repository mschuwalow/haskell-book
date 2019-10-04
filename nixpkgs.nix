{ fetchgit }:
let
  rev = "141b7210cb0f4684d8c88f4c870cf1e6663c47a2";
  sha256 = "0vg3rjxf1w0vgxnmpg5p80adgdcjv4f23ggsv1aakq03ga36y294";
in
import (
  fetchgit {
    inherit rev;
    inherit sha256;
    url = "https://github.com/NixOS/nixpkgs";
  }
)
