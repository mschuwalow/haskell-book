{ mkDerivation, stdenv, base, bytestring, hedis, mtl, network-uri, random
, scotty, semigroups, text, transformers }:
mkDerivation {
  pname = "hit-counter";
  version = "0.1.0.0";
  src = ../hit-counter;
  license = stdenv.lib.licenses.asl20;
  isExecutable = true;
  executableHaskellDepends = [
    base
    bytestring
    hedis
    mtl
    network-uri
    random
    scotty
    semigroups
    text
    transformers
  ];
}
