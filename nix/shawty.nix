{ mkDerivation, stdenv, base, bytestring, hedis, mtl, network-uri, random
, scotty, semigroups, text, transformers }:
mkDerivation {
  pname = "shawty";
  version = "0.1.0.0";
  src = ../shawty;
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
