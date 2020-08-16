{ mkDerivation, stdenv, base }:
mkDerivation {
  pname = "hello";
  version = "0.1.0.0";
  src = ../hello;
  license = stdenv.lib.licenses.asl20;
  isExecutable = true;
  executableHaskellDepends = [
    base
  ];
}
