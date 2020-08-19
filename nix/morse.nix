{ mkDerivation, stdenv, base, QuickCheck }:
mkDerivation {
  pname = "morse";
  version = "0.1.0.0";
  src = ../morse;
  license = stdenv.lib.licenses.asl20;
  isExecutable = true;
  executableHaskellDepends = [
    base
    QuickCheck
  ];
}
