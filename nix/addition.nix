{ mkDerivation, stdenv, base, hspec, QuickCheck }:
mkDerivation {
  pname = "addition";
  version = "0.1.0.0";
  src = ../addition;
  license = stdenv.lib.licenses.asl20;
  isExecutable = true;
  libraryHaskellDepends = [ base hspec QuickCheck ];
}
