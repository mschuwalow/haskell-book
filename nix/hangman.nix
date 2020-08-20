{ mkDerivation, stdenv, base, split, random, hspec, QuickCheck }:
mkDerivation {
  pname = "hangman";
  version = "0.1.0.0";
  src = ../hangman;
  license = stdenv.lib.licenses.asl20;
  isExecutable = true;
  libraryHaskellDepends = [ base split random hspec QuickCheck ];
}
