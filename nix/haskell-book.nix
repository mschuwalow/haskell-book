{ mkDerivation, stdenv, base, either, hspec, QuickCheck, checkers, trifecta
, raw-strings-qq, parsec, attoparsec, aeson, ghc-prim }:
mkDerivation {
  pname = "haskell-book";
  version = "0.0.0.1";
  src = ../haskell-book;
  license = stdenv.lib.licenses.asl20;
  isExecutable = false;
  isLibrary = true;
  libraryHaskellDepends = [
    base
    either
    hspec
    QuickCheck
    checkers
    trifecta
    raw-strings-qq
    parsec
    attoparsec
    aeson
    ghc-prim
  ];
}
