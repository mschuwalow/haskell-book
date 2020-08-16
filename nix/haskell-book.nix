{ mkDerivation, stdenv, base, either }:
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
  ];
}
