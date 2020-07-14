{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "haskell-book";
  version = "0.0.0.1";
  src = ../haskell-book;
  # isLibrary = true;
  # isExecutable = false;
  license = stdenv.lib.licenses.asl20;
  libraryHaskellDepends = [ base ];
}
