{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "haskell-book";
  version = "0.0.0.1";
  src = ./haskell-book;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.asl20;
}
