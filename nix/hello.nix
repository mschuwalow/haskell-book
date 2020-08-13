{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "hello";
  version = "0.0.0.1";
  src = ../hello;
  isLibrary = false;
  isExecutable = true;
  license = stdenv.lib.licenses.asl20;
  libraryHaskellDepends = [ base ];
}
