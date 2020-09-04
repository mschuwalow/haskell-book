let
  release = (import ./release.nix { });
  pkgs = release.pkgs;
  all-hies = import (builtins.fetchTarball
    "https://github.com/infinisil/all-hies/tarball/4b6aab017cdf96a90641dc287437685675d598da")
    { };
  hie = all-hies.versions."${builtins.replaceStrings [ "-" "." ] [ "" "" ]
    release.compiler}";
in pkgs.myHaskellPackages.shellFor {
  packages = pkgs: with pkgs; [ haskell-book ];
  withHoogle = true;
  nativeBuildInputs =
    [ hie pkgs.cabal-install pkgs.stack pkgs.nixfmt pkgs.ormolu pkgs.gnumake ];
}
