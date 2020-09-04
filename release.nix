{ nixpkgs ? import ./nixpkgs.nix }:
let
  # default compiler
  compiler = "ghc882";

  # Disable tests for these packages
  dontCheckPackages = [ "network" "streaming-commons" ];

  # Jailbreak these packages
  doJailbreakPackages = [ ];

  # Disable haddocks for these packages
  dontHaddockPackages = [ ];

  mkOverlay = compiler: self: super: {
    myHaskellPackages = let
      generatedOverrides = haskellPackagesNew: haskellPackagesOld:
        let
          toPackage = file: _: {
            name = builtins.replaceStrings [ ".nix" ] [ "" ] file;
            value = haskellPackagesNew.callPackage (./. + "/nix/${file}") { };
          };
          source = super.nix-gitignore.gitignoreSource [ ./.gitignore ] ./.;
        in super.lib.mapAttrs' toPackage (builtins.readDir "${source}/nix");

      makeOverrides = function: names: haskellPackagesNew: haskellPackagesOld:
        let
          toPackage = name: {
            inherit name;
            value = function haskellPackagesOld.${name};
          };
        in builtins.listToAttrs (map toPackage names);

      # More exotic overrides go here
      manualOverrides = haskellPackagesNew: haskellPackagesOld: { };

    in super.haskell.packages.${compiler}.override {
      overrides = super.lib.foldr super.lib.composeExtensions (_: _: { }) [
        generatedOverrides
        (makeOverrides pkgs.haskell.lib.dontCheck dontCheckPackages)
        (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
        (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
        manualOverrides
      ];
    };
  };
  overlays = [ (mkOverlay compiler) ];
  pkgs = nixpkgs { inherit overlays; };
in {
  inherit compiler overlays pkgs;
  haskell-book = pkgs.myHaskellPackages.haskell-book;
}
