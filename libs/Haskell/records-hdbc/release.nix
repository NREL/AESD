{ compiler ? "ghc7103" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              cesds-records-hdbc =
                haskellPackagesNew.callPackage ./default.nix { };
              cesds-records =
                haskellPackagesNew.callPackage ../records/default.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  {
   cesds-records-hdbc = pkgs.haskell.packages.${compiler}.cesds-records-hdbc;
  }
