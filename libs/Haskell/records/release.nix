{ compiler ? "ghc7103" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              cesds-records =
                haskellPackagesNew.callPackage ./default.nix { };
              raft =
                haskellPackagesNew.callPackage /home/bbush/Projects/Deploy/raft/default.nix {};
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  {
   cesds-records = pkgs.haskell.packages.${compiler}.cesds-records;
  }
