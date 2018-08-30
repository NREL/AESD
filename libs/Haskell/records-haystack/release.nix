{ compiler ? "ghc7103" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              aesd-records-haystack =
                haskellPackagesNew.callPackage ./default.nix { };
              aesd-records =
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
   aesd-records-haystack = pkgs.haskell.packages.${compiler}.aesd-records-haystack;
  }
