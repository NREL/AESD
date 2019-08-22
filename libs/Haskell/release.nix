{
  nixpkgs  ? import <nixpkgs>
, compiler ? "ghc822"
}:

let
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              aesd-records          = haskellPackagesNew.callPackage          ./records/default.nix { };
              aesd-records-haystack = haskellPackagesNew.callPackage ./records-haystack/default.nix { };
              aesd-records-hdbc     = haskellPackagesNew.callPackage     ./records-hdbc/default.nix { };
              bson-generic          = haskellPackagesNew.callPackage             ./bson-generic.nix { };
              daft                  = haskellPackagesNew.callPackage                     ./daft.nix { };
              raft                  = haskellPackagesNew.callPackage                     ./raft.nix { };
              type-list             = haskellPackagesNew.callPackage                ./type-list.nix { };
              vinyl                 = haskellPackagesNew.callPackage                    ./vinyl.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  {
    aesd-records          = pkgs.haskell.packages.${compiler}.aesd-records         ;
    aesd-records-haystack = pkgs.haskell.packages.${compiler}.aesd-records-haystack;
    aesd-records-hdbc     = pkgs.haskell.packages.${compiler}.aesd-records-hdbc    ;
  }
