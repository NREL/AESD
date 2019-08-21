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
              aesd-records-haystack = haskellPackagesNew.callPackage                              ./default.nix { };
              aesd-records          = haskellPackagesNew.callPackage                     ../records/default.nix { };
              raft                  = haskellPackagesNew.callPackage               ../../../../raft/default.nix { };
              daft                  = haskellPackagesNew.callPackage   ../../../../daft-public/daft/default.nix { };
              vinyl                 = haskellPackagesNew.callPackage     ../../../../daft-public/daft/vinyl.nix { };
              type-list             = haskellPackagesNew.callPackage ../../../../daft-public/daft/type-list.nix { };
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
