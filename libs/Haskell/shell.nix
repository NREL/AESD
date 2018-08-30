{ nixpkgs ? import <nixpkgs>, compiler ? "ghc7103" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              aesd-records =
                haskellPackagesNew.callPackage ./records/default.nix { };
              aesd-records-hdbc =
                haskellPackagesNew.callPackage ./records-hdbc/default.nix { };
              aesd-records-haystack =
                haskellPackagesNew.callPackage ./records-haystack/default.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = nixpkgs { inherit config; };

  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (self: [
    self.aesd-records
    self.aesd-records-hdbc
    self.aesd-records-haystack
  ]);

in

  with pkgs;
  stdenv.mkDerivation rec {
    name = "aesd-env";
    env = buildEnv {
      inherit name;
      paths = buildInputs;
    };
    buildInputs = with haskell.packages.${compiler}; [
      standalone-haddock
      ghc
    ];
    shellHook = ''
      standalone-haddock -o ../../docs/haskell records records-hdbc records-haystack
    '';
  }
