{
  nixpkgs  ? null
, compiler ? "ghc822"
}:

(import ../shell.nix) {package = "aesd-records-hdbc"; nixpkgs = nixpkgs; compiler = compiler;}
