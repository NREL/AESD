{
  nixpkgs  ? null
, compiler ? "ghc822"
}:

(import ../shell.nix) {package = "aesd-records"; nixpkgs = nixpkgs; compiler = compiler;}
