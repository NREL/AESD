{
  nixpkgs  ? null
, compiler ? "ghc822"
}:

(import ../shell.nix) {package = "aesd-records-haystack"; nixpkgs = nixpkgs; compiler = compiler;}
