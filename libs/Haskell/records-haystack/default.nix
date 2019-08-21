{
  mkDerivation, stdenv
, aeson, base, bytestring, aesd-records, daft, data-default, http-conduit, lens, mtl, network-uri, raft, resourcet, text, time, unordered-containers, vinyl, yaml
}:
mkDerivation {
  pname = "aesd-records-haystack";
  version = "0.5.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring aesd-records daft data-default http-conduit lens mtl network-uri raft resourcet text time unordered-containers vinyl yaml
  ];
  executableHaskellDepends = [
    aeson base bytestring aesd-records daft data-default http-conduit lens mtl network-uri raft resourcet text time unordered-containers vinyl yaml
  ];
  homepage = "https://github.com/NREL/AESD/libs/Haskell/records-haystack/ReadMe.md";
  description = "Haystack support for AESD records API";
  license = stdenv.lib.licenses.mit;
}
