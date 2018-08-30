{ mkDerivation, base, bytestring, cereal, containers, data-default
, directory, filepath, lens, mtl, protobuf, raft, random
, regex-posix, split, stdenv, stm, uuid, websockets
}:
mkDerivation {
  pname = "cesds-records";
  version = "0.4.4.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cereal containers data-default lens mtl protobuf
    raft regex-posix split stm uuid websockets
  ];
  executableHaskellDepends = [
    base bytestring cereal containers data-default directory filepath
    lens mtl protobuf raft random regex-posix split stm uuid websockets
  ];
  homepage = "https://github.nrel.gov/haskell/cesds-records";
  description = "CESDS Records";
  license = stdenv.lib.licenses.mit;
}
