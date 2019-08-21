{
  mkDerivation, stdenv
, base, bytestring, cereal, containers, data-default, directory, filepath, lens, mtl, protobuf, raft, random , regex-posix, split, stm, uuid, websockets
}:
mkDerivation {
  pname = "aesd-records";
  version = "0.5.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cereal containers data-default lens mtl protobuf raft regex-posix split stm uuid websockets
  ];
  executableHaskellDepends = [
    base bytestring cereal containers data-default directory filepath lens mtl protobuf raft random regex-posix split stm uuid websockets
  ];
  homepage = "https://github.com/NREL/AESD/libs/Haskell/aesd-records/ReadMe.md";
  description = "AESD Records";
  license = stdenv.lib.licenses.mit;
}
