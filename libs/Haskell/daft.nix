{
  mkDerivation, stdenv, fetchgit
, aeson, base, bytestring, containers, data-default, deepseq, hashable, mtl, raft, text, tostring, type-list, unordered-containers, vinyl
}:
mkDerivation rec {
  pname = "daft";
  version = "0.5.0.0";
  src = fetchgit {
    url = "https://github.com/NREL/daft";
    rev = "c20786ace9a131283fd1a42912ac275da0cf05fe";
    sha256 = "05v2bhngzh9w5s4yc8h2mcsf3v0r4l9pg17sfppcjfa0831qmh8h";
    fetchSubmodules = false;
  };
  postUnpack = ''
    sourceRoot+=/${pname}
  '';
  libraryHaskellDepends = [
    aeson base bytestring containers data-default deepseq hashable mtl raft text tostring type-list unordered-containers vinyl
  ];
  license = stdenv.lib.licenses.unfree;
}
