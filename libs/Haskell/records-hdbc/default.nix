{ mkDerivation, aeson, base, aesd-records, data-default, directory
, filepath, HDBC, HDBC-mysql, HDBC-odbc, HDBC-postgresql
, HDBC-sqlite3, stdenv, uuid, yaml
}:
mkDerivation {
  pname = "aesd-records-hdbc";
  version = "0.4.1.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base aesd-records data-default directory filepath HDBC uuid
  ];
  executableHaskellDepends = [
    aeson base aesd-records data-default directory filepath HDBC
    HDBC-mysql HDBC-odbc HDBC-postgresql HDBC-sqlite3 uuid yaml
  ];
  homepage = "https://github.com/NREL/AESD/libs/Haskell/records-hdbc";
  description = "Database support for AESD records API";
  license = stdenv.lib.licenses.mit;
}
