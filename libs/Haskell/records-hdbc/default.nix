{ mkDerivation, aeson, base, cesds-records, data-default, directory
, filepath, HDBC, HDBC-mysql, HDBC-odbc, HDBC-postgresql
, HDBC-sqlite3, stdenv, uuid, yaml
}:
mkDerivation {
  pname = "cesds-records-hdbc";
  version = "0.4.1.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base cesds-records data-default directory filepath HDBC uuid
  ];
  executableHaskellDepends = [
    aeson base cesds-records data-default directory filepath HDBC
    HDBC-mysql HDBC-odbc HDBC-postgresql HDBC-sqlite3 uuid yaml
  ];
  homepage = "https://github.nrel.gov/haskell/cesds-records-hdbc";
  description = "Database support for CESDS records API";
  license = stdenv.lib.licenses.mit;
}
