{
  mkDerivation, stdenv
, aeson, base, aesd-records, data-default, directory, filepath, HDBC, HDBC-mysql, HDBC-odbc, HDBC-postgresql, HDBC-sqlite3, uuid, yaml
, useMySQL      ? false
, useODBC       ? false
, usePostgreSQL ? true
, useSQLite3    ? true
}:
mkDerivation {
  pname = "aesd-records-hdbc";
  version = "0.5.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base aesd-records data-default directory filepath HDBC uuid
  ];
  executableHaskellDepends = [
    aeson base aesd-records data-default directory filepath HDBC uuid yaml
  ]
  ++ (if useMySQL      then [HDBC-mysql     ] else [])
  ++ (if useODBC       then [HDBC-odbc      ] else [])
  ++ (if usePostgreSQL then [HDBC-postgresql] else [])
  ++ (if useSQLite3    then [HDBC-sqlite3   ] else [])
  ;
  configureFlags = []
  ++ (if useMySQL      then ["-f MySQL=True"     ] else ["-f MySQL=False"     ])
  ++ (if useODBC       then ["-f ODBC=True"      ] else ["-f ODBC=False"      ])
  ++ (if usePostgreSQL then ["-f PostgreSQL=True"] else ["-f PostgreSQL=False"])
  ++ (if useSQLite3    then ["-f SQLite3=True"   ] else ["-f SQLite3=False"   ])
  ;
  homepage = "https://github.com/NREL/AESD/libs/Haskell/records-hdbc/ReadMe.md";
  description = "Database support for AESD records API";
  license = stdenv.lib.licenses.mit;
}
