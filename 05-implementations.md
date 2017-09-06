# Implementations

This section provides an overview of the variety of libraries and applications implementing the AESD Records API (see the table below).  In particular, pre-built applications are available for serving text-based data sources, database queries, and sensor data feeds.  Application Container Images (ACIs) [@aci] of each have been packed for use with the rkt container engine [@rkt].

| Client or Server? | Library or Application?  | Data Source | Implementation Language | Computing Platforms | URL                                         |
|-------------------|--------------------------|-------------|-------------------------|---------------------|---------------------------------------------|
| client            | GUI application          | any         | C++                     | Mac, Winodws, Linux | https://github.nrel.gov/d-star/cpp-records  |
| server            | GUI/CLI applications     | CSV files   | C++                     | Mac, Winodws, Linux | https://github.nrel.gov/d-star/cpp-records  |
| client            | library                  | any         | Haskell                 | Mac, Windows, Linux | https://github.com/NREL/AESD/lib/haskell    |
| server            | CLI application          | TSV files   | Haskell                 | Mac, Windows, Linux | https://github.com/NREL/AESD/lib/haskell    |
| server            | CLI application          | PostgreSQL  | Haskell                 | Mac, Windows, Linux | https://github.com/NREL/AESD/lib/haskell    |
| server            | CLI application          | MySQL       | Haskell                 | Mac, Windows, Linux | https://github.com/NREL/AESD/lib/haskell    |
| server            | CLI application          | SQLite3     | Haskell                 | Mac, Windows, Linux | https://github.com/NREL/AESD/lib/haskell    |
| server            | CLI application          | ODBC        | Haskell                 | Mac, Windows, Linux | https://github.com/NREL/AESD/lib/haskell    |
| server            | CLI application          | Haystack    | Haskell                 | Mac, Windows, Linux | https://github.com/NREL/AESD/lib/haskell    |
| client            | library, web application | any         | JavaScript              | Chrome, Firefox     | https://github.com/NREL/AESD/lib/javascript |
| client            | library                  | any         | Python                  | any                 | https://github.com/NREL/AESD/lib/python     |
| client            | library                  | any         | R                       | any                 | https://github.nrel.gov/d-star/r-records    |

Table: Available client and server applications and libraries for AESD Records.
