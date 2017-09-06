## Haskell

Both client and server applications in Haskell are available for the AESD Records API.  Full documentation resides at <<https://github.com/NREL/AESD/lib/haskell>>.


### Client Library

The client library described below provides the basic functions for interacting with any AESD Records server.


#### Types


**`data State`**

State information for a client.


#### Entry Point


**`clientMain`**

Run a client.

| Argument Type         | Descrption                     |
|-----------------------|--------------------------------|
| `:: String`           | The WebSocket host address.    |
| `-> Int`              | The WebSocket port number.     |
| `-> String`           | The WebSocket path.            |
| `-> (State -> IO ())` | Customize the client.          |
| `-> IO ()`            | Action for running the client. |


**`close`**

Close a client.

| Argument Type   | Descrption                     |
|-----------------|--------------------------------|
| `:: State`      | The state of the client.       |
| ` -> IO ()`     | Action for closing the client. |


#### Server Requests


**`fetchModels`**

Fetch model metadata.

| Argument Type                       | Descrption                                      |
|-------------------------------------|-------------------------------------------------|
| `:: State`                          | The state of the client.                        |
| `-> IO (Either String [ModelMeta])` | Action returning either an error or the models. |


**`fetchRecords`**

Fetch records from the server.

| Argument Type                           | Descrption                                       |
|-----------------------------------------|--------------------------------------------------|
| `:: State`                              | The state of the client.                         |
| `-> ModelIdentifier`                    | The model identifier.                            |
| `-> Maybe Int`                          | The maximum number of records to request.        |
| `-> IO (Either String [RecordContent])` | Action returning either an error or the records. |


**`fetchBookmarks`**

Fetch bookmark(s).

| Argument Type                          | Descrption                                           |
|----------------------------------------|------------------------------------------------------|
| `:: State`                             | The state of the client.                             |
| `-> ModelIdentifier`                   | The model identifier.                                |
| `-> Maybe BookmarkIdentifier`          | The bookmark identifier, or all bookmarks.           |
| `-> IO (Either String [BookmarkMeta])` | Action returning either an error or the bookmark(s). |


**`storeBookmark`**

Save a bookmark.

| Argument Type                        | Descrption                                        |
|--------------------------------------|---------------------------------------------------|
| `:: State`                           | The state of the client.                          |
| `-> ModelIdentifier`                 | The model identifier.                             |
| `-> BookmarkMeta`                    | The bookmark metadata.                            |
| `-> IO (Either String BookmarkMeta)` | Action returning either an error or the bookmark. |


### Server Library

The server library provides two options for implementing a AESD Records server.  The `CESDS.Records.Server` module provides a main entry point `serverMain`, a type class `ModelManager`, and a monad `ServiceM` that implement skeletal server which handles all of the WebSocket communication and Protocol Buffer serialization: an implementer need only create an instance of `ModelManager`.  Furthermore, the `CESDS.Records.Server.Manager` module provides such an instance `InMemoryManager` of the type class `ModelManger` to handle in-memory caching of data and on-disk persistence of bookmarks: here, an implementer just calls the function `makeInMemoryManager` and provides several functions that retrieve content:

**`makeInMemoryManager`**

Construct an in-memory model manager.

| Argument Type                                                  | Descrption                                         |
|----------------------------------------------------------------|----------------------------------------------------|
| `:: Maybe FilePath`                                            | The name of the journal file.                      |
| `-> a`                                                         | The initial state.                                 |
| `-> (a -> IO ([ModelMeta], a))`                                | List models in an action modifying the state.      |
| `-> (a -> ModelMeta -> IO ([RecordContent], a))`               | Load record data in an action modifying the state. |
| `-> (a -> ModelMeta -> [VarValue] -> IO ([RecordContent], a))` | Performing work in an action modifying the state.  |
| `-> IO (InMemoryManager a)`                                    | Action constructing the manager.                   |


### Server Backends

As previously mentioned, prebuilt servers have been implemented for standard types of data sources.


#### Tab-Separate-Value Files

Serving tab-separated-value (TSV) files is a simple as placing the TSV files in a directory and starting a server at the command line, which the arguments specified in the table below:

	aesd-file-server <host> <port> <directory> <persistence> <chunkSize>

| Parameter   | Description                               |
|-------------|-------------------------------------------|
| host        | host address to which to bind the service |
| port        | port to which to bind the service         |
| directory   | directory with TSV files to be served     |
| peristience | filename for bookmark data                |
| chunkSize   | number of records return in each chunk    |

Table: Command-line arguments for serving TSV files.


#### Database Queries

The AESD Records servers have been implemented for the most common database backends.  Each server takes a single command-line argument specifying a YAML [@yaml] configuration file with the parametes in the table below.

| Parameter   | Description                                | PostgreSQL                 | MySQL                      | SQLite3           | ODBC                       |
|-------------|--------------------------------------------|----------------------------|----------------------------|-------------------|----------------------------|
| host        | host address to which to bind the service  | required                   | required                   | required          | required                   |
| port        | port to which to bind the service          | required                   | required                   | required          | required                   |
| directory   | directory with queries to be served        | required                   | required                   | required          | required                   |
| peristience | filename for bookmark data                 | optional                   | optional                   | optional          | optional                   |
| chunkSize   | number of records return in each chunk     | optional                   | optional                   | optional          | optional                   |
| database    | database connection information            | required connection string | required connection string | required filename | required connection string |

Table: Parameters for database backends serving AESD Records.


#### Haystack Sensor Measurements and the "Internet of Things"

Furthermore, a server for Project Haystack [@haystack] data feeds, typically sensor measurements from devices in the "internet of things", has been implemented.  The server takes a command-line arguments specified in the table below.

	aesd-haystack-server <configuration> <host> <port> <startTime> <persistence> <chunkSize>

| Parameter     | Description                                                   |
|---------------|---------------------------------------------------------------|
| configuration | YAML configuration file for accessing the Haystack service    |
| host          | host address to which to bind the service                     |
| port          | port to which to bind the service                             |
| startTime     | earliest time to serve, specified in seconds of the UTC Epoch |
| peristience   | filename for bookmark data                                    |
| chunkSize     | number of records return in each chunk                        |

Table: Command-line arguments for serving Haystack data feeds.

The parameters in the YAML configuration file like the one below and are described in the following table:

	siteAccess     :
	  server       : xv11skys01.nrel.gov
	  root         : /api/nrel_wt_V7
	  authorization: ["my username","my password"]
	  secure       : false
	  timeZone     : [-360, true, Denver]
	siteIdentifier : NWTCv4
	siteURI        : http://aesd.nrel.gov/records/v4/nwtc/
	siteName       : NREL NWTC
	siteDescription: Sensors from NREL National Wind Technology Center
	siteTags       :
	               ! 'DC.source'     : https://xv11skys01.nrel.gov/proj/nrel_wt_v7
	               ! 'DC.creator'    : Brian W Bush <brian.bush@nrel.gov>
	               ! 'DC.description': NREL NWTC sensors
	siteMeters     :
	               - 1dca834e-c6af46d6 NWTC Alstom Turbine Electricity Meter Turbine-Alstom kW Demand Forward
	               - 1dca834e-69a3e57e NWTC Alstom Turbine Electricity Meter Turbine-Alstom kW Demand Reverse
	               - 1dca834e-f56e11f0 NWTC Alstom Turbine Electricity Meter Turbine-Alstom kWh Delivered Forward

| Parameter      | Description                                                                                                      | Required? |
|----------------|------------------------------------------------------------------------------------------------------------------|-----------|
| server         | hostname and port for the Haystack server                                                                        | required  |
| root           | path to the Haystack REST service                                                                                | required  |
| authorization  | the username and password for accessing the Haystack REST service                                                | optional  |
| secure         | whether to use HTTPS instead of HTTP                                                                             | optional  |
| timezone       | timezone information: minutes offset from UTC, whether to use daylight savings time, and the geographic location | required  |
| siteIdentifier | identifier for the AESD server                                                                                   | required  |
| siteURI        | URI for the AESD server metadata                                                                                 | required  |
| siteName       | name of the AESD server                                                                                          | required  |
| siteTags       | key-value pairs tagging the server with additional information                                                   | optional  |
| siteMeters     | list of meters to expose on the AESD server: the Haystack ID is followed by a space and textual description      | required  |

Table: YAML configuration parameters for Haystack-based AESD Records servers.
