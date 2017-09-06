## Haskell

Both client and server applications in Haskell are available for the AESD Records API.  Full documentation resides at <<https://github.com/NREL/AESD/lib/haskell>>.


### Client Library


#### Types


**`data State`**

State information for a client.


#### Entry Point


**`clientMain`**

Run a client.

| Argument Type       | Descrption                     |
|---------------------|--------------------------------|
| :: String           | The WebSocket host address.    |
| -> Int              | The WebSocket port number.     |
| -> String           | The WebSocket path.            |
| -> (State -> IO ()) | Customize the client.          |
| -> IO ()            | Action for running the client. |


**`close`**

Close a client.

| Argument Type | Descrption                     |
|---------------|--------------------------------|
| :: State      | The state of the client.       |
|  -> IO ()     | Action for closing the client. |


#### Server Requests


**`fetchModels`**

Fetch model metadata.

| Argument Type                     | Descrption                                      |
|-----------------------------------|-------------------------------------------------|
| :: State                          | The state of the client.                        |
| -> IO (Either String [ModelMeta]) | Action returning either an error or the models. |


**`fetchRecords`**

Fetch records from the server.

| Argument Type                         | Descrption                                       |
|---------------------------------------|--------------------------------------------------|
| :: State                              | The state of the client.                         |
| -> ModelIdentifier                    | The model identifier.                            |
| -> Maybe Int                          | The maximum number of records to request.        |
| -> IO (Either String [RecordContent]) | Action returning either an error or the records. |


**`fetchBookmarks`**

Fetch bookmark(s).

| Argument Type                        | Descrption                                           |
|--------------------------------------|------------------------------------------------------|
| :: State                             | The state of the client.                             |
| -> ModelIdentifier                   | The model identifier.                                |
| -> Maybe BookmarkIdentifier          | The bookmark identifier, or all bookmarks.           |
| -> IO (Either String [BookmarkMeta]) | Action returning either an error or the bookmark(s). |


**`storeBookmark`**

Save a bookmark.

| Argument Type                      | Descrption                                        |
|------------------------------------|---------------------------------------------------|
| :: State                           | The state of the client.                          |
| -> ModelIdentifier                 | The model identifier.                             |
| -> BookmarkMeta                    | The bookmark metadata.                            |
| -> IO (Either String BookmarkMeta) | Action returning eithre an error or the bookmark. |


### Server Library

The server library provides two options for implementing a AESD Records server.  The `CESDS.Records.Server` module provides a main entry point `serverMain`, a type class `ModelManager`, and a monad `ServiceM` that implement skeletal server which handles all of the WebSocket communication and Protocol Buffer serialization: an implementer need only create an instance of `ModelManager`.  Furthermore, the `CESDS.Records.Server.Manager` module provides such an instance `InMemoryManager` of the type class `ModelManger` to handle in-memory caching of data and on-disk persistence of bookmarks: here, an implementer just calls the function `makeInMemoryManager` and provides several functions that retrieve content:

**`makeInMemoryManager`**

Construct an in-memory model manager.

| Argument Type                                                | Descrption                       |
|--------------------------------------------------------------|----------------------------------|
| :: Maybe FilePath	                                       | The name of the journal file.    |
| -> a	                                                       | The initial state.               |
| -> (a -> IO ([ModelMeta], a))	                               | Handle listing models.           |
| -> (a -> ModelMeta -> IO ([RecordContent], a))	       | Handle loading record data.      |
| -> (a -> ModelMeta -> [VarValue] -> IO ([RecordContent], a)) | Handle performing work.          |
| -> IO (InMemoryManager a)	                               | Action constructing the manager. |


### Server Backends


#### Tab-Separate-Value Files

	cesds-file-server <host> <port> <directory> <persistence> <chunkSize>

| Parameter   | Description                            |
|-------------|----------------------------------------|
| host        | host address to bind to                |
| port        | port to bind to                        |
| directory   | directory with TSV files to be served  |
| peristience | filename for bookmark data             |
| chunkSize   | number of records return in each chunk |


#### Database Queries

| Parameter   | Description                                | PostgreSQL                 | MySQL                      | SQLite3           | ODBC                       |
|-------------|--------------------------------------------|----------------------------|----------------------------|-------------------|----------------------------|
| host        | host address to which to bind the service  | required                   | required                   | required          | required                   |
| port        | port to which to bind the service          | required                   | required                   | required          | required                   |
| directory   | directory with queries to be served        | required                   | required                   | required          | required                   |
| peristience | filename for bookmark data                 | optional                   | optional                   | optional          | optional                   |
| chunkSize   | number of records return in each chunk     | optional                   | optional                   | optional          | optional                   |
| database    | database connection information            | required connection string | required connection string | required filename | required connection string |


#### Haystack Sensor Measurements


	siteAccess     :
	  server       : xv11skys01.nrel.gov
	  root         : /api/nrel_wt_V7
	  authorization: ["bbush",<<INSERT PASSWORD HERE>>]
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
