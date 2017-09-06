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


