{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 National Renewable Energy Laboratory
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

A skeletal implementation of a client.

The client forks a thread to receive all incoming WebSocket messages and maintains a collection of handlers for handling specific responses.
-}


{-# LANGUAGE TupleSections #-}


module CESDS.Records.Client (
-- * Types
  State
-- * Entry point
, clientMain
, close
-- * Server requests
, fetchModels
, fetchRecords
, fetchBookmarks
, storeBookmark
) where


import CESDS.Records.Server.Manager (Cache, ContentStatus(..), contentStatus, modelMeta, recordContent)
import CESDS.Types.Bookmark as Bookmark (BookmarkIdentifier, BookmarkMeta)
import CESDS.Types.Model as Model (ModelIdentifier, ModelMeta, identifier)
import CESDS.Types.Record (RecordContent)
import CESDS.Types.Request as Request (Request, RequestIdentifier, identifier, loadBookmarkMeta, loadModelsMeta, loadRecordsData, saveBookmarkMeta)
import CESDS.Types.Response as Response (Response, identifier, nextChunkIdentifier, onResponse)
import Control.Concurrent.Util (makeCounter)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~), over)
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Monad (join, liftM2, when)
import Control.Monad.Except (liftIO)
import Data.Default (def)
import Data.Maybe (fromJust, fromMaybe)
import Network.WebSockets (Connection, receiveData, runClient, sendBinaryData)

import qualified Data.Map.Strict as M ((!), delete, empty, foldr, fromList, insert, lookup, member)


-- | State information for a client.
type State = (ThreadId, Processor, TVar Cache)


-- | Run a client.
clientMain :: String           -- ^ The WebSocket host address.
           -> Int              -- ^ The WebSocket port number.
           -> String           -- ^ The WebSocket path.
           -> (State -> IO ()) -- ^ Customize the client.
           -> IO ()            -- ^ Action for running the client.
clientMain host port path client =
  runClient host port path
    $ (client =<<)
    . makeModelCache


-- | Close a client.
close :: State -> IO ()
close (thread, _, _) = killThread thread


-- | Fetch model metadata.
fetchModels :: State                          -- ^ The state of the client.
            -> IO (Either String [ModelMeta]) -- ^ Action returning either an error or the models.
fetchModels (_, _, modelCache) =
  Right . M.foldr ((:) . (^. modelMeta)) []
    <$> atomically (readTVar modelCache)


-- | Fetch records from the server.
fetchRecords :: State                              -- ^ The state of the client.
             -> ModelIdentifier                    -- ^ The model identifier.
             -> Maybe Int                          -- ^ The maximum number of records to request.
             -> IO (Either String [RecordContent]) -- ^ Action returning either an error or the records.
fetchRecords (_, processor, modelCache) i c =
  do
    -- Check if the model is on the server.
    found <- M.member i <$> atomically (readTVar modelCache)
    if found
      then
        do -- FIXME: Generalize 'accumulate' to this case, too?
          -- Create an MVar to hold the result.
          result <- newEmptyMVar
          processor
            (loadRecordsData i (fromIntegral <$> c) [])
            $ \response ->
            do
              -- See if this the last chunk.
              let
                 done = response ^. nextChunkIdentifier <= Just 0
              -- Get the records in the resonse.
              recs <- onResponse handleError ignore keep ignore unexpected response
              recs' <-
                case recs of
                  -- Report an error.
                  Left message -> return $ Left message
                  -- Store the records in the cache.
                  Right recs'' -> fmap Right
                                    . atomically
                                    $ do
                                      cache <- readTVar modelCache
                                      let
                                        model = cache M.! i
                                        model' = model
                                                  & over recordContent (++ recs'')
                                                  & contentStatus .~ (if done then CompleteContent else PendingContent)
                                      writeTVar modelCache $ M.insert i model' cache
                                      return $ model' ^. recordContent
              -- If finished, take the records from the cache and put them in the result.
              when done
                $ putMVar result recs'
              return done
          -- Return the result.
          takeMVar result
      else return $ Right []


-- | Fetch bookmark(s).
fetchBookmarks :: State                             -- ^ The state of the client.
               -> ModelIdentifier                   -- ^ The model identifier.
               -> Maybe BookmarkIdentifier          -- ^ The bookmark identifier, or all bookmarks.
               -> IO (Either String [BookmarkMeta]) -- ^ Action returning either an error or the bookmark(s).
fetchBookmarks (_, processor, _) model bookmark =
  accumulate
    (processor $ loadBookmarkMeta model bookmark)
    $ onResponse handleError ignore ignore keep unexpected


-- | Save a bookmark.
storeBookmark :: State                           -- ^ The state of the client.
              -> ModelIdentifier                 -- ^ The model identifier.
              -> BookmarkMeta                    -- ^ The bookmark metadata.
              -> IO (Either String BookmarkMeta) -- ^ Action returning eithre an error or the bookmark.
storeBookmark (_, processor, _) model bookmark =
  do
    -- Create an MVar to hold the result.
    result <- newEmptyMVar
    processor
      (saveBookmarkMeta model bookmark)
      $ \response ->
      do
        -- Retrieve the bookmarks.
        bookmark' <- fmap head <$> onResponse handleError ignore ignore keep unexpected response
        putMVar result bookmark'
        return True
    -- Return the result.
    takeMVar result


-- | Make a model cache.
makeModelCache :: Connection -- ^ The web socket connection.
               -> IO State   -- ^ Action to create the state with a new model cache.
makeModelCache connection =
  do
    (thread, processor) <- makeProcessor connection
    -- Fetch the model model metadata.
    models <-
      either error id
      <$> accumulate
        (processor $ loadModelsMeta Nothing)
        (onResponse handleError keep ignore ignore unexpected)
    -- Return the thread for the processor, the processing function, and make the cache.
    fmap (thread, processor, )
      . newTVarIO
      $ M.fromList
      [
        (
          Model.identifier model
        , def & modelMeta .~ model
        )
      |
        model <- models
      ]


-- | Handle an error.
handleError :: Maybe RequestIdentifier -- ^ The request identifier.
            -> String                  -- ^ The error message.
            -> IO (Either String a)    -- Action returning either the error or a result.
handleError = const $ return . Left


-- | Report an unexpected result.
unexpected :: Either String a
unexpected = Left "Unexpected result."


-- | Ignore a result.
ignore :: Maybe RequestIdentifier -> a -> IO (Either String b)
ignore = const . const $ return unexpected


-- | Retain a result.
keep :: Maybe RequestIdentifier -> a -> IO (Either String a)
keep = const $ return . Right


-- | A processor of requests.
type Processor = Request -> Handler -> IO ()


-- | A handler of responses.
type Handler = Response -> IO Bool


-- | Create a processor.
makeProcessor :: Connection               -- ^ The web socket connection.
              -> IO (ThreadId, Processor) -- ^ Action to create the thread and processor.
makeProcessor connection =
  do
    -- Create a TVar for the list of currently active handlers.
    handlers <- liftIO $ newTVarIO M.empty
    -- Create a counter to generate unique request identifiers
    nextIdentifier <- liftIO $ makeCounter (+ 1) (0 :: RequestIdentifier)
    -- Fork a thread to receive responses.
    receiver <-
      forkIO
        $ let
            -- Function for looking up which handler to use to process a response.
            findHandler = (<$> atomically (readTVar handlers)) . M.lookup
            -- Repeat until done.
            loop =
              do
                liftIO
                  $ do
                    -- Receive a response and send it to the appropriate handler.
                    response <- receiveData connection
                    let
                      i = response ^. Response.identifier
                    f <- fmap join . sequence $ findHandler <$> i
                    -- Remove the handler if it indicates it is finished.
                    done <- fmap (fromMaybe False) . sequence $ ($ response) <$> f
                    when done
                      . atomically
                      . modifyTVar' handlers
                      . M.delete
                      $ fromJust i
                loop
          in
            loop
    -- Return the thread for receiving responses and the function for processing requests.
    return
      (
        receiver
      , \request handler ->
          do
            -- Use the next unique request identifier.
            i <- liftIO nextIdentifier
            let
              request' = (Request.identifier .~ Just i) request
            -- Send the request.
            liftIO
              $ do
                atomically
                  . modifyTVar' handlers 
                  $ M.insert i handler
                sendBinaryData connection request'
      )
        

-- | Accumulate responses.
accumulate :: Monad m
           => (Handler -> IO ())       -- ^ Handle responses, indicating when the handling is complete, and yield an action.
           -> (Response -> IO (m [t])) -- ^ Process a response to yield an action with the results.
           -> IO (m [t])               -- ^ Action for accumulating responses.
accumulate responder processor =
  do
    -- Create an MVar to hold the result.
    result <- newEmptyMVar
    xs <- newTVarIO $ return []
    responder
      $ \response ->
      do
        -- Retrieve the response.
        x <- processor response
        -- Add the response to the chunks already received.
        xs' <-
          atomically
            $ do
              modifyTVar' xs $ liftM2 (++) x
              readTVar xs
        -- Store the result if there are no more chunks.
        let
          done = response ^. nextChunkIdentifier <= Just 0
        when done
          $ putMVar result xs'
        return done
    takeMVar result
