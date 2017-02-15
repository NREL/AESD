{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 National Renewable Energy Laboratory
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

A skeletal implementation of a client.
-}


{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


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
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Monad (liftM2, when)
import Control.Monad.Except (liftIO)
import Data.Default (def)
import Data.Map.Strict as M ((!), empty, fromList, insert, member)
import Data.Maybe (fromJust)
import Network.WebSockets (Connection, runClient)
import Network.WebSockets.STM (Communicator, launch, send, start, stop)


-- | State information for a client.
data State =
  State
  {
    communicator   :: Communicator Request Response RequestIdentifier -- ^ WebSockets communicator.
  , nextIdentifier :: IO RequestIdentifier                            -- ^ Action to generate the next request identifier.
  , modelCache     :: TVar Cache                                      -- ^ Model cache.
  }


-- | Run a client.
clientMain :: String           -- ^ The WebSocket host address.
           -> Int              -- ^ The WebSocket port number.
           -> String           -- ^ The WebSocket path.
           -> (State -> IO ()) -- ^ Customize the client.
           -> IO ()            -- ^ Action for running the client.
clientMain host port path client =
  runClient host port path
    $ \connection ->
    do
      state <- makeModelCache connection
      client state
      close state


-- | Close a client.
close :: State -> IO ()
close State{..} = stop communicator


-- | Fetch model metadata.
fetchModels :: State -> IO (Either String [ModelMeta])
fetchModels state@State{..} =
  do
    -- Fetch the model model metadata.
    modelsOrError <-
      accumulate state
        (loadModelsMeta Nothing)
        $ onResponse handleError keep ignore ignore unexpected
    -- Store the models in the cache.
    case modelsOrError of
      Left  message -> return $ Left message
      Right models  -> do
                         atomically
                           . writeTVar modelCache
                           $ M.fromList
                           [
                             (
                               Model.identifier model
                             , def & modelMeta .~ model
                             )
                           |
                             model <- models
                           ]
                         return $ Right models


-- | Fetch records from the server.
fetchRecords :: State                              -- ^ The state of the client.
             -> ModelIdentifier                    -- ^ The model identifier.
             -> Maybe Int                          -- ^ The maximum number of records to request.
             -> IO (Either String [RecordContent]) -- ^ Action returning either an error or the records.
fetchRecords State{..} i c =
  do
    -- Check if the model is on the server.
    found <- M.member i <$> atomically (readTVar modelCache)
    if found
      then
        do -- FIXME: Generalize 'accumulate' to this case, too?
          -- Create an MVar to hold the result.
          result <- newEmptyMVar
          rid <- nextIdentifier
          -- Launch the handler of responses.
          launch communicator rid
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
          -- Send the request.
          send communicator rid
            $ loadRecordsData i (fromIntegral <$> c) []
          -- Return the result.
          takeMVar result
      else return $ Right []


-- | Fetch bookmark(s).
fetchBookmarks :: State                             -- ^ The state of the client.
               -> ModelIdentifier                   -- ^ The model identifier.
               -> Maybe BookmarkIdentifier          -- ^ The bookmark identifier, or all bookmarks.
               -> IO (Either String [BookmarkMeta]) -- ^ Action returning either an error or the bookmark(s).
fetchBookmarks state model bookmark =
  accumulate state
    (loadBookmarkMeta model bookmark)
    $ onResponse handleError ignore ignore keep unexpected


-- | Save a bookmark.
storeBookmark :: State                           -- ^ The state of the client.
              -> ModelIdentifier                 -- ^ The model identifier.
              -> BookmarkMeta                    -- ^ The bookmark metadata.
              -> IO (Either String BookmarkMeta) -- ^ Action returning eithre an error or the bookmark.
storeBookmark state model bookmark =
  fmap (fmap head)
    . accumulate state (saveBookmarkMeta model bookmark)
    $ onResponse handleError ignore ignore keep unexpected


-- | Make a model cache.
makeModelCache :: Connection -- ^ The web socket connection.
               -> IO State   -- ^ Action to create the state with a new model cache.
makeModelCache connection =
  do
    communicator <- start connection ((Request.identifier .~) . Just) (fromJust . (^. Response.identifier))
    nextIdentifier <- liftIO $ makeCounter (+ 1) 0
    modelCache <- newTVarIO M.empty
    return State{..}


-- | Handle an error.
handleError :: Maybe RequestIdentifier -- ^ The request identifier.
            -> String                  -- ^ The error message.
            -> IO (Either String a)    -- ^ Action returning either the error or a result.
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


-- | Accumulate responses.
accumulate :: (Monad m, Monoid a)
           => State                  -- ^ The state of the client.
           -> Request                -- ^ The request.
           -> (Response -> IO (m a)) -- ^ Process a response to yield an action with the results.
           -> IO (m a)               -- ^ Action for accumulating responses.
accumulate State{..} request processor =
  do
    -- Create an MVar to hold the result.
    result <- newEmptyMVar
    xs <- newTVarIO $ return mempty
    -- Generate the next request identifier.
    rid <- nextIdentifier
    -- Launch the handler of responses.
    launch communicator rid
      $ \response ->
      do
        -- Retrieve the response.
        x <- processor response
        -- Add the response to the chunks already received.
        xs' <-
          atomically
            $ do
              modifyTVar' xs $ liftM2 mappend x
              readTVar xs
        -- Store the result if there are no more chunks.
        let
          done = response ^. nextChunkIdentifier <= Just 0
        when done
          $ putMVar result xs'
        return done
    -- Make the request.
    send communicator rid request
    -- Wait for the response.
    takeMVar result
