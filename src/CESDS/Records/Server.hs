{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 National Renewable Energy Laboratory
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

A skeletal implementation of a server.
-}


{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CESDS.Records.Server (
-- * Entry ponts
  serverMain
, ModelManager(..)
-- * Service monad
, ServiceM
, serviceM
, fromService
, runServiceToIO
-- * Modifying a service
, modifyService
, modifyService'
, modifyServiceIO'
) where


import CESDS.Types.Bookmark as Bookmark (BookmarkIdentifier, BookmarkMeta)
import CESDS.Types.Filter (Filter)
import CESDS.Types.Model as Model (ModelIdentifier, ModelMeta)
import CESDS.Types.Record (RecordContent, VarValue)
import CESDS.Types.Request as Request (identifier, onLoadBookmarkMeta, onCancel, onLoadModelsMeta, onLoadRecordsData', onRequest, onSaveBookmarkMeta, onWork)
import CESDS.Types.Response as Response (bookmarkMetasResponse, chunkIdentifier, errorResponse, identifier, modelMetasResponse, nextChunkIdentifier, recordsResponse)
import CESDS.Types.Variable as Variable (VariableIdentifier)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, MonadError, MonadIO, liftIO, runExceptT, throwError)
import Control.Monad.Except.Util (guardSomeException)
import Control.Monad.Reader (MonadReader, ReaderT, ask, lift, runReaderT)
import Control.Monad.Trans (MonadTrans)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Network.WebSockets (acceptRequest, receiveData, runServer, sendBinaryData)


-- | Run the server.
serverMain :: ModelManager a
           => String    -- ^ The WebSocket host address.
           -> Int       -- ^ The WebSocket port number.
           -> Maybe Int -- ^ The number of records per chunk.
           -> a         -- ^ The inital state of the model manager.
           -> IO ()     -- ^ Action to run the server.
serverMain host port chunkSize initialManager =
  do
    -- Maintain a list of requests that have been cancelled.
    cancellations <- newTVarIO [] -- FIXME: Inefficient.
    -- Create a TVar for the model manager.
    manager <- newTVarIO initialManager
    -- Run a WebSocket server.
    runServer host port
      $ \pending ->
      do
        -- Accept all pending requests.
        connection <- acceptRequest pending
        let
          -- Loop forever.
          loop =
            do
              -- Receive a new request.
              request <- receiveData connection
              -- Process the request to determine its result.
              result <-
                runServiceToIO manager -- FIXME: If the ModelManager uses guardSomeException, then no IOExceptions ever show up here?
                  $ onRequest
                  (
                    -- Look metadata for model(s).
                    onLoadModelsMeta -- FIXME: Do this on a separate thread.
                      $ fmap ((: []) . modelMetasResponse)
                      . lookupModels False
                  )
                  (
                    -- Load records.
                    onLoadRecordsData' -- FIXME: Do this on a separate thread.
                      $ \model count variables maybeBookmarkOrFilter ->
                      do
                        -- Find the model.
                        [model'] <- lookupModels True $ Just model
                        -- Take only the requested number of records
                        recs <- maybe id (take . fromIntegral) count <$> loadContent model' maybeBookmarkOrFilter variables
                        -- Break the records into chunks and package them into separate responses.
                        return
                          [
                            recordsResponse recs''
                              & chunkIdentifier .~ Just i'
                              & nextChunkIdentifier .~ (if fromIntegral i' < n then Just (i' + 1) else Nothing)
                          |
                            let recs' = if null recs then [[]] else chunksOf (fromMaybe maxBound chunkSize) recs
                          , let n = length recs'
                          , (i', recs'') <- zip [1..] recs'
                          ]
                  )
                  (
                    -- Load metadata for bookmark(s)
                    onLoadBookmarkMeta
                      $ (fmap ((: []) . bookmarkMetasResponse) .)
                      . lookupBookmarks
                  )
                  (
                    -- Save a new bookmark.
                    onSaveBookmarkMeta
                     $ (fmap ((: []) . bookmarkMetasResponse . (: [])) . )
                     . saveBookmark
                  )
                  (
                    -- Record a previous request as having been cancelled.
                    onCancel
                      $ \i ->
                      do
                        liftIO . atomically $ modifyTVar' cancellations (Just i :)
                        return []
                  )
                  (
                    -- Dispatch more work.
                    onWork -- FIXME: Do this on a separate thread.
                      $ \model inputs ->
                      do
                        [model'] <- lookupModels True $ Just model
                        recs <- doWork model' inputs
                        return [recordsResponse recs]
                  )
                  [errorResponse "Unsupported request."]
                request
              -- Check for cancellation, and then send the responses.
              cancellations' <- liftIO . atomically $ readTVar cancellations
              forM_
                (filter ((`notElem` cancellations') . (^. Response.identifier)) $ either ((: []) . errorResponse) id result)
                $ sendBinaryData connection
                . (Response.identifier .~ request ^. Request.identifier)
              loop
        loop


-- | Type class for managing a model.
class ModelManager a where
 
  -- | List all models.
  listModels :: ServiceM a [ModelMeta]

  -- | Lookup a model.
  lookupModel :: ModelIdentifier      -- ^ The model identifier.
              -> ServiceM a ModelMeta -- ^ Action to lookup a model.

  -- | Load record data.
  loadContent :: ModelMeta                                -- ^ The model metadata.
              -> Maybe (Either BookmarkIdentifier Filter) -- ^ Maybe the bookmark or filter.
              -> [VariableIdentifier]                     -- ^ The variables to select.
              -> ServiceM a [RecordContent]               -- ^ Action to load record data.

  -- | List all bookmarks.
  listBookmarks :: ModelIdentifier           -- ^ The model identifier.
                -> ServiceM a [BookmarkMeta] -- ^ Action to list all bookmarks.

  -- | Lookup a bookmark.
  lookupBookmark :: ModelIdentifier         -- ^ The model identifier.
                 -> BookmarkIdentifier      -- ^ The bookmark identifier.
                 -> ServiceM a BookmarkMeta -- ^ Action to lookup a bookmark.

  -- | Save a bookmark.
  saveBookmark :: ModelIdentifier         -- ^ The model identifier.
               -> BookmarkMeta            -- ^ The bookmark metadata.
               -> ServiceM a BookmarkMeta -- ^ Action to save a bookmark and return it.

  -- | Perform work for a model.
  doWork :: ModelMeta                  -- ^ The model metadata.
         -> [VarValue]                 -- ^ The values of the input variables.
         -> ServiceM a [RecordContent] -- ^ Action to perform the work and return the new records.


-- | Lookup a model.
lookupModels :: ModelManager a
             => Bool                   -- ^ Whether a model identifier must be provided. 
             -> Maybe ModelIdentifier  -- ^ The model identifier, or all models.
             -> ServiceM a [ModelMeta] -- ^ Action to lookup models.
lookupModels True  Nothing      = return []
lookupModels False Nothing      = listModels
lookupModels _     (Just model) = (: []) <$> lookupModel model


-- | Lookup bookmark(s).
lookupBookmarks :: ModelManager a
                => ModelIdentifier            -- ^ The model identifier.
                -> Maybe BookmarkIdentifier   -- ^ The bookmark identifier, or all bookmarks.
                -> ServiceM  a [BookmarkMeta] -- ^ Action to lookup the bookmark(s).
lookupBookmarks model Nothing         = listBookmarks model
lookupBookmarks model (Just bookmark) = (: []) <$> lookupBookmark model bookmark


-- | The service.
newtype ServiceM s a = ServiceM {runServiceM :: ExceptT String (ReaderT (TVar s) IO) a}
  deriving (Applicative, Functor, MonadError String, Monad, MonadIO, MonadReader (TVar s))


-- | Run the service to the IO monad
runServiceToIO :: TVar s               -- ^ The initial state.
               -> ServiceM s a         -- ^ The service.
               -> IO (Either String a) -- ^ The action to run the service, possibly reporting an error.
runServiceToIO state service = runReaderT (runExceptT (runServiceM service)) state


-- | Lift into the service.
serviceM :: MonadTrans t => ServiceM s a -> t (ServiceM s) a
serviceM = lift


-- | Apply a function to a service.
fromService :: (s -> a)     -- ^ The function.
            -> ServiceM s a -- ^ The action resulting from applying the function to the state.
fromService f = f <$> (ask >>= liftIO . readTVarIO)


-- | Modify the state in a service.
modifyService :: (s -> s) -> ServiceM s ()
modifyService f = ask >>= liftIO . atomically . flip modifyTVar' f


-- | Modify the state in a service, allowing for an error result.
modifyService' :: (s -> Either String (s, a)) -> ServiceM s a
modifyService' f =
  do
    sTVar <- ask
    result <-
      liftIO
        . atomically
        $ do
          s <- readTVar sTVar
          case f s of
            Left message  -> return $ Left message
            Right (s', x) -> writeTVar sTVar s' >> return (Right x)
    either throwError return result


-- | Modify the state in a service, allowing for an error result.
modifyServiceIO' :: (s -> IO (Either String (s, a))) -> ServiceM s a
modifyServiceIO' f =
  do -- FIXME: Make this atomic.
    sTVar <- ask
    s <- liftIO $ readTVarIO sTVar
    sx <- guardSomeException $ f s
    case sx of
      Left message  -> throwError message
      Right (s', x) -> do
                         liftIO . atomically $ writeTVar sTVar s'
                         return x
