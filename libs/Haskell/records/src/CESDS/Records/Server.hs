{-|
Module      :  $Header$
Copyright   :  (c) 2016-17 National Renewable Energy Laboratory
License     :  MIT
Maintainer  :  Brian W Bush <brian.bush@nrel.gov>
Stability   :  Stable
Portability :  Portable

A skeletal implementation of a server.
-}


{-# LANGUAGE CPP                        #-}
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
import CESDS.Types.Request as Request (Request, identifier, onLoadBookmarkMeta, onCancel, onLoadModelsMeta, onLoadRecordsData', onRequest, onSaveBookmarkMeta, onWork)
import CESDS.Types.Response as Response (Response, bookmarkMetasResponse, chunkIdentifier, errorResponse, identifier, modelMetasResponse, nextChunkIdentifier, recordsResponse)
import CESDS.Types.Variable as Variable (VariableIdentifier)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Control.Monad (unless, void)
import Control.Monad.Except (ExceptT, MonadError, MonadIO, liftIO, runExceptT, throwError)
import Control.Monad.Except.Util (guardSomeException)
import Control.Monad.Reader (MonadReader, ReaderT, ask, lift, runReaderT)
import Control.Monad.Trans (MonadTrans)
import Data.Set as S (delete, empty, insert, member)
import Network.WebSockets (acceptRequest, runServer)
import Network.WebSockets.STM (Communicator, launch, send, start, waitToStop)

#ifdef CESDS_THROTTLE
import Control.Concurrent (threadDelay)

{-# INLINE pause #-}
pause :: IO ()
pause = threadDelay $ CESDS_THROTTLE * 1000

#else

{-# INLINE pause #-}
pause :: IO ()
pause = return ()
#endif


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
    cancellations <- newTVarIO S.empty
    -- Create a TVar for the model manager.
    manager <- newTVarIO initialManager
    -- Run a WebSocket server.
    runServer host port
      $ \pending ->
      do
        -- Accept all pending requests.
        connection <- acceptRequest pending
        -- Start threads for communication.
        communicator <- start connection (const id) (const ()) :: IO (Communicator Response Request ())
        -- Define a function to send chucks of records.
        let
          -- Send a single response.
          sendResponse rid toResponse x =
            do
              x' <- runServiceToIO manager x
              send communicator ()
                . (Response.identifier .~ rid)
                $ either errorResponse toResponse x'
              return False
          -- Send a stream of records.
          streamChunks rid chunker =
            do
              void
                . forkIO -- Do all of the work on a separate thread.
                $ do
                  result <- runServiceToIO manager chunker
                  case result of
                    Left  message   -> send communicator () $ errorResponse message & Response.identifier .~ rid
                    Right nextChunk -> sendChunks rid 1 nextChunk
              return False
          -- Send the next chunk in a stream of records.
          sendChunks rid cid nextChunk =
            do
              result <- runServiceToIO manager nextChunk
              cancelled <-
                atomically  -- FIXME: Clean this up?
                  $ do
                    cancellations' <- readTVar cancellations
                    modifyTVar' cancellations $ S.delete rid
                    return $ rid `S.member` cancellations'
              let
                lastChunk = cancelled || either (const True) null result
                cid' = cid + 1
              send communicator ()
                . (Response.identifier .~ rid                                     )
                . (chunkIdentifier     .~ Just cid                                )
                . (nextChunkIdentifier .~ if lastChunk then Nothing else Just cid')
                $ either errorResponse recordsResponse result
              pause
              unless lastChunk
                $ sendChunks rid cid' nextChunk
        -- Process all requests on a single thread, but queue the messages to be sent for the sender thread.
        launch communicator () -- FIXME: Cancellation won't work until we either have the sending thread filter or put workers on separate threads and have them filter.
          $ \request -> onRequest
            (
              -- Look metadata for model(s).
              onLoadModelsMeta
                $ sendResponse (request ^. Request.identifier) modelMetasResponse
                . lookupModels False
            )
            (
              -- Load records.
              onLoadRecordsData' -- FIXME: Do this on a separate thread.
                $ \model count variables maybeBookmarkOrFilter ->
                  streamChunks (request ^. Request.identifier)
                    $ do
                      -- Find the model.
                      [model'] <- lookupModels True $ Just model
                      loadContent model' maybeBookmarkOrFilter variables (fromIntegral <$> count) chunkSize
            )
            (
              -- Load metadata for bookmark(s)
              onLoadBookmarkMeta
                $ (sendResponse (request ^. Request.identifier) bookmarkMetasResponse .)
                .  lookupBookmarks
            )
            (
              -- Save a new bookmark.
              onSaveBookmarkMeta
                $ (sendResponse (request ^. Request.identifier) (bookmarkMetasResponse . (: [])) .)
                . saveBookmark
            )
            (
              -- Record a previous request as having been cancelled.
              onCancel
                $ \i ->
                do
                  liftIO . atomically $ modifyTVar' cancellations (S.insert $ Just i)
                  return False
            )
            (
              -- Dispatch more work.
              onWork -- FIXME: Do this on a separate thread.
                $ \model inputs ->
                  streamChunks (request ^. Request.identifier)
                    $ do
                      -- Find the model.
                      [model'] <- lookupModels True (Just model)
                      -- Do the work.
                      doWork model' inputs Nothing chunkSize
            )
            (
              -- Handle unknown requests.
              send communicator () (errorResponse "Unsupported request.")
                >> return False
            )
            request
        -- Don't exit until the receiver thread has closed.
        waitToStop communicator


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
              -> Maybe Int                                -- ^ The maximum number of records to return.
              -> Maybe Int                                -- ^ The number of records per chunk.
              -> ServiceM a (ServiceM a [RecordContent])  -- ^ Action returning an action for loading chunks of record data.

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
  doWork :: ModelMeta                               -- ^ The model metadata.
         -> [VarValue]                              -- ^ The values of the input variables.
         -> Maybe Int                               -- ^ The maximum number of records to return.
         -> Maybe Int                               -- ^ The number of records per chunk.
         -> ServiceM a (ServiceM a [RecordContent]) -- ^ Action to perform the work and return an action for loading chunks of the new records.


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
