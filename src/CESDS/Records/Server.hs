{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CESDS.Records.Server (
  ServiceM
, serviceM
, fromService
, modifyService
, modifyService'
, modifyServiceIO'
, ModelManager(..)
, State
, serverMain
) where


import CESDS.Types.Bookmark as Bookmark (BookmarkIdentifier, BookmarkMeta)
import CESDS.Types.Model as Model (ModelIdentifier, ModelMeta)
import CESDS.Types.Record (RecordContent)
import CESDS.Types.Request as Request (identifier, onLoadBookmarkMeta, onLoadModelsMeta, onLoadRecordsData, onRequest, onSaveBookmarkMeta)
import CESDS.Types.Response as Response (bookmarkMetasResponse, chunkIdentifier, errorResponse, identifier, modelMetasResponse, nextChunkIdentifier, recordsResponse)
import CESDS.Types.Variable as Variable (VariableIdentifier)
import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, MonadError, MonadIO, liftIO, runExceptT, throwError)
import Control.Monad.Except.Util (guardIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, lift, runReaderT)
import Control.Monad.Trans (MonadTrans)
import Data.List.Split (chunksOf)
import Network.WebSockets (Connection, acceptRequest, receiveData, runServer, sendBinaryData)


newtype ServiceM s a = ServiceM {runServiceM :: ExceptT String (ReaderT (TVar s) IO) a}
  deriving (Applicative, Functor, MonadError String, Monad, MonadIO, MonadReader (TVar s))


serviceM :: MonadTrans t => ServiceM s a -> t (ServiceM s) a
serviceM = lift


fromService :: (s -> a) -> ServiceM s a
fromService f = f <$> (ask >>= liftIO . readTVarIO)


modifyService :: (s -> s) -> ServiceM s ()
modifyService f = ask >>= liftIO . atomically . flip modifyTVar' f


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


modifyServiceIO' :: (s -> IO (Either String (s, a))) -> ServiceM s a
modifyServiceIO' f =
  do -- FIXME: Make this atomic.
    sTVar <- ask
    s <- liftIO $ readTVarIO sTVar
    sx <- guardIO $ f s
    case sx of
      Left message  -> throwError message
      Right (s', x) -> do
                         liftIO . atomically $ writeTVar sTVar s'
                         return x


runServiceToIO :: TVar s -> ServiceM s a -> IO (Either String a)
runServiceToIO state service = runReaderT (runExceptT (runServiceM service)) state


class ModelManager a where
  listModels :: ServiceM a [ModelMeta]
  lookupModel :: ModelIdentifier -> ServiceM a ModelMeta
  loadContent :: ModelMeta -> Maybe BookmarkIdentifier -> [VariableIdentifier] -> ServiceM a [RecordContent]
  listBookmarks :: ModelIdentifier -> ServiceM a [BookmarkMeta]
  lookupBookmark :: ModelIdentifier -> BookmarkIdentifier -> ServiceM a BookmarkMeta
  saveBookmark :: ModelIdentifier -> BookmarkMeta -> ServiceM a BookmarkMeta


lookupModels :: ModelManager a => Bool -> Maybe ModelIdentifier -> ServiceM a [ModelMeta]
lookupModels True  Nothing      = return []
lookupModels False Nothing      = listModels
lookupModels _     (Just model) = (: []) <$> lookupModel model


lookupBookmarks :: ModelManager a => ModelIdentifier -> Maybe BookmarkIdentifier -> ServiceM  a [BookmarkMeta]
lookupBookmarks model Nothing         = listBookmarks model
lookupBookmarks model (Just bookmark) = (: []) <$> lookupBookmark model bookmark


type State a = (TVar a, Connection)


serverMain :: ModelManager a
           => String
           -> Int
           -> a
           -> IO ()
serverMain host port initialManager =
  do
    manager <- newTVarIO initialManager
    runServer host port
      $ \pending ->
      do
        connection <- acceptRequest pending
        let
          loop =
            do
              request <- receiveData connection
              result <-
                runServiceToIO manager -- FIXME: If the ModelManager uses guardIO, then no IOExceptions ever show up here?
                  $ onRequest
                  (
                    onLoadModelsMeta
                      $ fmap ((: []) . modelMetasResponse)
                      . lookupModels False
                  )
                  (
                    onLoadRecordsData
                      $ \model count variables maybeBookmark ->
                      do
                        [model'] <- lookupModels True $ Just model
                        recs <- loadContent model' maybeBookmark variables
                        return
                          $ 
                          [
                            recordsResponse recs''
                              & chunkIdentifier .~ Just i'
                              & nextChunkIdentifier .~ (if fromIntegral i' < n then Just (i' + 1) else Nothing)
                          |
                            let recs' = chunksOf (maybe maxBound fromIntegral count) recs
                          , let n = length recs'
                          , (i', recs'') <- zip [1..] recs'
                          ]
                          <|>
                          [
                            recordsResponse []
                          ]
                  )
                  (
                    onLoadBookmarkMeta
                      $ (fmap ((: []) . bookmarkMetasResponse) .)
                      . lookupBookmarks
                  )
                  (
                    onSaveBookmarkMeta
                     $ (fmap ((: []) . bookmarkMetasResponse . (: [])) . )
                     . saveBookmark
                  )
                  [errorResponse "Unsupported request."]
                request
              forM_ (either ((: []) . errorResponse) id result)
                $ sendBinaryData connection
                . (Response.identifier .~ request ^. Request.identifier)
              loop
        loop
