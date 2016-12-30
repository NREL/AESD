{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module CESDS.Records.Server (
  ServiceM
, serviceM
, fromService
, modifyService
, ModelManager(..)
, State
, serverMain
) where


import CESDS.Types.Model as Model (ModelIdentifier, ModelMeta)
import CESDS.Types.Record (RecordIdentifier, RecordContent)
import CESDS.Types.Request as Request (onLoadModelsMeta, onLoadRecordsData, onRequest, identifier)
import CESDS.Types.Response as Response (chunkIdentifier, identifier, modelMetasResponse, nextChunkIdentifier, recordsResponse)
import CESDS.Types.Variable as Variable (VariableIdentifier)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens ((&))
import Control.Lens.Setter ((.~))
import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, MonadError, MonadIO, liftIO, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, ask, lift, runReaderT)
import Control.Monad.Trans (MonadTrans)
import Data.List.Split (chunksOf)
import Data.Maybe (maybeToList)
import Network.WebSockets (Connection, acceptRequest, receiveData, runServer, sendBinaryData)


newtype ServiceM s a = ServiceM {runServiceM :: ExceptT String (ReaderT (TVar s) IO) a}
  deriving (Applicative, Functor, MonadError String, Monad, MonadIO, MonadReader (TVar s))


serviceM :: MonadTrans t => ServiceM s a -> t (ServiceM s) a
serviceM = lift


fromService :: (s -> a) -> ServiceM s a
fromService f = f <$> (ask >>= liftIO . readTVarIO)


modifyService :: (s -> s) -> ServiceM s ()
modifyService f = ask >>= liftIO . atomically . flip modifyTVar' f


runServiceToIO :: TVar s -> ServiceM s a -> IO (Either String a)
runServiceToIO state service = runReaderT (runExceptT (runServiceM service)) state


class ModelManager a where
  listModels :: ServiceM a [ModelMeta]
  lookupModel :: ModelIdentifier -> ServiceM a (Maybe ModelMeta)
  loadContent :: [RecordIdentifier] -> [VariableIdentifier] -> ModelMeta -> ServiceM a [RecordContent]


lookupModels :: ModelManager a => Bool -> Maybe ModelIdentifier -> ServiceM a [ModelMeta]
lookupModels True  Nothing      = return []
lookupModels False Nothing      = listModels
lookupModels _     (Just model) = maybeToList <$> lookupModel model


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
              Right responses <- -- FIXME
                runServiceToIO manager
                  $ onRequest
                  (
                    onLoadModelsMeta
                      $ fmap (Just . (: []) . modelMetasResponse)
                      . lookupModels False
                  )
                  (
                    onLoadRecordsData
                      $ \maybeModel count variables Nothing -> -- FIXME: Handle bookmarks.
                      do
                        models <- lookupModels True (Just maybeModel)
                        recs <- concat <$> mapM (loadContent [] variables) models
                        return
                          $ Just
                          [
                            recordsResponse recs''
                              & chunkIdentifier .~ Just i'
                              & nextChunkIdentifier .~ (if fromIntegral i' < n then Just (i' + 1) else Nothing)
                          |
                            let recs' = chunksOf (maybe maxBound fromIntegral count) recs
                          , let n = length recs'
                          , (i', recs'') <- zip [1..] recs'
                          ]
                  )
                  undefined
                  undefined
                request
              forM_ responses
                . mapM_
                $ sendBinaryData connection
                . (Response.identifier .~ request ^. Request.identifier)
              loop
        loop
