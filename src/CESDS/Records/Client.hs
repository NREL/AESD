{-# LANGUAGE TupleSections #-}


module CESDS.Records.Client (
  State
, clientMain
, fetchModels
, fetchRecords
, close
) where


import CESDS.Records (Cache, ContentStatus(..), ModelCache(..))
import CESDS.Types.Record (RecordContent)
import CESDS.Types.Request (Request, loadModelsMeta, loadRecordsData, requestIdentifier)
import CESDS.Types.Response (Response, identifier, nextChunkIdentifier, onResponse)
import Control.Concurrent.Util (makeCounter)
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((.~))
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, writeTVar)
import Control.Monad (join, when)
import Control.Monad.Except (liftIO)
import Data.Int (Int32)
import Data.Maybe (fromJust, fromMaybe)
import Network.WebSockets (Connection, receiveData, runClient, sendBinaryData)

import qualified CESDS.Types.Model as Model (ModelIdentifier, ModelMeta, identifier)
import qualified Data.Map.Strict as M ((!), delete, empty, foldr, fromList, insert, lookup, member)


clientMain :: String -> Int -> String -> (State -> IO ()) -> IO ()
clientMain host port path client =
  runClient host port path
    $ (client =<<)
    . makeModelCache


type State = (ThreadId, Processor, TVar Cache)


close :: State -> IO ()
close (thread, _, _) = killThread thread


fetchRecords :: State -> Model.ModelIdentifier -> IO [RecordContent]
fetchRecords (_, processor, modelCache) i =
  do
    found <- M.member i <$> atomically (readTVar modelCache)
    if found
      then do
             result <- newEmptyMVar
             processor
               (loadRecordsData i Nothing [] Nothing)
               $ \response ->
               do
                 let
                    done = response ^. nextChunkIdentifier <= Just 0
                 Just recs <- onResponse ignore ignore keep ignore response
                 recs' <-
                   atomically
                     $ do
                       cache <- readTVar modelCache
                       let
                         model = cache M.! i
                         model' = model
                                   {
                                     recordContent = recordContent model ++ recs
                                   , contentStatus = if done then CompleteContent else PendingContent
                                   }
                       writeTVar modelCache $ M.insert i model' cache
                       return $ recordContent model'
                 when done
                   $ putMVar result recs'
                 return done
             takeMVar result
      else return []
        


fetchModels :: State -> IO [Model.ModelMeta]
fetchModels (_, _, modelCache) =
  M.foldr (\m ms -> modelMeta m : ms) []
    <$> atomically (readTVar modelCache)


makeModelCache :: Connection -> IO State
makeModelCache connection =
  do
    (thread, processor) <- makeProcessor connection
    result <- newEmptyMVar
    processor
      (loadModelsMeta Nothing)
      $ \response ->
      do
        putMVar result =<< onResponse ignore keep ignore ignore response
        return True
    models <- fromMaybe [] <$> takeMVar result
    fmap (thread, processor, )
      . newTVarIO
      $ M.fromList
      [
        (
          model ^. Model.identifier
        , ModelCache model [] EmptyContent
        )
      |
        model <- models
      ]


ignore :: Maybe Int32 -> a -> IO (Maybe b)
ignore = const . const $ return Nothing


keep :: Maybe Int32 -> a -> IO (Maybe a)
keep = const $ return . Just


type Processor = Request -> Handler -> IO ()


type Handler = Response -> IO Bool


makeProcessor :: Connection -> IO (ThreadId, Processor)
makeProcessor connection =
  do
    handlers <- liftIO $ newTVarIO M.empty
    nextIdentifier <- liftIO $ makeCounter (+ 1) (0 :: Int32)
    receiver <-
      forkIO
        $ let
            findHandler = (<$> atomically (readTVar handlers)) . M.lookup
            loop =
              do
                liftIO
                  $ do
                    response <- receiveData connection
                    let
                      i = response ^. identifier
                    f <- fmap join . sequence $ findHandler <$> i
                    done <- fmap (fromMaybe False) . sequence $ ($ response) <$> f
                    when done
                      . atomically
                      . modifyTVar' handlers
                      . M.delete
                      $ fromJust i
                loop
          in
            loop
    return
      (
        receiver
      , \request handler ->
          do
            i <- liftIO nextIdentifier
            let
              request' = (requestIdentifier .~ Just i) request
            liftIO
              $ do
                atomically
                  . modifyTVar' handlers 
                  $ M.insert i handler
                sendBinaryData connection request'
      )
