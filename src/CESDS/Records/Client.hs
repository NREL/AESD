{-# LANGUAGE TupleSections #-}


module CESDS.Records.Client (
  State
, clientMain
, fetchModels
, fetchRecords
, fetchBookmarks
, storeBookmark
, close
) where


import CESDS.Records.Server.Manager (Cache, ContentStatus(..), contentStatus, modelMeta, recordContent)
import CESDS.Types.Bookmark as Bookmark (BookmarkIdentifier, BookmarkMeta)
import CESDS.Types.Model as Model (ModelIdentifier, ModelMeta, identifier)
import CESDS.Types.Record (RecordContent)
import CESDS.Types.Request as Request (Request, identifier, loadBookmarkMeta, loadModelsMeta, loadRecordsData, saveBookmarkMeta)
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
import Data.Int (Int32)
import Data.Maybe (fromJust, fromMaybe)
import Network.WebSockets (Connection, receiveData, runClient, sendBinaryData)

import qualified Data.Map.Strict as M ((!), delete, empty, foldr, fromList, insert, lookup, member)


clientMain :: String -> Int -> String -> (State -> IO ()) -> IO ()
clientMain host port path client =
  runClient host port path
    $ (client =<<)
    . makeModelCache


type State = (ThreadId, Processor, TVar Cache)


close :: State -> IO ()
close (thread, _, _) = killThread thread


fetchRecords :: State -> ModelIdentifier -> IO (Either String [RecordContent])
fetchRecords (_, processor, modelCache) i =
  do
    found <- M.member i <$> atomically (readTVar modelCache)
    if found
      then do -- FIXME: Generalize 'accumulate' to this case, too?
             result <- newEmptyMVar
             processor
               (loadRecordsData i Nothing [] Nothing)
               $ \response ->
               do
                 let
                    done = response ^. nextChunkIdentifier <= Just 0
                 recs <- onResponse handleError ignore keep ignore unexpected response
                 recs' <-
                   case recs of
                     Left message -> return $ Left message
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
                 when done
                   $ putMVar result recs'
                 return done
             takeMVar result
      else return $ Right []
        


fetchModels :: State -> IO (Either String [ModelMeta])
fetchModels (_, _, modelCache) =
  Right . M.foldr ((:) . (^. modelMeta)) []
    <$> atomically (readTVar modelCache)


accumulate :: Monad m => ((Response -> IO Bool) -> IO ()) -> (Response -> IO (m [t])) -> IO (m [t])
accumulate responder processor =
  do
    result <- newEmptyMVar
    xs <- newTVarIO $ return []
    responder
      $ \response ->
      do
        let
          done = response ^. nextChunkIdentifier <= Just 0
        x <- processor response
        xs' <-
          atomically
            $ do
              modifyTVar' xs $ liftM2 (++) x
              readTVar xs
        when done
          $ putMVar result xs'
        return done
    takeMVar result


fetchBookmarks :: State -> ModelIdentifier -> Maybe BookmarkIdentifier -> IO (Either String [BookmarkMeta])
fetchBookmarks (_, processor, _) model bookmark =
  accumulate
    (processor $ loadBookmarkMeta model bookmark)
    $ onResponse handleError ignore ignore keep unexpected


storeBookmark :: State -> ModelIdentifier -> BookmarkMeta -> IO (Either String BookmarkMeta)
storeBookmark (_, processor, _) model bookmark =
  do
    result <- newEmptyMVar
    processor
      (saveBookmarkMeta model bookmark)
      $ \response ->
      do
        bookmark' <- fmap head <$> onResponse handleError ignore ignore keep unexpected response
        putMVar result bookmark'
        return True
    takeMVar result


makeModelCache :: Connection -> IO State
makeModelCache connection =
  do
    (thread, processor) <- makeProcessor connection
    models <-
      either error id
      <$> accumulate
        (processor $ loadModelsMeta Nothing)
        (onResponse handleError keep ignore ignore unexpected)
    fmap (thread, processor, )
      . newTVarIO
      $ M.fromList
      [
        (
          model ^. Model.identifier
        , def & modelMeta .~ model
        )
      |
        model <- models
      ]


handleError :: Maybe Int32 -> String -> IO (Either String a)
handleError = const $ return . Left


unexpected :: Either String a
unexpected = Left "Unexpected result."


ignore :: Maybe Int32 -> a -> IO (Either String b)
ignore = const . const $ return unexpected


keep :: Maybe Int32 -> a -> IO (Either String a)
keep = const $ return . Right


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
                      i = response ^. Response.identifier
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
              request' = (Request.identifier .~ Just i) request
            liftIO
              $ do
                atomically
                  . modifyTVar' handlers 
                  $ M.insert i handler
                sendBinaryData connection request'
      )
