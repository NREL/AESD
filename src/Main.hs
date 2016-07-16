{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main (
  main
) where


import CESDS.Haystack (HaystackAccess)
import CESDS.Haystack.Cache (CacheManager(access, cache), makeCacheManager, refreshExtractCacheManager, timeKeys)
import CESDS.Server (RecordFilter(..), ServerM, Service(..), gets, runService, sets)
import CESDS.Types (Tags(..))
import CESDS.Types.Bookmark (Bookmark, BookmarkIdentifier, validateBookmark, validateBookmarks)
import CESDS.Types.Filter (Filter, FilterIdentifier, validateFilter, validateFilters)
import CESDS.Types.Model (Model(Model), ModelIdentifier, validateModel)
import CESDS.Types.Record (Record(Record), RecordIdentifier)
import CESDS.Types.Server (Server(Server), validateServer)
import Control.Arrow ((***))
import Control.Monad (void)
import Control.Monad.Except (liftIO, throwError)
import Control.Monad.Except.Util (assert)
import Data.List.Util (hasSubset)
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import Data.Time.Util (getSecondsPOSIX)
import Data.UUID.V4 (nextRandom)
import Data.Yaml (decodeFile)
import NREL.Meters (metersRSF2, modelRSF2)
import System.Environment (getArgs)

import qualified CESDS.Types.Bookmark as Bookmark (Bookmark(..))
import qualified CESDS.Types.Command as Command (Command(..), Result(..))
import qualified CESDS.Types.Filter as Filter (Filter(..))
import qualified CESDS.Types.Model as Model (Model(..))
import qualified CESDS.Types.Server as Server (Server(..), Status(Okay))
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Strict as M


data ServerState =
  ServerState
  {
    server       :: Server
  , model        :: Model
  , cacheManager :: CacheManager
  , bookmarks    :: [Bookmark]
  , filters      :: [Filter]
  }
    deriving (Eq, Read, Show)


recordIdentifiers :: ServerState -> [RecordIdentifier]
recordIdentifiers ServerState{..} = map (pack . show) $ timeKeys cacheManager


updateServerStats :: ServerState -> IO ServerState
updateServerStats serverState@ServerState{..} =
  do
    g <- getSecondsPOSIX
    return
      serverState
      {
        model = model
                {
                  Model.generation  = g
                , Model.recordCount = M.size $ cache cacheManager
                }
      }


initialize :: HaystackAccess -> IO ServerState
initialize access =
  do
    let
      model = modelRSF2
      server =
        Server
        {
          Server.identifier = "CESDS Haystack"
        , typ               = "record_server"
        , version           = 1
        , models            = [Model.identifier model]
        , status            = Server.Okay "operating normally"
        }
      bookmarks = []
      filters = []
    cacheManager <- makeCacheManager access metersRSF2
    updateServerStats ServerState{..}


validateServerState :: ServerState -> ServerM s ()
validateServerState serverState@ServerState{..} =
  do
    let
      Model{..} = model
    validateServer server
    validateModel [] model
    validateBookmarks (recordIdentifiers serverState) bookmarks
    validateFilters variables filters


service :: Service ServerState
service =
  let
    getServer =
      do
        serverState@ServerState{..} <- liftIO . updateServerStats =<< gets id
        validateServerState serverState
        sets serverState
        return server
    postServer (Command.Restart _) =
      do
        ServerState{..} <- gets id
        sets =<< liftIO (initialize $ access cacheManager)
        return . Command.Result $ Just "server reinitialized"
    postServer (Command.Clear _) =
      postServer (Command.Restart [])
    postServer _ =
      throwError "bad request"
    getModel modelIdentifier =
      do
        serverState@ServerState{..} <- liftIO . updateServerStats =<< checkModel modelIdentifier
        sets serverState
        return model
    postModel (Command.Restart _) modelIdentifier =
      do
        void $ checkModel modelIdentifier
        postServer (Command.Restart [])
    postModel (Command.Clear _) modelIdentifier =
      postModel (Command.Restart []) modelIdentifier
    postModel (Command.SetStrategy _) modelIdentifier =
      do
        void $ checkModel modelIdentifier
        return . Command.Result $ Just "ignored: model is read-only"
    postModel (Command.GetStrategy _) modelIdentifier =
      postModel (Command.SetStrategy []) modelIdentifier
    getRecord RecordFilter{..} modelIdentifier =
      do
        serverState@ServerState{..} <- checkModel modelIdentifier
        let
          recordIdentifier = read . unpack <$> rfRecord
        (cacheManager', rows) <- refreshExtractCacheManager cacheManager recordIdentifier recordIdentifier
        sets serverState {cacheManager = cacheManager'}
        if null rows
          then throwError "record not found"
          else return . uncurry Record . (pack . show *** H.toList) $ head rows
    getRecords RecordFilter{..} modelIdentifier =
      do
        serverState@ServerState{..} <- checkModel modelIdentifier
        (cacheManager', rows) <- refreshExtractCacheManager cacheManager rfFrom rfTo
        sets serverState {cacheManager = cacheManager'}
        return $ map (uncurry Record . (pack . show *** H.toList)) rows
    postRecord _ modelIdentifier =
      do
        ServerState{..} <- checkModel modelIdentifier
        throwError "model is read-only"
    getWork _ modelIdentifier =
      do
        void $ checkModel modelIdentifier
        throwError "model is read-only"
    getWorks _ modelIdentifier =
      do
        void $ checkModel modelIdentifier
        throwError "model is read-only"
    postWork _ modelIdentifier =
      do
        ServerState{..} <- checkModel modelIdentifier
        throwError "model is read-only"
    deleteWork _ modelIdentifier =
      do
        ServerState{..} <- checkModel modelIdentifier
        throwError "model is read-only"
    getBookmarkList tags modelIdentifier =
      do
        ServerState{..} <- checkModel modelIdentifier
        return
          . map (\b -> b {Bookmark.records = Nothing})
          $ filterBookmarks tags Nothing bookmarks
    getBookmark bookmarkIdentifier modelIdentifier =
      do
        ServerState{..} <- checkModel modelIdentifier
        let
          bookmarks' = filterBookmarks (Tags []) (Just bookmarkIdentifier) bookmarks
        if null bookmarks'
          then throwError "bookmark not found"
          else return $ head bookmarks'
    postBookmark bookmark modelIdentifier =
      do
        serverState <- checkModel modelIdentifier
        (serverState', bookmark') <- addBookmark serverState bookmark
        sets serverState'
        return bookmark'
    deleteBookmark _bookmarkIdentifier modelIdentifier =
      do
        ServerState{..} <- checkModel modelIdentifier
        throwError "unsupported operation"
    getFilterList tags modelIdentifier =
      do
        ServerState{..} <- checkModel modelIdentifier
        return
          . map (\f -> f {Filter.expression = Nothing})
          $ filterFilters tags Nothing filters
    getFilter filterIdentifier modelIdentifier =
      do
        ServerState{..} <- checkModel modelIdentifier
        let
          filters' = filterFilters (Tags []) (Just filterIdentifier) filters
        if null filters'
          then throwError "filter not found"
          else return $ head filters'
    postFilter filter' modelIdentifier =
      do
        serverState <- checkModel modelIdentifier
        (serverState', filter'') <- addFilter serverState filter'
        sets serverState'
        return filter''
    deleteFilter _filterIdentifier modelIdentifier =
      do
        ServerState{..} <- checkModel modelIdentifier
        throwError "unsupported operation"
  in
    Service{..}


checkModel :: ModelIdentifier -> ServerM ServerState ServerState
checkModel modelIdentifier =
  do
    serverState@ServerState{..} <- gets id
    assert "model not found" $ modelIdentifier == Model.identifier model
    return serverState


filterBookmarks :: Tags -> Maybe BookmarkIdentifier -> [Bookmark] -> [Bookmark]
filterBookmarks tags Nothing = filter ((`hasSubset` unTags tags) . unTags . fromMaybe (Tags []) . Bookmark.tags)
filterBookmarks tags bookmarkIdentifier = filterBookmarks tags Nothing . filter ((== bookmarkIdentifier) . Bookmark.identifier)


filterFilters :: Tags -> Maybe FilterIdentifier -> [Filter] -> [Filter]
filterFilters tags Nothing = filter ((`hasSubset` unTags tags) . unTags . fromMaybe (Tags []) . Filter.tags)
filterFilters tags filterIdentifier = filterFilters tags Nothing . filter ((== filterIdentifier) . Filter.identifier)
    

addBookmark :: ServerState -> Bookmark -> ServerM ServerState (ServerState, Bookmark)
addBookmark serverState@ServerState{..} bookmark =
  do
    validateBookmark bookmarks (recordIdentifiers serverState) bookmark
    bookmarkIdentifier <- Just . pack . show <$> liftIO nextRandom
    let
      bookmark' = bookmark {Bookmark.identifier = bookmarkIdentifier, Bookmark.size = length $ Bookmark.records bookmark}
    return (serverState {bookmarks = bookmark' : bookmarks}, bookmark')


addFilter :: ServerState -> Filter -> ServerM ServerState (ServerState, Filter)
addFilter ms@ServerState{..} filter' =
  do
    validateFilter filters (Model.variables model) filter'
    filterIdentifier <- Just . pack . show <$> liftIO nextRandom
    let
      filter'' = filter' {Filter.identifier = filterIdentifier}
    return (ms {filters = filter'' : filters}, filter'')


main :: IO ()
main =
  do
    [configurationFile] <- getArgs
    Just access <- decodeFile configurationFile
    runService 8091 service =<< initialize access
