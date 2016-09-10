{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Main (
  main
) where


import CESDS.Haystack.Cache.Singular (CacheManager, cacheSize, clearCache, makeCacheManager, refreshExtractCacheManager, timeKeys)
import CESDS.Server (RecordFilter(..), ServerM, Service(..), gets, modifys, runService, sets)
import CESDS.Types (Tags(..))
import CESDS.Types.Bookmark (Bookmark, BookmarkIdentifier, validateBookmark, validateBookmarks)
import CESDS.Types.Filter (Filter, FilterIdentifier, validateFilter, validateFilters)
import CESDS.Types.Model (Model, ModelIdentifier, validateModels)
import CESDS.Types.Record (Record(Record), RecordIdentifier)
import CESDS.Types.Server (Server(Server), validateServer)
import Control.Arrow ((***))
import Control.Monad (void)
import Control.Monad.Except (liftIO, throwError)
import Data.List.Util (hasSubset)
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import Data.Time.Util (getSecondsPOSIX)
import Data.UUID.V4 (nextRandom)
import Data.Yaml (decodeFile)
import NREL.Meters (Site(siteAccess), meters, siteModels)
import System.Environment (getArgs)

import qualified CESDS.Types.Bookmark as Bookmark (Bookmark(..))
import qualified CESDS.Types.Command as Command (Command(..), Result(..))
import qualified CESDS.Types.Filter as Filter (Filter(..))
import qualified CESDS.Types.Model as Model (Model(..))
import qualified CESDS.Types.Server as Server (Server(..), Status(Okay))
import qualified Data.HashMap.Strict as H


data ServerState =
  ServerState
  {
    server       :: Server
  , models       :: H.HashMap ModelIdentifier ModelState
  , cacheManager :: CacheManager
  }


data ModelState =
  ModelState
  {
    model        :: Model
  , bookmarks    :: [Bookmark]
  , filters      :: [Filter]
  }


withModelState :: ServerState -> ModelIdentifier -> (ModelState -> ModelState) -> ServerState
withModelState serverState@ServerState{..} modelIdentifier f =
  serverState
  {
    models = H.adjust f modelIdentifier models
  }


withModel :: ServerState -> ModelIdentifier -> (Model -> Model) -> ServerState
withModel serverState modelIdentifier f =
  withModelState serverState modelIdentifier $ \modelState@ModelState{..} ->
    modelState
    {
      model = f model
    }


withBookmarks :: ServerState -> BookmarkIdentifier -> ([Bookmark] -> [Bookmark]) -> ServerState
withBookmarks serverState modelIdentifier f =
  withModelState serverState modelIdentifier $ \modelState@ModelState{..} ->
    modelState
    {
      bookmarks = f bookmarks
    }


withFilters :: ServerState -> FilterIdentifier -> ([Filter] -> [Filter]) -> ServerState
withFilters serverState modelIdentifier f =
  withModelState serverState modelIdentifier $ \modelState@ModelState{..} ->
    modelState
    {
      filters = f filters
    }


recordIdentifiers :: ServerState -> ModelIdentifier -> [RecordIdentifier]
recordIdentifiers ServerState{..} modelIdentifier =
  map (pack . show)
    $ timeKeys modelIdentifier cacheManager


updateServerStats :: ServerState -> IO ServerState
updateServerStats serverState@ServerState{..} =
  do
    g <- getSecondsPOSIX
    return
      serverState
      {
        models = (\modelState@ModelState{..} -> modelState
                                                {
                                                  model = model
                                                          {
                                                            Model.generation  = g
                                                          , Model.recordCount = cacheSize (Model.identifier model) cacheManager
                                                          }
                                                }
                 ) <$> models
      }


updateModelStats :: ServerState -> ModelIdentifier -> IO ServerState
updateModelStats serverState@ServerState{..} modelIdentifier =
  do
    g <- getSecondsPOSIX
    return
      . withModel serverState modelIdentifier
      $ \model -> model
                  {
                    Model.generation  = g
                  , Model.recordCount = cacheSize modelIdentifier cacheManager
                  }


initialize :: Site -> IO ServerState
initialize site =
  do
    let
      models =
        H.fromList
        [
          (Model.identifier model, ModelState{..})
        |
          model <- siteModels site
        , let bookmarks = []
        , let filters   = []
        ]
      server =
        Server
        {
          Server.identifier = "CESDS Haystack"
        , typ               = "record_server"
        , version           = 1
        , models            = H.keys models
        , status            = Server.Okay "operating normally"
        }
    cacheManager <- makeCacheManager (siteAccess site) $ meters site
    updateServerStats ServerState{..}


reinitialize :: ServerState -> ServerState
reinitialize serverState@ServerState{..} =
  serverState
  {
    models       = reinitializeModel <$> models
  , cacheManager = clearCache cacheManager
  }


reinitializeModel :: ModelState -> ModelState
reinitializeModel modelState@ModelState{..} =
  modelState
  {
    bookmarks = []
  , filters   = []
  } -- FIXME


validateServerState :: ServerState -> ServerM s ()
validateServerState serverState@ServerState{..} =
  do
    validateServer server
    validateModels $ model <$> H.elems models
    sequence_
      [
        do
          validateBookmarks (recordIdentifiers serverState (Model.identifier model)) bookmarks
          validateFilters (Model.variables model) filters
      |
        ModelState{..} <- H.elems models
      ]


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
        modifys reinitialize
        return . Command.Result $ Just "server reinitialized"
    postServer (Command.Clear _) =
      postServer (Command.Restart [])
    postServer _ =
      throwError "bad request"
    getModel modelIdentifier =
      do
        (modelState, serverState) <- checkModel modelIdentifier
        serverState' <- liftIO $ updateModelStats serverState modelIdentifier
        sets serverState'
        return $ model modelState
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
        (_, serverState@ServerState{..}) <- checkModel modelIdentifier
        let
          recordIdentifier = read . unpack <$> rfRecord
        (cacheManager', rows) <- refreshExtractCacheManager cacheManager modelIdentifier recordIdentifier recordIdentifier
        sets serverState {cacheManager = cacheManager'}
        if null rows
          then throwError "record not found"
          else return . uncurry Record . (pack . show *** H.toList) $ head rows
    getRecords RecordFilter{..} modelIdentifier =
      do
        (_, serverState@ServerState{..}) <- checkModel modelIdentifier
        (cacheManager', rows) <- refreshExtractCacheManager cacheManager modelIdentifier rfFrom rfTo
        sets serverState {cacheManager = cacheManager'}
        return $ map (uncurry Record . (pack . show *** H.toList)) rows
    postRecord _ modelIdentifier =
      do
        void $ checkModel modelIdentifier
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
        void $ checkModel modelIdentifier
        throwError "model is read-only"
    deleteWork _ modelIdentifier =
      do
        void $ checkModel modelIdentifier
        throwError "model is read-only"
    getBookmarkList tags modelIdentifier =
      do
        (ModelState{..}, ServerState{..}) <- checkModel modelIdentifier
        return
          . map (\b -> b {Bookmark.records = Nothing})
          $ filterBookmarks tags Nothing bookmarks
    getBookmark bookmarkIdentifier modelIdentifier =
      do
        (ModelState{..}, _) <- checkModel modelIdentifier
        let
          bookmarks' = filterBookmarks (Tags []) (Just bookmarkIdentifier) bookmarks
        if null bookmarks'
          then throwError "bookmark not found"
          else return $ head bookmarks'
    postBookmark bookmark modelIdentifier =
      do
        (_, serverState) <- checkModel modelIdentifier
        (serverState', bookmark') <- addBookmark modelIdentifier serverState bookmark
        sets serverState'
        return bookmark'
    deleteBookmark _bookmarkIdentifier modelIdentifier =
      do
        void $ checkModel modelIdentifier
        throwError "unsupported operation"
    getFilterList tags modelIdentifier =
      do
        (ModelState{..}, _) <- checkModel modelIdentifier
        return
          . map (\f -> f {Filter.expression = Nothing})
          $ filterFilters tags Nothing filters
    getFilter filterIdentifier modelIdentifier =
      do
        (ModelState{..}, ServerState{..}) <- checkModel modelIdentifier
        let
          filters' = filterFilters (Tags []) (Just filterIdentifier) filters
        if null filters'
          then throwError "filter not found"
          else return $ head filters'
    postFilter filter' modelIdentifier =
      do
        (_, serverState) <- checkModel modelIdentifier
        (serverState', filter'') <- addFilter modelIdentifier serverState filter'
        sets serverState'
        return filter''
    deleteFilter _filterIdentifier modelIdentifier =
      do
        void $ checkModel modelIdentifier
        throwError "unsupported operation"
  in
    Service{..}


checkModel :: ModelIdentifier -> ServerM ServerState (ModelState, ServerState)
checkModel modelIdentifier =
  do
    serverState@ServerState{..} <- gets id
    maybe
      (throwError "model not found")
      (return . (, serverState))
      $ (modelIdentifier `H.lookup` models :: Maybe ModelState)


filterBookmarks :: Tags -> Maybe BookmarkIdentifier -> [Bookmark] -> [Bookmark]
filterBookmarks tags Nothing = filter ((`hasSubset` unTags tags) . unTags . fromMaybe (Tags []) . Bookmark.tags)
filterBookmarks tags bookmarkIdentifier = filterBookmarks tags Nothing . filter ((== bookmarkIdentifier) . Bookmark.identifier)


filterFilters :: Tags -> Maybe FilterIdentifier -> [Filter] -> [Filter]
filterFilters tags Nothing = filter ((`hasSubset` unTags tags) . unTags . fromMaybe (Tags []) . Filter.tags)
filterFilters tags filterIdentifier = filterFilters tags Nothing . filter ((== filterIdentifier) . Filter.identifier)
    

addBookmark :: ModelIdentifier -> ServerState -> Bookmark -> ServerM ServerState (ServerState, Bookmark)
addBookmark modelIdentifier serverState@ServerState{..} bookmark =
  do
    let
      modelState =  models H.! modelIdentifier
    validateBookmark (bookmarks modelState) (recordIdentifiers serverState modelIdentifier) bookmark
    bookmarkIdentifier <- Just . pack . show <$> liftIO nextRandom
    let
      bookmark' = bookmark {Bookmark.identifier = bookmarkIdentifier, Bookmark.size = length $ Bookmark.records bookmark}
    return
      . (, bookmark')
      $ withBookmarks serverState modelIdentifier (bookmark' :)


addFilter :: ModelIdentifier -> ServerState -> Filter -> ServerM ServerState (ServerState, Filter)
addFilter modelIdentifier serverState@ServerState{..} filter' =
  do
    let
      modelState =  models H.! modelIdentifier
    validateFilter (filters modelState) (Model.variables $ model modelState) filter'
    filterIdentifier <- Just . pack . show <$> liftIO nextRandom
    let
      filter'' = filter' {Filter.identifier = filterIdentifier}
    return
      . (, filter'')
      $ withFilters serverState modelIdentifier (filter'' :)


main :: IO ()
main =
  do
    [configurationFile, port] <- getArgs
    Just site <- decodeFile configurationFile
    runService (read port) service =<< initialize site
