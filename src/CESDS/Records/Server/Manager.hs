{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}


module CESDS.Records.Server.Manager ( -- FIXME: Should we export less?
  InMemoryManager
, makeInMemoryManager
, Cache
, ModelCache
, modelMeta
, recordContent
, bookmarkMetas
, contentStatus
, ContentStatus(..)
) where


import CESDS.Records.Server (ModelManager(..), ServiceM, fromService, modifyService, modifyService')
import CESDS.Types.Bookmark as Bookmark (BookmarkIdentifier, BookmarkMeta, identifier)
import CESDS.Types.Model as Model (ModelIdentifier, ModelMeta, identifier)
import CESDS.Types.Record (RecordContent, filterRecords, filterVariables)
import Control.Arrow ((&&&))
import Control.Concurrent.Util (makeCounter)
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', (&), lens)
import Control.Lens.Setter ((.~), over)
import Control.Monad.Except (liftIO, throwError)
import Data.Default (Default(..))
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)

import qualified Data.Map.Strict as M (elems, empty, fromList, insert, lookup, member, union, update)


type Cache = Map ModelIdentifier ModelCache


data InMemoryManager =
  InMemoryManager
  {
    cache'       :: Cache
  , nextBookmark :: IO BookmarkIdentifier
  , lister       :: IO [ModelMeta]
  , loader       :: ModelMeta -> IO [RecordContent]
  }

instance ModelManager InMemoryManager where
  listModels =
    do
      manager <- fromService id
      models <- liftIO $ lister manager
      let
        additions =
          M.fromList
            $ ((^. Model.identifier) &&& flip (modelMeta .~) def)
            <$> models
        -- FIXME: Should we also delete models not longer present?
      modifyService
        $ over cache (`M.union` additions)
      return models
  lookupModel model =
    (^. modelMeta)
      <$> checkModel model
  loadContent records variables model =
    do -- FIXME: Should we also cache the records?
      loader' <- fromService loader
      liftIO
        $ (if null variables then id else filterVariables variables)
        . (if null records   then id else filterRecords   records  )
        <$> loader' model
  listBookmarks model =
    M.elems . (^. bookmarkMetas)
      <$> checkModel model
  lookupBookmark model bookmark =
    maybe (throwError "Bookmark not found.") return
      =<< M.lookup bookmark . (^. bookmarkMetas)
      <$> checkModel model
  saveBookmark model bookmark =
    do
      i <- liftIO =<< fromService nextBookmark
      modifyService'
        $ \manager ->
        do
          m <-
            maybe (throwError "Model not found.") Right
              . M.lookup model
              $ manager ^. cache
          b <-
            case bookmark ^. Bookmark.identifier of
              Nothing -> return $ bookmark & Bookmark.identifier .~ Just i
              Just i' -> if i' `M.member` (m ^. bookmarkMetas) then return bookmark else throwError "Bookmark not found."
          return
            (
              manager
                & over cache
                (
                  M.update
                    (
                      Just . over bookmarkMetas
                      (
                        M.insert (fromJust $ b ^. Bookmark.identifier) b
                      )
                    )
                    model
                )
            , b
            )


checkModel :: ModelIdentifier -> ServiceM InMemoryManager ModelCache
checkModel model =
  maybe (throwError "Model not found.") return
    =<< fromService (M.lookup model . (^. cache))


cache :: Lens' InMemoryManager Cache
cache = lens cache' (\s x -> s {cache' = x})


makeInMemoryManager :: IO [ModelMeta] -> (ModelMeta -> IO [RecordContent]) -> IO InMemoryManager
makeInMemoryManager lister loader =
  do
    let
      cache' = M.empty 
    nextBookmark <- makeCounter (show . ((+ 1) :: Integer -> Integer) . read) "0"
    return InMemoryManager{..}


data ModelCache =
  ModelCache
  {
    modelMeta'     :: ModelMeta
  , recordContent' :: [RecordContent]
  , contentStatus' :: ContentStatus
  , bookmarkMetas' :: Map BookmarkIdentifier BookmarkMeta
  }
    deriving (Generic, Show)

instance Default ModelCache where
  def = ModelCache def [] def M.empty


modelMeta :: Lens' ModelCache ModelMeta
modelMeta = lens modelMeta' (\s x -> s {modelMeta' = x})


recordContent :: Lens' ModelCache [RecordContent]
recordContent = lens recordContent' (\s x -> s {recordContent' = x})


contentStatus :: Lens' ModelCache ContentStatus
contentStatus = lens contentStatus' (\s x -> s {contentStatus' = x})


bookmarkMetas :: Lens' ModelCache (Map BookmarkIdentifier BookmarkMeta)
bookmarkMetas = lens bookmarkMetas' (\s x -> s {bookmarkMetas' = x})


data ContentStatus =
    EmptyContent
  | PendingContent
  | CompleteContent
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance Default ContentStatus where
  def = EmptyContent
