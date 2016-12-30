{-# LANGUAGE DeriveGeneric   #-}


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


import CESDS.Records.Server (ModelManager(..), fromService, modifyService)
import CESDS.Types.Bookmark (BookmarkIdentifier, BookmarkMeta)
import CESDS.Types.Model as Model (ModelIdentifier, ModelMeta, identifier)
import CESDS.Types.Record (RecordContent, filterRecords, filterVariables)
import Control.Arrow ((&&&))
import Control.Lens.Getter ((^.))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Setter ((.~), over)
import Control.Monad.Except (liftIO)
import Data.Default (Default(..))
import Data.Map.Strict (Map, empty)
import GHC.Generics (Generic)

import qualified Data.Map.Strict as M (empty, fromList, lookup, union)


type Cache = Map ModelIdentifier ModelCache


data InMemoryManager =
  InMemoryManager
  {
    cache' :: Cache
  , lister :: IO [ModelMeta]
  , loader :: ModelMeta -> IO [RecordContent]
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
    fromService
      $ fmap (^. modelMeta)
      . M.lookup model
      . (^. cache)
  loadContent records variables model =
    do -- FIXME: Should we also cache the records?
      loader' <- fromService loader
      liftIO
        $ (if null variables then id else filterVariables variables)
        . (if null records   then id else filterRecords   records  )
        <$> loader' model


cache :: Lens' InMemoryManager Cache
cache = lens cache' (\s x -> s {cache' = x})


makeInMemoryManager :: IO [ModelMeta] -> (ModelMeta -> IO [RecordContent]) -> InMemoryManager
makeInMemoryManager = InMemoryManager M.empty


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
  def = ModelCache def [] def empty


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
