{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}


module CESDS.Records (
  Cache
, ModelCache(..)
, ContentStatus(..)
) where


import CESDS.Types.Bookmark (BookmarkIdentifier, BookmarkMeta)
import CESDS.Types.Model (ModelIdentifier, ModelMeta)
import CESDS.Types.Record (RecordContent)
import Data.Default (Default(..))
import Data.Map.Strict (Map, empty)
import GHC.Generics (Generic)


type Cache = Map ModelIdentifier ModelCache


data ModelCache =
  ModelCache
  {
    modelMeta     :: ModelMeta
  , recordContent :: [RecordContent]
  , contentStatus :: ContentStatus
  , bookmarkMetas :: Map BookmarkIdentifier BookmarkMeta
  }
    deriving (Generic, Show)

instance Default ModelCache where
  def = ModelCache def [] def empty


data ContentStatus =
    EmptyContent
  | PendingContent
  | CompleteContent
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

instance Default ContentStatus where
  def = EmptyContent
