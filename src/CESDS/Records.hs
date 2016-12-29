{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}


module CESDS.Records (
  Cache
, ModelCache(..)
, ContentStatus(..)
) where


import CESDS.Types.Record (RecordContent)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)

import qualified CESDS.Types.Model as Model (ModelIdentifier, ModelMeta)


type Cache = Map Model.ModelIdentifier ModelCache


data ModelCache =
  ModelCache
  {
    modelMeta     :: Model.ModelMeta
  , recordContent :: [RecordContent]
  , contentStatus :: ContentStatus
  }
    deriving (Generic, Show)


data ContentStatus =
    EmptyContent
  | PendingContent
  | CompleteContent
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)
