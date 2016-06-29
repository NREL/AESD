{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TupleSections  #-}


module CESDS.Types.Record (
  RecordIdentifier
, Record(..)
) where


import CESDS.Types (Identifier, Val)
import CESDS.Types.Variable (VariableIdentifier)
import Control.Arrow (second)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), object, withObject)
import Data.Function (on)
import Data.HashMap.Strict (toList)
import Data.List (sortBy)
import GHC.Generics (Generic)


type RecordIdentifier = Identifier


newtype Record = Record {unRecord :: [(VariableIdentifier, Val)]}
  deriving (Generic, Read, Show)

instance Eq Record where
  Record x == Record y =
    sort x == sort y
      where
        sort = sortBy (compare `on` fst)

instance FromJSON Record where
  parseJSON =
    withObject "RECORD" $ \o ->
      Record
        <$> sequence
        [
          (k, ) <$> parseJSON v
        |
          (k, v) <- toList o
        ]

instance ToJSON Record where
  toJSON (Record kvs) = toJSON . object $ map (second toJSON) kvs
