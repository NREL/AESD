{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module CESDS.Types.Record (
  RecordIdentifier
, Record(..)
, validateRecord
) where


import CESDS.Types (Identifier, Val)
import CESDS.Types.Variable (Variable, VariableIdentifier, canHaveVal, hasVariable)
import Control.Arrow (second)
import Control.Monad.Except (MonadError)
import Control.Monad.Except.Util (assert)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), (.:), (.=), object, withObject)
import Data.Function (on)
import Data.HashMap.Strict (toList)
import Data.List (sortBy)
import Data.List.Util (noDuplicates)
import Data.String (IsString)
import GHC.Generics (Generic)

import qualified CESDS.Types.Variable as Variable (Variable(..))


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
      do
        variables <- o .: "variables"
        Record
          <$> sequence
          [
            (k, ) <$> parseJSON v
          |
            (k, v) <- toList variables
          ]

instance ToJSON Record where
  toJSON (Record kvs) = object ["variables" .= object (map (second toJSON) kvs)]


validateRecord :: (IsString e, MonadError e m) => [Variable] -> Record -> m ()
validateRecord variables Record{..} =
  do
    assert "duplicate variables in record" $ noDuplicates $ map fst unRecord
    sequence_
      [
        do
          variable' <- variables `hasVariable` variable
          Variable.domain variable' `canHaveVal` value
      |
        (variable, value) <- unRecord
      ]
