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
import Control.Monad.Except (MonadError)
import Control.Monad.Except.Util (assert)
import Data.Aeson.Types (FromJSON(..), ToJSON(..), (.:), (.=), object, withArray, withObject)
import Data.Function (on)
import Data.List (sortBy)
import Data.List.Util (noDuplicates)
import Data.Vector (toList)
import Data.String (IsString)
import GHC.Generics (Generic)

import qualified CESDS.Types.Variable as Variable (Variable(..))


type RecordIdentifier = Identifier


data Record =
  Record
  {
    recordIdentifier :: RecordIdentifier
  , recordValues     :: [(VariableIdentifier, Val)]
  }
    deriving (Generic, Read, Show)

instance Eq Record where
  x == y =
    recordIdentifier x == recordIdentifier y
    &&
    sort (recordValues x) == sort (recordValues y)
      where
        sort = sortBy (compare `on` fst)

instance FromJSON Record where
  parseJSON =
    withObject "RECORD" $ \o ->
      do
        recordIdentifier <- o .: "id"
        recordValues <- withArray "VAR_VALUED_LIST"
                          (\a ->
                            sequence
                              [
                                withObject "VAR_VALUED_LIST"
                                  (\o'' ->
                                    do
                                      k <- o'' .: "id"
                                      v <- o'' .: "value"
                                      return (k, v)
                                  ) o'
                              |
                                o' <- toList a
                              ]
                          ) =<< o .: "variables"
                        
        return Record{..}

instance ToJSON Record where
  toJSON Record{..} =
    object
      [
        "id"        .= recordIdentifier
      , "variables" .= [
                         object ["id" .= k, "value" .= v]
                       |
                         (k, v) <-recordValues
                       ]
      ]


validateRecord :: (IsString e, MonadError e m) => [Variable] -> Record -> m ()
validateRecord variables Record{..} =
  do
    assert "duplicate variables in record" $ noDuplicates $ map fst recordValues
    sequence_
      [
        do
          variable' <- variables `hasVariable` variable
          Variable.domain variable' `canHaveVal` value
      |
        (variable, value) <- recordValues
      ]
