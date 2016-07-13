{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CESDS.Types.Model (
  ModelIdentifier
, Model(..)
, validateModels
, validateModel
) where


import CESDS.Types (Generation, Identifier, Tags, object')
import CESDS.Types.Variable (Variable, VariableIdentifier, validateVariable)
import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Except.Util (assert)
import Data.Aeson.Types (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.:?), (.=), withObject)
import Data.List (delete)
import Data.List.Util (noDuplicates, notDuplicatedIn)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.URI (URI)

import qualified CESDS.Types.Variable as Variable (Variable(..))


type ModelIdentifier = Identifier


data Model =
  Model
  {
    identifier  :: ModelIdentifier
  , uri         :: Maybe URI
  , name        :: Text
  , description :: Maybe Text
  , tags        :: Maybe Tags
  , generation  :: Generation
  , recordCount :: Int
  , variables   :: [Variable]
  , primaryKey  :: VariableIdentifier
  , timeKey     :: Maybe VariableIdentifier
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Model where
  parseJSON = withObject "MODEL" $ \o ->
                do
                  identifier  <- o .:  "model_id"
                  uri         <- o .:? "model_uri"
                  name        <- o .:  "label"
                  description <- o .:? "description"
                  tags        <- o .:? "tags"
                  generation  <- o .:  "generation"
                  recordCount <- o .:  "record_count"
                  variables   <- o .:  "variables"
                  primaryKey  <- o .:  "record_id_var"
                  timeKey     <- o .:? "time_key"
                  return Model{..}

instance ToJSON Model where
  toJSON Model{..} = object'
                       [
                         "model_id"      .= identifier
                       , "model_uri"     .= uri
                       , "label"         .= name
                       , "description"   .= description
                       , "tags"          .= tags
                       , "generation"    .= generation
                       , "record_count"  .= recordCount
                       , "variables"     .= variables
                       , "record_id_var" .= primaryKey
                       , "time_key"      .= timeKey
                       ]


validateModels :: (IsString e, MonadError e m) => [Model] -> m ()
validateModels models =
  do
    let
      modelIdentifiers = identifier <$> models
    unless (noDuplicates modelIdentifiers)
      $ throwError "duplicate model identifiers"
    sequence_
      [
        validateModel (identifier model `delete` modelIdentifiers) model
      |
        model <- models
      ]


validateModel :: (IsString e, MonadError e m) => [ModelIdentifier] -> Model -> m ()
validateModel modelIdentifiers Model{..} =
  do
    assert "duplicate model identifiers" $ notDuplicatedIn id identifier modelIdentifiers
    let
      variables' = map Variable.identifier variables
    assert "duplicate variable identifiers" $ noDuplicates variables'
    assert "invalid record key" $ primaryKey `elem` variables'
    assert "invalid time key" $ maybe True (`elem` variables') timeKey
    sequence_
      [
        validateVariable (Variable.identifier variable `delete` variables') variable
      |
        variable <- variables
      ]
