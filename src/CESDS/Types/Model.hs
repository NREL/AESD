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
  , uri         :: URI
  , name        :: Maybe Text
  , description :: Maybe Text
  , tags        :: Tags
  , generation  :: Generation
  , variables   :: [Variable]
  , primaryKey  :: VariableIdentifier
  , timeKey     :: Maybe VariableIdentifier
  }
    deriving (Eq, Generic, Read, Show)

instance FromJSON Model where
  parseJSON = withObject "MODEL" $ \o ->
                do
                  identifier  <- o .:  "model_id"
                  uri         <- o .:  "model_uri"
                  name        <- o .:? "label"
                  description <- o .:? "description"
                  tags        <- o .:  "tags"
                  generation  <- o .:  "generation"
                  variables   <- o .:  "variables"
                  primaryKey  <- o .:  "primary_key"
                  timeKey     <- o .:? "time_key"
                  return Model{..}

instance ToJSON Model where
  toJSON Model{..} = object'
                       [
                         "model_id"    .= identifier
                       , "model_uri"   .= uri
                       , "label"       .= name
                       , "description" .= description
                       , "tags"        .= tags
                       , "generation"  .= generation
                       , "variables"   .= variables
                       , "primary_key" .= primaryKey
                       , "time_key"    .= timeKey
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
    unless (notDuplicatedIn id identifier modelIdentifiers)
      $ throwError "duplicate model identifiers"
    let
      variables' = map Variable.identifier variables
    unless (noDuplicates variables')
      $ throwError "duplicate variable identifiers"
    unless (primaryKey `elem` variables')
      $ throwError "invalid primary key"
    unless (maybe True (`elem` variables') timeKey)
      $ throwError "invalid time key"
    sequence_
      [
        validateVariable (Variable.identifier variable `delete` variables') variable
      |
        variable <- variables
      ]
